#include "precompiled.hpp"

#include "classfile/altHashing.hpp"
#include "classfile/compactHashtable.inline.hpp"
#include "classfile/javaClasses.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/filemap.hpp"
#include "memory/metaspaceClosure.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "services/diagnosticCommand.hpp"
#include "utilities/hashtable.inline.hpp"

// --------------------------------------------------------------------------
// the number of buckets a thread claims
const int ClaimChunkSize = 32;

SymbolTable* SymbolTable::_the_table = NULL;
// Static arena for symbols that are not deallocated
Arena* SymbolTable::_arena = NULL;
bool SymbolTable::_needs_rehashing = false;
bool SymbolTable::_lookup_shared_first = false;

CompactHashtable<Symbol*, char> SymbolTable::_shared_table;

Symbol* SymbolTable::allocate_symbol(const u1* name, int len, bool c_heap, TRAPS) {

  Symbol* sym;

  if (c_heap) {
    // refcount starts as 1
    sym = new (len, THREAD) Symbol(name, len, 1);
  } else {
    // Allocate to global arena
    sym = new (len, arena(), THREAD) Symbol(name, len, PERM_REFCOUNT);
  }
  return sym;
}

void SymbolTable::initialize_symbols(int arena_alloc_size) {
  // Initialize the arena for global symbols, size passed in depends on CDS.
  if (arena_alloc_size == 0) {
    _arena = new (mtSymbol) Arena(mtSymbol);
  } else {
    _arena = new (mtSymbol) Arena(mtSymbol, arena_alloc_size);
  }
}

// Call function for all symbols in the symbol table.
void SymbolTable::symbols_do(SymbolClosure *cl) {
  // all symbols from shared table
  _shared_table.symbols_do(cl);

  // all symbols from the dynamic table
  const int n = the_table()->table_size();
  for (int i = 0; i < n; i++) {
    for (HashtableEntry<Symbol*, mtSymbol>* p = the_table()->bucket(i); p != NULL; p = p->next()) {
      cl->do_symbol(p->literal_addr());
    }
  }
}

void SymbolTable::metaspace_pointers_do(MetaspaceClosure* it) {
  const int n = the_table()->table_size();
  for (int i = 0; i < n; i++) {
    for (HashtableEntry<Symbol*, mtSymbol>* p = the_table()->bucket(i); p != NULL; p = p->next()) {
      it->push(p->literal_addr());
    }
  }
}

int SymbolTable::_symbols_removed = 0;
int SymbolTable::_symbols_counted = 0;
volatile int SymbolTable::_parallel_claimed_idx = 0;

void SymbolTable::buckets_unlink(int start_idx, int end_idx, BucketUnlinkContext* context) {
  for (int i = start_idx; i < end_idx; ++i) {
    HashtableEntry<Symbol*, mtSymbol>** p = the_table()->bucket_addr(i);
    HashtableEntry<Symbol*, mtSymbol>* entry = the_table()->bucket(i);
    while (entry != NULL) {
      // Shared entries are normally at the end of the bucket and if we run into
      // a shared entry, then there is nothing more to remove. However, if we
      // have rehashed the table, then the shared entries are no longer at the
      // end of the bucket.
      if (entry->is_shared() && !use_alternate_hashcode()) {
        break;
      }
      Symbol* s = entry->literal();
      context->_num_processed++;
      // If reference count is zero, remove.
      if (s->refcount() == 0) {
        delete s;
        *p = entry->next();
        context->free_entry(entry);
      } else {
        p = entry->next_addr();
      }
      // get next entry
      entry = (HashtableEntry<Symbol*, mtSymbol>*)HashtableEntry<Symbol*, mtSymbol>::make_ptr(*p);
    }
  }
}

// Remove unreferenced symbols from the symbol table
// This is done late during GC.
void SymbolTable::unlink(int* processed, int* removed) {
  BucketUnlinkContext context;
  buckets_unlink(0, the_table()->table_size(), &context);
  _the_table->bulk_free_entries(&context);
  *processed = context._num_processed;
  *removed = context._num_removed;

  _symbols_removed = context._num_removed;
  _symbols_counted = context._num_processed;
}

void SymbolTable::possibly_parallel_unlink(int* processed, int* removed) {
  const int limit = the_table()->table_size();

  BucketUnlinkContext context;
  for (;;) {
    // Grab next set of buckets to scan
    int start_idx = Atomic::add(ClaimChunkSize, &_parallel_claimed_idx) - ClaimChunkSize;
    if (start_idx >= limit) {
      // End of table
      break;
    }

    int end_idx = MIN2(limit, start_idx + ClaimChunkSize);
    buckets_unlink(start_idx, end_idx, &context);
  }

  _the_table->bulk_free_entries(&context);
  *processed = context._num_processed;
  *removed = context._num_removed;

  Atomic::add(context._num_processed, &_symbols_counted);
  Atomic::add(context._num_removed, &_symbols_removed);
}

// Create a new table and using alternate hash code, populate the new table
// with the existing strings.   Set flag to use the alternate hash code afterwards.
void SymbolTable::rehash_table() {

  // Create a new symbol table
  SymbolTable* new_table = new SymbolTable();

  the_table()->move_to(new_table);

  // Delete the table and buckets (entries are reused in new table).
  delete _the_table;
  // Don't check if we need rehashing until the table gets unbalanced again.
  // Then rehash with a new global seed.
  _needs_rehashing = false;
  _the_table = new_table;
}

// Lookup a symbol in a bucket.

Symbol* SymbolTable::lookup_dynamic(int index, const char* name, int len, unsigned int hash) {
  int count = 0;
  for (HashtableEntry<Symbol*, mtSymbol>* e = bucket(index); e != NULL; e = e->next()) {
    count++;  // count all entries in this bucket, not just ones with same hash
    if (e->hash() == hash) {
      Symbol* sym = e->literal();
      if (sym->equals(name, len)) {
        // something is referencing this symbol now.
        sym->increment_refcount();
        return sym;
      }
    }
  }
  // If the bucket size is too deep check if this hash code is insufficient.
  if (count >= rehash_count && !needs_rehashing()) {
    _needs_rehashing = check_rehash_table(count);
  }
  return NULL;
}

Symbol* SymbolTable::lookup_shared(const char* name, int len, unsigned int hash) {
  if (use_alternate_hashcode()) {
    // hash_code parameter may use alternate hashing algorithm but the shared table
    // always uses the same original hash code.
    hash = hash_shared_symbol(name, len);
  }
  return _shared_table.lookup(name, hash, len);
}

Symbol* SymbolTable::lookup(int index, const char* name, int len, unsigned int hash) {
  Symbol* sym;
  if (_lookup_shared_first) {
    sym = lookup_shared(name, len, hash);
    if (sym != NULL) {
      return sym;
    }
    _lookup_shared_first = false;
    return lookup_dynamic(index, name, len, hash);
  } else {
    sym = lookup_dynamic(index, name, len, hash);
    if (sym != NULL) {
      return sym;
    }
    sym = lookup_shared(name, len, hash);
    if (sym != NULL) {
      _lookup_shared_first = true;
    }
    return sym;
  }
}

u4 SymbolTable::encode_shared(Symbol* sym) {
  uintx base_address = uintx(MetaspaceShared::shared_rs()->base());
  uintx offset = uintx(sym) - base_address;
  return u4(offset);
}

Symbol* SymbolTable::decode_shared(u4 offset) {
  uintx base_address = _shared_table.base_address();
  Symbol* sym = (Symbol*)(base_address + offset);

  return sym;
}

// Pick hashing algorithm.
unsigned int SymbolTable::hash_symbol(const char* s, int len) {
  return use_alternate_hashcode() ?
           AltHashing::murmur3_32(seed(), (const jbyte*)s, len) :
           java_lang_String::hash_code((const jbyte*)s, len);
}

unsigned int SymbolTable::hash_shared_symbol(const char* s, int len) {
  return java_lang_String::hash_code((const jbyte*)s, len);
}

// We take care not to be blocking while holding the
// SymbolTable_lock. Otherwise, the system might deadlock, since the
// symboltable is used during compilation (VM_thread) The lock free
// synchronization is simplified by the fact that we do not delete
// entries in the symbol table during normal execution (only during
// safepoints).

Symbol* SymbolTable::lookup(const char* name, int len, TRAPS) {
  unsigned int hashValue = hash_symbol(name, len);
  int index = the_table()->hash_to_index(hashValue);

  Symbol* s = the_table()->lookup(index, name, len, hashValue);

  // Found
  if (s != NULL) return s;

  // Grab SymbolTable_lock first.
  MutexLocker ml(SymbolTable_lock, THREAD);

  // Otherwise, add to symbol to table
  return the_table()->basic_add(index, (u1*)name, len, hashValue, true, THREAD);
}

Symbol* SymbolTable::lookup(const Symbol* sym, int begin, int end, TRAPS) {
  char* buffer;
  int index, len;
  unsigned int hashValue;
  char* name;
  {
    name = (char*)sym->base() + begin;
    len = end - begin;
    hashValue = hash_symbol(name, len);
    index = the_table()->hash_to_index(hashValue);
    Symbol* s = the_table()->lookup(index, name, len, hashValue);

    // Found
    if (s != NULL) return s;
  }

  // Otherwise, add to symbol to table. Copy to a C string first.
  char stack_buf[128];
  ResourceMark rm(THREAD);
  if (len <= 128) {
    buffer = stack_buf;
  } else {
    buffer = NEW_RESOURCE_ARRAY_IN_THREAD(THREAD, char, len);
  }
  for (int i=0; i<len; i++) {
    buffer[i] = name[i];
  }
  // Make sure there is no safepoint in the code above since name can't move.
  // We can't include the code in NoSafepointVerifier because of the
  // ResourceMark.

  // Grab SymbolTable_lock first.
  MutexLocker ml(SymbolTable_lock, THREAD);

  return the_table()->basic_add(index, (u1*)buffer, len, hashValue, true, THREAD);
}

Symbol* SymbolTable::lookup_only(const char* name, int len, unsigned int& hash) {
  hash = hash_symbol(name, len);
  int index = the_table()->hash_to_index(hash);

  Symbol* s = the_table()->lookup(index, name, len, hash);
  return s;
}

// Look up the address of the literal in the SymbolTable for this Symbol*
// Do not create any new symbols
// Do not increment the reference count to keep this alive
Symbol** SymbolTable::lookup_symbol_addr(Symbol* sym) {
  unsigned int hash = hash_symbol((char*)sym->bytes(), sym->utf8_length());
  int index = the_table()->hash_to_index(hash);

  for (HashtableEntry<Symbol*, mtSymbol>* e = the_table()->bucket(index); e != NULL; e = e->next()) {
    if (e->hash() == hash) {
      Symbol* literal_sym = e->literal();
      if (sym == literal_sym) {
        return e->literal_addr();
      }
    }
  }
  return NULL;
}

// Suggestion: Push unicode-based lookup all the way into the hashing
// and probing logic, so there is no need for convert_to_utf8 until
// an actual new Symbol* is created.
Symbol* SymbolTable::lookup_unicode(const jchar* name, int utf16_length, TRAPS) {
  int utf8_length = UNICODE::utf8_length((jchar*) name, utf16_length);
  char stack_buf[128];
  if (utf8_length < (int) sizeof(stack_buf)) {
    char* chars = stack_buf;
    UNICODE::convert_to_utf8(name, utf16_length, chars);
    return lookup(chars, utf8_length, THREAD);
  } else {
    ResourceMark rm(THREAD);
    char* chars = NEW_RESOURCE_ARRAY(char, utf8_length + 1);
    UNICODE::convert_to_utf8(name, utf16_length, chars);
    return lookup(chars, utf8_length, THREAD);
  }
}

Symbol* SymbolTable::lookup_only_unicode(const jchar* name, int utf16_length, unsigned int& hash) {
  int utf8_length = UNICODE::utf8_length((jchar*) name, utf16_length);
  char stack_buf[128];
  if (utf8_length < (int) sizeof(stack_buf)) {
    char* chars = stack_buf;
    UNICODE::convert_to_utf8(name, utf16_length, chars);
    return lookup_only(chars, utf8_length, hash);
  } else {
    ResourceMark rm;
    char* chars = NEW_RESOURCE_ARRAY(char, utf8_length + 1);
    UNICODE::convert_to_utf8(name, utf16_length, chars);
    return lookup_only(chars, utf8_length, hash);
  }
}

void SymbolTable::add(ClassLoaderData* loader_data, const constantPoolHandle& cp, int names_count, const char** names, int* lengths, int* cp_indices, unsigned int* hashValues, TRAPS) {
  // Grab SymbolTable_lock first.
  MutexLocker ml(SymbolTable_lock, THREAD);

  SymbolTable* table = the_table();
  bool added = table->basic_add(loader_data, cp, names_count, names, lengths,
                                cp_indices, hashValues, CHECK);
  if (!added) {
    // do it the hard way
    for (int i=0; i<names_count; i++) {
      int index = table->hash_to_index(hashValues[i]);
      bool c_heap = !loader_data->is_the_null_class_loader_data();
      Symbol* sym = table->basic_add(index, (u1*)names[i], lengths[i], hashValues[i], c_heap, CHECK);
      cp->symbol_at_put(cp_indices[i], sym);
    }
  }
}

Symbol* SymbolTable::new_permanent_symbol(const char* name, TRAPS) {
  unsigned int hash;
  Symbol* result = SymbolTable::lookup_only((char*)name, (int)strlen(name), hash);
  if (result != NULL) {
    return result;
  }
  // Grab SymbolTable_lock first.
  MutexLocker ml(SymbolTable_lock, THREAD);

  SymbolTable* table = the_table();
  int index = table->hash_to_index(hash);
  return table->basic_add(index, (u1*)name, (int)strlen(name), hash, false, THREAD);
}

Symbol* SymbolTable::basic_add(int index_arg, u1 *name, int len, unsigned int hashValue_arg, bool c_heap, TRAPS) {

  // Don't allow symbols to be created which cannot fit in a Symbol*.
  if (len > Symbol::max_length()) {
    THROW_MSG_0(vmSymbols::java_lang_InternalError(), "name is too long to represent");
  }

  // Cannot hit a safepoint in this function because the "this" pointer can move.
  NoSafepointVerifier nsv;

  // Check if the symbol table has been rehashed, if so, need to recalculate
  // the hash value and index.
  unsigned int hashValue;
  int index;
  if (use_alternate_hashcode()) {
    hashValue = hash_symbol((const char*)name, len);
    index = hash_to_index(hashValue);
  } else {
    hashValue = hashValue_arg;
    index = index_arg;
  }

  // Since look-up was done lock-free, we need to check if another
  // thread beat us in the race to insert the symbol.
  Symbol* test = lookup(index, (char*)name, len, hashValue);
  if (test != NULL) {
    // A race occurred and another thread introduced the symbol.
    return test;
  }

  // Create a new symbol.
  Symbol* sym = allocate_symbol(name, len, c_heap, CHECK_NULL);

  HashtableEntry<Symbol*, mtSymbol>* entry = new_entry(hashValue, sym);
  add_entry(index, entry);
  return sym;
}

// This version of basic_add adds symbols in batch from the constant pool
// parsing.
bool SymbolTable::basic_add(ClassLoaderData* loader_data, const constantPoolHandle& cp, int names_count, const char** names, int* lengths, int* cp_indices, unsigned int* hashValues, TRAPS) {

  // Check symbol names are not too long.  If any are too long, don't add any.
  for (int i = 0; i< names_count; i++) {
    if (lengths[i] > Symbol::max_length()) {
      THROW_MSG_0(vmSymbols::java_lang_InternalError(), "name is too long to represent");
    }
  }

  // Cannot hit a safepoint in this function because the "this" pointer can move.
  NoSafepointVerifier nsv;

  for (int i=0; i<names_count; i++) {
    // Check if the symbol table has been rehashed, if so, need to recalculate
    // the hash value.
    unsigned int hashValue;
    if (use_alternate_hashcode()) {
      hashValue = hash_symbol(names[i], lengths[i]);
    } else {
      hashValue = hashValues[i];
    }
    // Since look-up was done lock-free, we need to check if another
    // thread beat us in the race to insert the symbol.
    int index = hash_to_index(hashValue);
    Symbol* test = lookup(index, names[i], lengths[i], hashValue);
    if (test != NULL) {
      // A race occurred and another thread introduced the symbol, this one
      // will be dropped and collected. Use test instead.
      cp->symbol_at_put(cp_indices[i], test);
    } else {
      // Create a new symbol.  The null class loader is never unloaded so these
      // are allocated specially in a permanent arena.
      bool c_heap = !loader_data->is_the_null_class_loader_data();
      Symbol* sym = allocate_symbol((const u1*)names[i], lengths[i], c_heap, CHECK_(false));
      HashtableEntry<Symbol*, mtSymbol>* entry = new_entry(hashValue, sym);
      add_entry(index, entry);
      cp->symbol_at_put(cp_indices[i], sym);
    }
  }
  return true;
}

void SymbolTable::verify() {
  for (int i = 0; i < the_table()->table_size(); ++i) {
    HashtableEntry<Symbol*, mtSymbol>* p = the_table()->bucket(i);
    for ( ; p != NULL; p = p->next()) {
      Symbol* s = (Symbol*)(p->literal());
      guarantee(s != NULL, "symbol is NULL");
      unsigned int h = hash_symbol((char*)s->bytes(), s->utf8_length());
      guarantee(p->hash() == h, "broken hash in symbol table entry");
      guarantee(the_table()->hash_to_index(h) == i, "wrong index in symbol table");
    }
  }
}

void SymbolTable::dump(outputStream* st, bool verbose) {
  if (!verbose) {
    the_table()->print_table_statistics(st, "SymbolTable");
  } else {
    st->print_cr("VERSION: 1.0");
    for (int i = 0; i < the_table()->table_size(); ++i) {
      HashtableEntry<Symbol*, mtSymbol>* p = the_table()->bucket(i);
      for ( ; p != NULL; p = p->next()) {
        Symbol* s = (Symbol*)(p->literal());
        const char* utf8_string = (const char*)s->bytes();
        int utf8_length = s->utf8_length();
        st->print("%d %d: ", utf8_length, s->refcount());
        HashtableTextDump::put_utf8(st, utf8_string, utf8_length);
        st->cr();
      }
    }
  }
}

void SymbolTable::write_to_archive() { }

void SymbolTable::serialize(SerializeClosure* soc) { }

// Utility for dumping symbols
SymboltableDCmd::SymboltableDCmd(outputStream* output, bool heap) :
                                 DCmdWithParser(output, heap),
  _verbose("-verbose", "Dump the content of each symbol in the table", "BOOLEAN", false, "false") {
  _dcmdparser.add_dcmd_option(&_verbose);
}

void SymboltableDCmd::execute(DCmdSource source, TRAPS) {
  VM_DumpHashtable dumper(output(), VM_DumpHashtable::DumpSymbols,
                         _verbose.value());
  VMThread::execute(&dumper);
}

int SymboltableDCmd::num_arguments() {
  ResourceMark rm;
  SymboltableDCmd* dcmd = new SymboltableDCmd(NULL, false);
  if (dcmd != NULL) {
    DCmdMark mark(dcmd);
    return dcmd->_dcmdparser.num_arguments();
  } else {
    return 0;
  }
}

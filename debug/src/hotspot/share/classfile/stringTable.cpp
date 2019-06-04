#include "precompiled.hpp"

#include "classfile/altHashing.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "classfile/stringTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/oopStorage.inline.hpp"
#include "gc/shared/oopStorageParState.inline.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/access.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "oops/weakHandle.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/timerTrace.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "utilities/concurrentHashTable.inline.hpp"
#include "utilities/concurrentHashTableTasks.inline.hpp"
#include "utilities/macros.hpp"

// We prefer short chains of avg 2
#define PREF_AVG_LIST_LEN   2
// 2^24 is max size
#define END_SIZE           24
// If a chain gets to 32 something might be wrong
#define REHASH_LEN         32
// If we have as many dead items as 50% of the number of bucket
#define CLEAN_DEAD_HIGH_WATER_MARK 0.5

// --------------------------------------------------------------------------
StringTable* StringTable::_the_table = NULL;
bool StringTable::_alt_hash = false;

static juint murmur_seed = 0;

uintx hash_string(const jchar* s, int len, bool useAlt) {
  return useAlt ? AltHashing::murmur3_32(murmur_seed, s, len) : java_lang_String::hash_code(s, len);
}

class StringTableConfig : public StringTableHash::BaseConfig {
 private:
 public:
  static uintx get_hash(WeakHandle<vm_string_table_data> const& value, bool* is_dead) {
    EXCEPTION_MARK;
    oop val_oop = value.peek();
    if (val_oop == NULL) {
      *is_dead = true;
      return 0;
    }
    *is_dead = false;
    ResourceMark rm(THREAD);
    // All String oops are hashed as unicode
    int length;
    jchar* chars = java_lang_String::as_unicode_string(val_oop, length, THREAD);
    if (chars != NULL) {
      return hash_string(chars, length, StringTable::_alt_hash);
    }
    vm_exit_out_of_memory(length, OOM_MALLOC_ERROR, "get hash from oop");
    return 0;
  }
  // We use default allocation/deallocation but counted
  static void* allocate_node(size_t size, WeakHandle<vm_string_table_data> const& value) {
    StringTable::item_added();
    return StringTableHash::BaseConfig::allocate_node(size, value);
  }
  static void free_node(void* memory, WeakHandle<vm_string_table_data> const& value) {
    value.release();
    StringTableHash::BaseConfig::free_node(memory, value);
    StringTable::item_removed();
  }
};

class StringTableLookupJchar : StackObj {
 private:
  Thread* _thread;
  uintx _hash;
  int _len;
  const jchar* _str;
  Handle _found;

 public:
  StringTableLookupJchar(Thread* thread, uintx hash, const jchar* key, int len)
    : _thread(thread), _hash(hash), _str(key), _len(len) {
  }
  uintx get_hash() const {
    return _hash;
  }
  bool equals(WeakHandle<vm_string_table_data>* value, bool* is_dead) {
    oop val_oop = value->peek();
    if (val_oop == NULL) {
      // dead oop, mark this hash dead for cleaning
      *is_dead = true;
      return false;
    }
    bool equals = java_lang_String::equals(val_oop, (jchar*)_str, _len);
    if (!equals) {
      return false;
    }
    // Need to resolve weak handle and Handleize through possible safepoint.
     _found = Handle(_thread, value->resolve());
    return true;
  }
};

class StringTableLookupOop : public StackObj {
 private:
  Thread* _thread;
  uintx _hash;
  Handle _find;
  Handle _found;  // Might be a different oop with the same value that's already
                  // in the table, which is the point.
 public:
  StringTableLookupOop(Thread* thread, uintx hash, Handle handle)
    : _thread(thread), _hash(hash), _find(handle) { }

  uintx get_hash() const {
    return _hash;
  }

  bool equals(WeakHandle<vm_string_table_data>* value, bool* is_dead) {
    oop val_oop = value->peek();
    if (val_oop == NULL) {
      // dead oop, mark this hash dead for cleaning
      *is_dead = true;
      return false;
    }
    bool equals = java_lang_String::equals(_find(), val_oop);
    if (!equals) {
      return false;
    }
    // Need to resolve weak handle and Handleize through possible safepoint.
    _found = Handle(_thread, value->resolve());
    return true;
  }
};

static size_t ceil_pow_2(uintx val) {
  size_t ret;
  for (ret = 1; ((size_t)1 << ret) < val; ++ret);
  return ret;
}

StringTable::StringTable() : _local_table(NULL), _current_size(0), _has_work(0), _needs_rehashing(false), _weak_handles(NULL), _items(0), _uncleaned_items(0) {
  _weak_handles = new OopStorage("StringTable weak", StringTableWeakAlloc_lock, StringTableWeakActive_lock);
  size_t start_size_log_2 = ceil_pow_2(StringTableSize);
  _current_size = ((size_t)1) << start_size_log_2;
  _local_table = new StringTableHash(start_size_log_2, END_SIZE, REHASH_LEN);
}

size_t StringTable::item_added() {
  return Atomic::add((size_t)1, &(the_table()->_items));
}

size_t StringTable::add_items_to_clean(size_t ndead) {
  size_t total = Atomic::add((size_t)ndead, &(the_table()->_uncleaned_items));
  return total;
}

void StringTable::item_removed() {
  Atomic::add((size_t)-1, &(the_table()->_items));
}

double StringTable::get_load_factor() {
  return (_items*1.0)/_current_size;
}

double StringTable::get_dead_factor() {
  return (_uncleaned_items*1.0)/_current_size;
}

size_t StringTable::table_size(Thread* thread) {
  return ((size_t)(1)) << _local_table->get_size_log2(thread != NULL ? thread
                                                      : Thread::current());
}

void StringTable::trigger_concurrent_work() {
  MutexLockerEx ml(Service_lock, Mutex::_no_safepoint_check_flag);
  the_table()->_has_work = true;
  Service_lock->notify_all();
}

// Probing
oop StringTable::lookup(Symbol* symbol) {
  ResourceMark rm;
  int length;
  jchar* chars = symbol->as_unicode(length);
  return lookup(chars, length);
}

oop StringTable::lookup(jchar* name, int len) {
  unsigned int hash = java_lang_String::hash_code(name, len);
  if (StringTable::_alt_hash) {
    hash = hash_string(name, len, true);
  }
  return StringTable::the_table()->do_lookup(name, len, hash);
}

class StringTableGet : public StackObj {
  Thread* _thread;
  Handle  _return;
 public:
  StringTableGet(Thread* thread) : _thread(thread) { }
  void operator()(WeakHandle<vm_string_table_data>* val) {
    oop result = val->resolve();
    _return = Handle(_thread, result);
  }
  oop get_res_oop() {
    return _return();
  }
};

oop StringTable::do_lookup(jchar* name, int len, uintx hash) {
  Thread* thread = Thread::current();
  StringTableLookupJchar lookup(thread, hash, name, len);
  StringTableGet stg(thread);
  bool rehash_warning;
  _local_table->get(thread, lookup, stg, &rehash_warning);
  if (rehash_warning) {
    _needs_rehashing = true;
  }
  return stg.get_res_oop();
}

// Interning
oop StringTable::intern(Symbol* symbol, TRAPS) {
  if (symbol == NULL) return NULL;
  ResourceMark rm(THREAD);
  int length;
  jchar* chars = symbol->as_unicode(length);
  Handle string;
  oop result = intern(string, chars, length, CHECK_NULL);
  return result;
}

oop StringTable::intern(oop string, TRAPS) {
  if (string == NULL) return NULL;
  ResourceMark rm(THREAD);
  int length;
  Handle h_string (THREAD, string);
  jchar* chars = java_lang_String::as_unicode_string(string, length, CHECK_NULL);
  oop result = intern(h_string, chars, length, CHECK_NULL);
  return result;
}

oop StringTable::intern(const char* utf8_string, TRAPS) {
  if (utf8_string == NULL) return NULL;
  ResourceMark rm(THREAD);
  int length = UTF8::unicode_length(utf8_string);
  jchar* chars = NEW_RESOURCE_ARRAY(jchar, length);
  UTF8::convert_to_unicode(utf8_string, chars, length);
  Handle string;
  oop result = intern(string, chars, length, CHECK_NULL);
  return result;
}

oop StringTable::intern(Handle string_or_null_h, jchar* name, int len, TRAPS) {
  // shared table always uses java_lang_String::hash_code
  unsigned int hash = java_lang_String::hash_code(name, len);
  if (StringTable::_alt_hash) {
    hash = hash_string(name, len, true);
  }
  oop found_string = StringTable::the_table()->do_lookup(name, len, hash);
  if (found_string != NULL) {
    return found_string;
  }
  return StringTable::the_table()->do_intern(string_or_null_h, name, len, hash, CHECK_NULL);
}

class StringTableCreateEntry : public StackObj {
 private:
   Thread* _thread;
   Handle  _return;
   Handle  _store;
 public:
  StringTableCreateEntry(Thread* thread, Handle store)
    : _thread(thread), _store(store) { }

  WeakHandle<vm_string_table_data> operator()() { // No dups found
    WeakHandle<vm_string_table_data> wh = WeakHandle<vm_string_table_data>::create(_store);
    return wh;
  }
  void operator()(bool inserted, WeakHandle<vm_string_table_data>* val) {
    oop result = val->resolve();
    _return = Handle(_thread, result);
  }
  oop get_return() const {
    return _return();
  }
};

oop StringTable::do_intern(Handle string_or_null_h, jchar* name,
                           int len, uintx hash, TRAPS) {
  HandleMark hm(THREAD);  // cleanup strings created
  Handle string_h;

  if (!string_or_null_h.is_null()) {
    string_h = string_or_null_h;
  } else {
    string_h = java_lang_String::create_from_unicode(name, len, CHECK_NULL);
  }

  // Deduplicate the string before it is interned. Note that we should never
  // deduplicate a string after it has been interned. Doing so will counteract
  // compiler optimizations done on e.g. interned string literals.
  Universe::heap()->deduplicate_string(string_h());

  StringTableLookupOop lookup(THREAD, hash, string_h);
  StringTableCreateEntry stc(THREAD, string_h);

  bool rehash_warning;
  _local_table->get_insert_lazy(THREAD, lookup, stc, stc, &rehash_warning);
  if (rehash_warning) {
    _needs_rehashing = true;
  }
  return stc.get_return();
}

// GC support
class StringTableIsAliveCounter : public BoolObjectClosure {
  BoolObjectClosure* _real_boc;
 public:
  size_t _count;
  size_t _count_total;
  StringTableIsAliveCounter(BoolObjectClosure* boc) : _real_boc(boc), _count(0),
                                                      _count_total(0) { }
  bool do_object_b(oop obj) {
    bool ret = _real_boc->do_object_b(obj);
    if (!ret) {
      ++_count;
    }
    ++_count_total;
    return ret;
  }
};

void StringTable::unlink_or_oops_do(BoolObjectClosure* is_alive, OopClosure* f, int* processed, int* removed) {
  DoNothingClosure dnc;
  StringTableIsAliveCounter stiac(is_alive);
  OopClosure* tmp = f != NULL ? f : &dnc;

  StringTable::the_table()->_weak_handles->weak_oops_do(&stiac, tmp);

  // This is the serial case without ParState.
  // Just set the correct number and check for a cleaning phase.
  the_table()->_uncleaned_items = stiac._count;
  StringTable::the_table()->check_concurrent_work();

  if (processed != NULL) {
    *processed = (int) stiac._count_total;
  }
  if (removed != NULL) {
    *removed = (int) stiac._count;
  }
}

void StringTable::oops_do(OopClosure* f) {
  StringTable::the_table()->_weak_handles->oops_do(f);
}

void StringTable::possibly_parallel_unlink(OopStorage::ParState<false, false>* _par_state_string, BoolObjectClosure* cl, int* processed, int* removed)
{
  DoNothingClosure dnc;
  StringTableIsAliveCounter stiac(cl);

  _par_state_string->weak_oops_do(&stiac, &dnc);

  // Accumulate the dead strings.
  the_table()->add_items_to_clean(stiac._count);

  *processed = (int) stiac._count_total;
  *removed = (int) stiac._count;
}

void StringTable::possibly_parallel_oops_do(OopStorage::ParState<false /* concurrent */, false /* const */>* _par_state_string, OopClosure* f)
{
  _par_state_string->oops_do(f);
}

// Concurrent work
void StringTable::grow(JavaThread* jt) {
  StringTableHash::GrowTask gt(_local_table);
  if (!gt.prepare(jt)) {
    return;
  }
  {
    while (gt.do_task(jt)) {
      gt.pause(jt);
      {
        ThreadBlockInVM tbivm(jt);
      }
      gt.cont(jt);
    }
  }
  gt.done(jt);
  _current_size = table_size(jt);
}

struct StringTableDoDelete : StackObj {
  void operator()(WeakHandle<vm_string_table_data>* val) {
    /* do nothing */
  }
};

struct StringTableDeleteCheck : StackObj {
  long _count;
  long _item;
  StringTableDeleteCheck() : _count(0), _item(0) { }
  bool operator()(WeakHandle<vm_string_table_data>* val) {
    ++_item;
    oop tmp = val->peek();
    if (tmp == NULL) {
      ++_count;
      return true;
    } else {
      return false;
    }
  }
};

void StringTable::clean_dead_entries(JavaThread* jt) {
  StringTableHash::BulkDeleteTask bdt(_local_table);
  if (!bdt.prepare(jt)) {
    return;
  }

  StringTableDeleteCheck stdc;
  StringTableDoDelete stdd;
  {
    while (bdt.do_task(jt, stdc, stdd)) {
      bdt.pause(jt);
      {
        ThreadBlockInVM tbivm(jt);
      }
      bdt.cont(jt);
    }
    bdt.done(jt);
  }
}

void StringTable::check_concurrent_work() {
  if (_has_work) {
    return;
  }

  double load_factor = StringTable::get_load_factor();
  double dead_factor = StringTable::get_dead_factor();
  // We should clean/resize if we have more dead than alive,
  // more items than preferred load factor or
  // more dead items than water mark.
  if ((dead_factor > load_factor) || (load_factor > PREF_AVG_LIST_LEN) || (dead_factor > CLEAN_DEAD_HIGH_WATER_MARK)) {
    trigger_concurrent_work();
  }
}

void StringTable::concurrent_work(JavaThread* jt) {
  _has_work = false;
  double load_factor = get_load_factor();
  // We prefer growing, since that also removes dead items
  if (load_factor > PREF_AVG_LIST_LEN && !_local_table->is_max_size_reached()) {
    grow(jt);
  } else {
    clean_dead_entries(jt);
  }
}

void StringTable::do_concurrent_work(JavaThread* jt) {
  StringTable::the_table()->concurrent_work(jt);
}

// Rehash
bool StringTable::do_rehash() {
  if (!_local_table->is_safepoint_safe()) {
    return false;
  }

  // We use max size
  StringTableHash* new_table = new StringTableHash(END_SIZE, END_SIZE, REHASH_LEN);
  // Use alt hash from now on
  _alt_hash = true;
  if (!_local_table->try_move_nodes_to(Thread::current(), new_table)) {
    _alt_hash = false;
    delete new_table;
    return false;
  }

  // free old table
  delete _local_table;
  _local_table = new_table;

  return true;
}

void StringTable::try_rehash_table() {
  static bool rehashed = false;

  // Grow instead of rehash.
  if (get_load_factor() > PREF_AVG_LIST_LEN && !_local_table->is_max_size_reached()) {
    trigger_concurrent_work();
    _needs_rehashing = false;
    return;
  }
  // Already rehashed.
  if (rehashed) {
    trigger_concurrent_work();
    _needs_rehashing = false;
    return;
  }

  murmur_seed = AltHashing::compute_seed();
  {
    if (do_rehash()) {
      rehashed = true;
    }
  }
  _needs_rehashing = false;
}

void StringTable::rehash_table() {
  StringTable::the_table()->try_rehash_table();
}

// Statistics
static int literal_size(oop obj) {
  // NOTE: this would over-count if (pre-JDK8)
  // java_lang_Class::has_offset_field() is true and the String.value array is
  // shared by several Strings. However, starting from JDK8, the String.value
  // array is not shared anymore.
  if (obj == NULL) {
    return 0;
  } else if (obj->klass() == SystemDictionary::String_klass()) {
    return (obj->size() + java_lang_String::value(obj)->size()) * HeapWordSize;
  } else {
    return obj->size();
  }
}

struct SizeFunc : StackObj {
  size_t operator()(WeakHandle<vm_string_table_data>* val) {
    oop s = val->peek();
    if (s == NULL) {
      // Dead
      return 0;
    }
    return literal_size(s);
  };
};

void StringTable::print_table_statistics(outputStream* st, const char* table_name) {
  SizeFunc sz;
  _local_table->statistics_to(Thread::current(), sz, st, table_name);
}

// Dumping
class PrintString : StackObj {
  Thread* _thr;
  outputStream* _st;
 public:
  PrintString(Thread* thr, outputStream* st) : _thr(thr), _st(st) { }
  bool operator()(WeakHandle<vm_string_table_data>* val) {
    oop s = val->peek();
    if (s == NULL) {
      return true;
    }
    typeArrayOop value     = java_lang_String::value_no_keepalive(s);
    int          length    = java_lang_String::length(s);
    bool         is_latin1 = java_lang_String::is_latin1(s);

    if (length <= 0) {
      _st->print("%d: ", length);
    } else {
      ResourceMark rm(_thr);
      int utf8_length = length;
      char* utf8_string;

      if (!is_latin1) {
        jchar* chars = value->char_at_addr(0);
        utf8_string = UNICODE::as_utf8(chars, utf8_length);
      } else {
        jbyte* bytes = value->byte_at_addr(0);
        utf8_string = UNICODE::as_utf8(bytes, utf8_length);
      }

      _st->print("%d: ", utf8_length);
    }
    _st->cr();
    return true;
  };
};

void StringTable::dump(outputStream* st, bool verbose) {
  if (!verbose) {
    the_table()->print_table_statistics(st, "StringTable");
  } else {
    Thread* thr = Thread::current();
    ResourceMark rm(thr);
    st->print_cr("VERSION: 1.1");
    PrintString ps(thr, st);
    if (!the_table()->_local_table->try_scan(thr, ps)) {
      st->print_cr("dump unavailable at this moment");
    }
  }
}

#ifndef SHARE_VM_CLASSFILE_STRINGTABLE_HPP
#define SHARE_VM_CLASSFILE_STRINGTABLE_HPP

#include "gc/shared/oopStorage.hpp"
#include "gc/shared/oopStorageParState.hpp"
#include "memory/allocation.hpp"
#include "memory/padded.hpp"
#include "oops/oop.hpp"
#include "oops/weakHandle.hpp"
#include "utilities/concurrentHashTable.hpp"

template <class T, class N> class CompactHashtable;
class CompactStringTableWriter;
class SerializeClosure;

class StringTable;
class StringTableConfig;
typedef ConcurrentHashTable<WeakHandle<vm_string_table_data>,
                            StringTableConfig, mtSymbol> StringTableHash;

class StringTableCreateEntry;

class StringTable : public CHeapObj<mtSymbol>{
  friend class VMStructs;
  friend class Symbol;
  friend class StringTableConfig;
  friend class StringTableCreateEntry;

private:
  void grow(JavaThread* jt);
  void clean_dead_entries(JavaThread* jt);

  // The string table
  static StringTable* _the_table;
  // Shared string table
  static CompactHashtable<oop, char> _shared_table;
  static bool _shared_string_mapped;
  static bool _alt_hash;
private:

   // Set if one bucket is out of balance due to hash algorithm deficiency
  StringTableHash* _local_table;
  size_t _current_size;
  volatile bool _has_work;
  volatile bool _needs_rehashing;

  OopStorage* _weak_handles;

  volatile size_t _items;
  DEFINE_PAD_MINUS_SIZE(1, DEFAULT_CACHE_LINE_SIZE, sizeof(volatile size_t));
  volatile size_t _uncleaned_items;
  DEFINE_PAD_MINUS_SIZE(2, DEFAULT_CACHE_LINE_SIZE, sizeof(volatile size_t));

  double get_load_factor();
  double get_dead_factor();

  void check_concurrent_work();
  void trigger_concurrent_work();

  static size_t item_added();
  static void item_removed();
  size_t add_items_to_clean(size_t ndead);

  StringTable();

  static oop intern(Handle string_or_null_h, jchar* name, int len, TRAPS);
  oop do_intern(Handle string_or_null, jchar* name, int len, uintx hash, TRAPS);
  oop do_lookup(jchar* name, int len, uintx hash);

  void concurrent_work(JavaThread* jt);
  void print_table_statistics(outputStream* st, const char* table_name);

  void try_rehash_table();
  bool do_rehash();

 public:
  // The string table
  static StringTable* the_table() { return _the_table; }
  size_t table_size(Thread* thread = NULL);

  static OopStorage* weak_storage() { return the_table()->_weak_handles; }

  static void create_table() {
    _the_table = new StringTable();
  }

  static void do_concurrent_work(JavaThread* jt);
  static bool has_work() { return the_table()->_has_work; }

  // GC support

  // Must be called before a parallel walk where strings might die.
  static void reset_dead_counter() {
    the_table()->_uncleaned_items = 0;
  }
  // After the parallel walk this method must be called to trigger
  // cleaning. Note it might trigger a resize instead.
  static void finish_dead_counter() {
    the_table()->check_concurrent_work();
  }

  // If GC uses ParState directly it should add the number of cleared
  // strings to this method.
  static void inc_dead_counter(size_t ndead) {
    the_table()->add_items_to_clean(ndead);
  }

  //   Delete pointers to otherwise-unreachable objects.
  static void unlink(BoolObjectClosure* cl) {
    unlink_or_oops_do(cl);
  }
  static void unlink_or_oops_do(BoolObjectClosure* is_alive, OopClosure* f = NULL, int* processed = NULL, int* removed = NULL);

  // Serially invoke "f->do_oop" on the locations of all oops in the table.
  static void oops_do(OopClosure* f);

  // Possibly parallel versions of the above
  static void possibly_parallel_unlink(OopStorage::ParState<false /* concurrent */, false /* const*/>* par_state_string, BoolObjectClosure* cl, int* processed, int* removed);
  static void possibly_parallel_oops_do(OopStorage::ParState<false /* concurrent */, false /* const*/>* par_state_string, OopClosure* f);

  // Probing
  static oop lookup(Symbol* symbol);
  static oop lookup(jchar* chars, int length);

  // Interning
  static oop intern(Symbol* symbol, TRAPS);
  static oop intern(oop string, TRAPS);
  static oop intern(const char *utf8_string, TRAPS);

  // Rehash the string table if it gets out of balance
  static void rehash_table();
  static bool needs_rehashing()
    { return StringTable::the_table()->_needs_rehashing; }

  // Sharing
 private:
  oop lookup_shared(jchar* name, int len, unsigned int hash) { return NULL; };
  static void copy_shared_string_table(CompactStringTableWriter* ch_table) { };
 public:
  static oop create_archived_string(oop s, Thread* THREAD) { return NULL; };
  static void set_shared_string_mapped() { _shared_string_mapped = true; }
  static bool shared_string_mapped()     { return _shared_string_mapped; }
  static void shared_oops_do(OopClosure* f) { };
  static void write_to_archive() { };
  static void serialize(SerializeClosure* soc) { };

  // Jcmd
  static void dump(outputStream* st, bool verbose=false);
  // Debugging
  static size_t verify_and_compare_entries();
  static void verify();
};

#endif

#ifndef SHARE_VM_PRIMS_RESOLVEDMETHOD_HPP
#define SHARE_VM_PRIMS_RESOLVEDMETHOD_HPP

#include "oops/symbol.hpp"
#include "oops/weakHandle.hpp"
#include "utilities/hashtable.hpp"

// Hashtable to record Method* used in ResolvedMethods, via. ResolvedMethod oops.
// This is needed for redefinition to replace Method* with redefined versions.

// Entry in a ResolvedMethodTable, mapping a ClassLoaderWeakHandle for a single oop of
// java_lang_invoke_ResolvedMethodName which holds JVM Method* in vmtarget.

class ResolvedMethodEntry : public HashtableEntry<ClassLoaderWeakHandle, mtClass> {
 public:
  ResolvedMethodEntry* next() const {
    return (ResolvedMethodEntry*)HashtableEntry<ClassLoaderWeakHandle, mtClass>::next();
  }

  ResolvedMethodEntry** next_addr() {
    return (ResolvedMethodEntry**)HashtableEntry<ClassLoaderWeakHandle, mtClass>::next_addr();
  }

  oop object();
  oop object_no_keepalive();

  void print_on(outputStream* st) const;
};

class ResolvedMethodTable : public Hashtable<ClassLoaderWeakHandle, mtClass> {
  enum Constants {
    _table_size  = 1007
  };

  static int _oops_removed;
  static int _oops_counted;

  static ResolvedMethodTable* _the_table;
private:
  ResolvedMethodEntry* bucket(int i) {
    return (ResolvedMethodEntry*) Hashtable<ClassLoaderWeakHandle, mtClass>::bucket(i);
  }

  ResolvedMethodEntry** bucket_addr(int i) {
    return (ResolvedMethodEntry**) Hashtable<ClassLoaderWeakHandle, mtClass>::bucket_addr(i);
  }

  unsigned int compute_hash(Method* method);

  // need not be locked; no state change
  oop lookup(int index, unsigned int hash, Method* method);
  oop lookup(Method* method);

  // must be done under ResolvedMethodTable_lock
  oop basic_add(Method* method, Handle rmethod_name);

public:
  ResolvedMethodTable();

  static void create_table() {
    _the_table = new ResolvedMethodTable();
  }

  // Called from java_lang_invoke_ResolvedMethodName
  static oop find_method(Method* method);
  static oop add_method(Handle rmethod_name);

  // Cleanup cleared entries
  static void unlink();

  void verify();
};

#endif

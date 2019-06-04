#ifndef SHARE_VM_OOPS_KLASSVTABLE_HPP
#define SHARE_VM_OOPS_KLASSVTABLE_HPP

#include "oops/oopsHierarchy.hpp"
#include "runtime/handles.hpp"
#include "utilities/growableArray.hpp"

// A klassVtable abstracts the variable-length vtable that is embedded in InstanceKlass
// and ArrayKlass.  klassVtable objects are used just as convenient transient accessors to the vtable,
// not to actually hold the vtable data.
// Note: the klassVtable should not be accessed before the class has been verified
// (until that point, the vtable is uninitialized).

// Currently a klassVtable contains a direct reference to the vtable data, and is therefore
// not preserved across GCs.

class vtableEntry;

class klassVtable {
  Klass*       _klass;            // my klass
  int          _tableOffset;      // offset of start of vtable data within klass
  int          _length;           // length of vtable (number of entries)

  // Ordering important, so greater_than (>) can be used as an merge operator.
  enum AccessType {
    acc_private         = 0,
    acc_package_private = 1,
    acc_publicprotected = 2
  };

 public:
  klassVtable(Klass* klass, void* base, int length) : _klass(klass) {
    _tableOffset = (address)base - (address)klass; _length = length;
  }

  // accessors
  vtableEntry* table() const      { return (vtableEntry*)(address(_klass) + _tableOffset); }
  Klass* klass() const            { return _klass; }
  int length() const              { return _length; }
  inline Method* method_at(int i) const;
  inline Method* unchecked_method_at(int i) const;
  inline Method** adr_method_at(int i) const;

  // searching; all methods return -1 if not found
  int index_of(Method* m) const                         { return index_of(m, _length); }
  int index_of_miranda(Symbol* name, Symbol* signature);

  void initialize_vtable(bool checkconstraints, TRAPS);   // initialize vtable of a new klass

  // CDS/RedefineClasses support - clear vtables so they can be reinitialized
  // at dump time.  Clearing gives us an easy way to tell if the vtable has
  // already been reinitialized at dump time (see dump.cpp).  Vtables can
  // be initialized at run time by RedefineClasses so dumping the right order
  // is necessary.
  void clear_vtable();
  bool is_initialized();

  // computes vtable length (in words) and the number of miranda methods
  static void compute_vtable_size_and_num_mirandas(int* vtable_length,
                                                   int* num_new_mirandas,
                                                   GrowableArray<Method*>* all_mirandas,
                                                   const Klass* super,
                                                   Array<Method*>* methods,
                                                   AccessFlags class_flags,
                                                   u2 major_version,
                                                   Handle classloader,
                                                   Symbol* classname,
                                                   Array<Klass*>* local_interfaces,
                                                   TRAPS);

  // Debugging code
  void print() { };

 protected:
  friend class vtableEntry;

 public:
  // Transitive overridng rules for class files < JDK1_7 use the older JVMS rules.
  // Overriding is determined as we create the vtable, so we use the class file version
  // of the class whose vtable we are calculating.
  enum { VTABLE_TRANSITIVE_OVERRIDE_VERSION = 51 };

 private:
  void copy_vtable_to(vtableEntry* start);
  int  initialize_from_super(Klass* super);
  int  index_of(Method* m, int len) const; // same as index_of, but search only up to len
  void put_method_at(Method* m, int index);
  static bool needs_new_vtable_entry(const methodHandle& m, const Klass* super, Handle classloader, Symbol* classname, AccessFlags access_flags, u2 major_version, TRAPS);

  bool update_inherited_vtable(InstanceKlass* klass, const methodHandle& target_method, int super_vtable_len, int default_index, bool checkconstraints, TRAPS);
 InstanceKlass* find_transitive_override(InstanceKlass* initialsuper, const methodHandle& target_method, int vtable_index,
                                         Handle target_loader, Symbol* target_classname, Thread* THREAD);

  // support for miranda methods
  bool is_miranda_entry_at(int i);
  int fill_in_mirandas(int initialized);
  static bool is_miranda(Method* m, Array<Method*>* class_methods, Array<Method*>* default_methods, const Klass* super, bool is_interface);
  static void add_new_mirandas_to_lists(
      GrowableArray<Method*>* new_mirandas,
      GrowableArray<Method*>* all_mirandas,
      Array<Method*>* current_interface_methods,
      Array<Method*>* class_methods,
      Array<Method*>* default_methods,
      const Klass* super,
      bool is_interface);
  static void get_mirandas(
      GrowableArray<Method*>* new_mirandas,
      GrowableArray<Method*>* all_mirandas,
      const Klass* super,
      Array<Method*>* class_methods,
      Array<Method*>* default_methods,
      Array<Klass*>* local_interfaces,
      bool is_interface);
  void verify_against(outputStream* st, klassVtable* vt, int index);
  inline InstanceKlass* ik() const;
};

// private helper class for klassVtable
// description of entry points:
//    destination is interpreted:
//      from_compiled_code_entry_point -> c2iadapter
//      from_interpreter_entry_point   -> interpreter entry point
//    destination is compiled:
//      from_compiled_code_entry_point -> nmethod entry point
//      from_interpreter_entry_point   -> i2cadapter
class vtableEntry {
  friend class VMStructs;
  friend class JVMCIVMStructs;

 public:
  // size in words
  static int size()          { return sizeof(vtableEntry) / wordSize; }
  static int size_in_bytes() { return sizeof(vtableEntry); }

  static int method_offset_in_bytes() { return offset_of(vtableEntry, _method); }
  Method* method() const     { return _method; }
  Method** method_addr()     { return &_method; }

 private:
  Method* _method;
  void set(Method* method)   { _method = method; }
  void clear()               { _method = NULL; }
  void print()               { };

  friend class klassVtable;
};

inline Method* klassVtable::method_at(int i) const {
  return table()[i].method();
}

inline Method* klassVtable::unchecked_method_at(int i) const {
  return table()[i].method();
}

inline Method** klassVtable::adr_method_at(int i) const {
  // Allow one past the last entry to be referenced; useful for loop bounds.
  return (Method**)(address(table() + i) + vtableEntry::method_offset_in_bytes());
}

// --------------------------------------------------------------------------------
class klassItable;
class itableMethodEntry;

class itableOffsetEntry {
 private:
  Klass* _interface;
  int      _offset;
 public:
  Klass* interface_klass() const { return _interface; }
  Klass**interface_klass_addr()  { return &_interface; }
  int      offset() const          { return _offset; }

  static itableMethodEntry* method_entry(Klass* k, int offset) { return (itableMethodEntry*)(((address)k) + offset); }
  itableMethodEntry* first_method_entry(Klass* k)              { return method_entry(k, _offset); }

  void initialize(Klass* interf, int offset) { _interface = interf; _offset = offset; }

  // Static size and offset accessors
  static int size()                       { return sizeof(itableOffsetEntry) / wordSize; }    // size in words
  static int interface_offset_in_bytes()  { return offset_of(itableOffsetEntry, _interface); }
  static int offset_offset_in_bytes()     { return offset_of(itableOffsetEntry, _offset); }

  friend class klassItable;
};

class itableMethodEntry {
 private:
  Method* _method;

 public:
  Method* method() const { return _method; }
  Method**method_addr() { return &_method; }

  void clear()             { _method = NULL; }

  void initialize(Method* method);

  // Static size and offset accessors
  static int size()                         { return sizeof(itableMethodEntry) / wordSize; }  // size in words
  static int method_offset_in_bytes()       { return offset_of(itableMethodEntry, _method); }

  friend class klassItable;
};

//
// Format of an itable
//
//    ---- offset table ---
//    Klass* of interface 1 \
//    offset to vtable from start of oop  / offset table entry
//    ...
//    Klass* of interface n \
//    offset to vtable from start of oop  / offset table entry
//    --- vtable for interface 1 ---
//    Method* \
//    compiler entry point                / method table entry
//    ...
//    Method* \
//    compiler entry point                / method table entry
//    -- vtable for interface 2 ---
//    ...
//
class klassItable {
 private:
  InstanceKlass*       _klass;             // my klass
  int                  _table_offset;      // offset of start of itable data within klass (in words)
  int                  _size_offset_table; // size of offset table (in itableOffset entries)
  int                  _size_method_table; // size of methodtable (in itableMethodEntry entries)

  void initialize_itable_for_interface(int method_table_offset, Klass* interf_h, bool checkconstraints, TRAPS);
 public:
  klassItable(InstanceKlass* klass);

  itableOffsetEntry* offset_entry(int i) { return &((itableOffsetEntry*)vtable_start())[i]; }

  itableMethodEntry* method_entry(int i) { return &((itableMethodEntry*)method_start())[i]; }

  int size_offset_table()                { return _size_offset_table; }

  // Initialization
  void initialize_itable(bool checkconstraints, TRAPS);

  // Setup of itable
  static int assign_itable_indices_for_interface(Klass* klass);
  static int method_count_for_interface(Klass* klass);
  static int compute_itable_size(Array<Klass*>* transitive_interfaces);
  static void setup_itable_offset_table(InstanceKlass* klass);

  // Resolving of method to index
  static Method* method_for_itable_index(Klass* klass, int itable_index);

 private:
  intptr_t* vtable_start() const { return ((intptr_t*)_klass) + _table_offset; }
  intptr_t* method_start() const { return vtable_start() + _size_offset_table * itableOffsetEntry::size(); }

  // Helper methods
  static int  calc_itable_size(int num_interfaces, int num_methods) { return (num_interfaces * itableOffsetEntry::size()) + (num_methods * itableMethodEntry::size()); }

  static void update_stats(int size) { }
};

#endif

#ifndef SHARE_VM_CLASSFILE_MODULEENTRY_HPP
#define SHARE_VM_CLASSFILE_MODULEENTRY_HPP

#include "jni.h"
#include "classfile/classLoaderData.hpp"
#include "classfile/vmSymbols.hpp"
#include "oops/oopHandle.hpp"
#include "oops/symbol.hpp"
#include "runtime/jniHandles.hpp"
#include "runtime/mutexLocker.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/hashtable.hpp"
#include "utilities/macros.hpp"
#include "utilities/ostream.hpp"

#define UNNAMED_MODULE "unnamed module"
#define UNNAMED_MODULE_LEN 14
#define JAVAPKG "java"
#define JAVAPKG_LEN 4
#define JAVA_BASE_NAME "java.base"
#define JAVA_BASE_NAME_LEN 9

class ModuleClosure;

// A ModuleEntry describes a module that has been defined by a call to JVM_DefineModule.
// It contains:
//   - Symbol* containing the module's name.
//   - pointer to the java.lang.Module for this module.
//   - pointer to the java.security.ProtectionDomain shared by classes defined to this module.
//   - ClassLoaderData*, class loader of this module.
//   - a growable array containg other module entries that this module can read.
//   - a flag indicating if this module can read all unnamed modules.
//
// The Mutex Module_lock is shared between ModuleEntry and PackageEntry, to lock either
// data structure.
class ModuleEntry : public HashtableEntry<Symbol*, mtModule> {
private:
  OopHandle _module;                   // java.lang.Module
  OopHandle _pd;                       // java.security.ProtectionDomain, cached
                                       // for shared classes from this module
  ClassLoaderData* _loader_data;
  GrowableArray<ModuleEntry*>* _reads; // list of modules that are readable by this module
  Symbol* _version;                    // module version number
  Symbol* _location;                   // module location
  bool _can_read_all_unnamed;
  bool _must_walk_reads;               // walk module's reads list at GC safepoints to purge out dead modules
  bool _is_open;                       // whether the packages in the module are all unqualifiedly exported
  bool _is_patched;                    // whether the module is patched via --patch-module
  enum { MODULE_READS_SIZE = 101 };      // Initial size of list of modules that the module can read.

public:
  void init() {
    _module = NULL;
    _loader_data = NULL;
    _pd = NULL;
    _reads = NULL;
    _version = NULL;
    _location = NULL;
    _can_read_all_unnamed = false;
    _must_walk_reads = false;
    _is_patched = false;
    _is_open = false;
  }

  Symbol*          name() const                        { return literal(); }
  void             set_name(Symbol* n)                 { set_literal(n); }

  oop              module() const;
  OopHandle        module_handle() const               { return _module; }
  void             set_module(OopHandle j)             { _module = j; }

  // The shared ProtectionDomain reference is set once the VM loads a shared class
  // originated from the current Module. The referenced ProtectionDomain object is
  // created by the ClassLoader when loading a class (shared or non-shared) from the
  // Module for the first time. This ProtectionDomain object is used for all
  // classes from the Module loaded by the same ClassLoader.
  oop              shared_protection_domain();
  void             set_shared_protection_domain(ClassLoaderData *loader_data, Handle pd);

  ClassLoaderData* loader_data() const                 { return _loader_data; }

  void set_loader_data(ClassLoaderData* cld) {
    _loader_data = cld;
  }

  Symbol*          version() const                     { return _version; }
  void             set_version(Symbol* version);

  Symbol*          location() const                    { return _location; }
  void             set_location(Symbol* location);
  bool             should_show_version();

  bool             can_read(ModuleEntry* m) const;
  bool             has_reads_list() const;
  void             add_read(ModuleEntry* m);
  void             set_read_walk_required(ClassLoaderData* m_loader_data);

  bool             is_open() const                     { return _is_open; }
  void             set_is_open(bool is_open);

  bool             is_named() const                    { return (name() != NULL); }

  bool can_read_all_unnamed() const {
    return _can_read_all_unnamed;
  }

  // Modules can only go from strict to loose.
  void set_can_read_all_unnamed() { _can_read_all_unnamed = true; }

  void set_is_patched() {
      _is_patched = true;
  }
  bool is_patched() {
      return _is_patched;
  }

  ModuleEntry* next() const {
    return (ModuleEntry*)HashtableEntry<Symbol*, mtModule>::next();
  }
  ModuleEntry** next_addr() {
    return (ModuleEntry**)HashtableEntry<Symbol*, mtModule>::next_addr();
  }

  // iteration support for readability
  void module_reads_do(ModuleClosure* const f);

  // Purge dead weak references out of reads list when any given class loader is unloaded.
  void purge_reads();
  void delete_reads();

  // Special handling for unnamed module, one per class loader
  static ModuleEntry* create_unnamed_module(ClassLoaderData* cld);
  static ModuleEntry* create_boot_unnamed_module(ClassLoaderData* cld);
  static ModuleEntry* new_unnamed_module_entry(Handle module_handle, ClassLoaderData* cld);
  void delete_unnamed_module();

  void print(outputStream* st = tty);
};

// Iterator interface
class ModuleClosure: public StackObj {
 public:
  virtual void do_module(ModuleEntry* module) = 0;
};

// The ModuleEntryTable is a Hashtable containing a list of all modules defined
// by a particular class loader.  Each module is represented as a ModuleEntry node.
//
// Each ModuleEntryTable contains a _javabase_module field which allows for the
// creation of java.base's ModuleEntry very early in bootstrapping before the
// corresponding JVM_DefineModule call for java.base occurs during module system
// initialization.  Setting up java.base's ModuleEntry early enables classes,
// loaded prior to the module system being initialized to be created with their
// PackageEntry node's correctly pointing at java.base's ModuleEntry.  No class
// outside of java.base is allowed to be loaded pre-module system initialization.
//
// The ModuleEntryTable's lookup is lock free.
//
class ModuleEntryTable : public Hashtable<Symbol*, mtModule> {
  friend class VMStructs;
public:
  enum Constants {
    _moduletable_entry_size  = 109 // number of entries in module entry table
  };

private:
  static ModuleEntry* _javabase_module;

  ModuleEntry* new_entry(unsigned int hash, Handle module_handle, bool is_open,
                         Symbol* name, Symbol* version, Symbol* location, ClassLoaderData* loader_data);
  void add_entry(int index, ModuleEntry* new_entry);

  int entry_size() const { return BasicHashtable<mtModule>::entry_size(); }

  ModuleEntry** bucket_addr(int i) {
    return (ModuleEntry**)Hashtable<Symbol*, mtModule>::bucket_addr(i);
  }

  static unsigned int compute_hash(Symbol* name) { return ((name == NULL) ? 0 : (unsigned int)(name->identity_hash())); }
  int index_for(Symbol* name) const              { return hash_to_index(compute_hash(name)); }

public:
  ModuleEntryTable(int table_size);
  ~ModuleEntryTable();

  ModuleEntry* bucket(int i) {
    return (ModuleEntry*)Hashtable<Symbol*, mtModule>::bucket(i);
  }

  // Create module in loader's module entry table, if already exists then
  // return null.  Assume Module_lock has been locked by caller.
  ModuleEntry* locked_create_entry_or_null(Handle module_handle,
                                           bool is_open,
                                           Symbol* module_name,
                                           Symbol* module_version,
                                           Symbol* module_location,
                                           ClassLoaderData* loader_data);

  // Only lookup module within loader's module entry table.  The table read is lock-free.
  ModuleEntry* lookup_only(Symbol* name);

  // purge dead weak references out of reads list
  void purge_all_module_reads();

  // Special handling for java.base
  static ModuleEntry* javabase_moduleEntry()                   { return _javabase_module; }
  static void set_javabase_moduleEntry(ModuleEntry* java_base) { _javabase_module = java_base; }

  static bool javabase_defined() { return ((_javabase_module != NULL) && (_javabase_module->module() != NULL)); }
  static void finalize_javabase(Handle module_handle, Symbol* version, Symbol* location);
  static void patch_javabase_entries(Handle module_handle);

  void print(outputStream* st = tty);
};

#endif

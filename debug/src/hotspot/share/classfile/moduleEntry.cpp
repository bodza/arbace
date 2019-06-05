#include "precompiled.hpp"

#include "jni.h"
#include "classfile/classLoaderData.inline.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "classfile/moduleEntry.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oopHandle.inline.hpp"
#include "oops/symbol.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/safepoint.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/hashtable.inline.hpp"
#include "utilities/ostream.hpp"

ModuleEntry* ModuleEntryTable::_javabase_module = NULL;

oop ModuleEntry::module() const { return _module.resolve(); }

void ModuleEntry::set_location(Symbol* location) {
  if (_location != NULL) {
    // _location symbol's refcounts are managed by ModuleEntry,
    // must decrement the old one before updating.
    _location->decrement_refcount();
  }

  _location = location;

  if (location != NULL) {
    location->increment_refcount();
  }
}

// Return true if the module's version should be displayed in error messages,
// logging, etc.
// Return false if the module's version is null, if it is unnamed, or if the
// module is not an upgradeable module.
// Detect if the module is not upgradeable by checking:
//     1. Module location is "jrt:/java." and its loader is boot or platform
//     2. Module location is "jrt:/jdk.", its loader is one of the builtin loaders
//        and its version is the same as module java.base's version
// The above check is imprecise but should work in almost all cases.
bool ModuleEntry::should_show_version() {
  if (version() == NULL || !is_named()) return false;

  if (location() != NULL) {
    ResourceMark rm;
    const char* loc = location()->as_C_string();
    ClassLoaderData* cld = loader_data();

    if ((cld->is_the_null_class_loader_data() || cld->is_platform_class_loader_data()) && (strncmp(loc, "jrt:/java.", 10) == 0)) {
      return false;
    }
    if ((ModuleEntryTable::javabase_moduleEntry()->version()->fast_compare(version()) == 0) && cld->is_permanent_class_loader_data() && (strncmp(loc, "jrt:/jdk.", 9) == 0)) {
      return false;
    }
  }
  return true;
}

void ModuleEntry::set_version(Symbol* version) {
  if (_version != NULL) {
    // _version symbol's refcounts are managed by ModuleEntry,
    // must decrement the old one before updating.
    _version->decrement_refcount();
  }

  _version = version;

  if (version != NULL) {
    version->increment_refcount();
  }
}

// Returns the shared ProtectionDomain
oop ModuleEntry::shared_protection_domain() {
  return _pd.resolve();
}

// Set the shared ProtectionDomain atomically
void ModuleEntry::set_shared_protection_domain(ClassLoaderData *loader_data, Handle pd_h) {
  // Create a handle for the shared ProtectionDomain and save it atomically.
  // init_handle_locked checks if someone beats us setting the _pd cache.
  loader_data->init_handle_locked(_pd, pd_h);
}

// Returns true if this module can read module m
bool ModuleEntry::can_read(ModuleEntry* m) const {
  // Unnamed modules read everyone and all modules
  // read java.base.  If either of these conditions
  // hold, readability has been established.
  if (!this->is_named() || (m == ModuleEntryTable::javabase_moduleEntry())) {
    return true;
  }

  MutexLocker m1(Module_lock);

  if (!has_reads_list()) {
    return false;
  } else {
    return _reads->contains(m);
  }
}

// Add a new module to this module's reads list
void ModuleEntry::add_read(ModuleEntry* m) {
  // Unnamed module is special cased and can read all modules
  if (!is_named()) {
    return;
  }

  MutexLocker m1(Module_lock);

  if (m == NULL) {
    set_can_read_all_unnamed();
  } else {
    if (_reads == NULL) {
      // Lazily create a module's reads list
      _reads = new (ResourceObj::C_HEAP, mtModule)GrowableArray<ModuleEntry*>(MODULE_READS_SIZE, true);
    }

    // Determine, based on this newly established read edge to module m,
    // if this module's read list should be walked at a GC safepoint.
    set_read_walk_required(m->loader_data());

    // Establish readability to module m
    _reads->append_if_missing(m);
  }
}

// If the module's loader, that a read edge is being established to, is
// not the same loader as this module's and is not one of the 3 builtin
// class loaders, then this module's reads list must be walked at GC
// safepoint. Modules have the same life cycle as their defining class
// loaders and should be removed if dead.
void ModuleEntry::set_read_walk_required(ClassLoaderData* m_loader_data) {
  if (!_must_walk_reads && loader_data() != m_loader_data && !m_loader_data->is_builtin_class_loader_data()) {
    _must_walk_reads = true;
  }
}

// Set whether the module is open, i.e. all its packages are unqualifiedly exported
void ModuleEntry::set_is_open(bool is_open) {
  _is_open = is_open;
}

// Returns true if the module has a non-empty reads list. As such, the unnamed
// module will return false.
bool ModuleEntry::has_reads_list() const {
  return ((_reads != NULL) && !_reads->is_empty());
}

// Purge dead module entries out of reads list.
void ModuleEntry::purge_reads() {
  if (_must_walk_reads && has_reads_list()) {
    // This module's _must_walk_reads flag will be reset based
    // on the remaining live modules on the reads list.
    _must_walk_reads = false;

    // Go backwards because this removes entries that are dead.
    int len = _reads->length();
    for (int idx = len - 1; idx >= 0; idx--) {
      ModuleEntry* module_idx = _reads->at(idx);
      ClassLoaderData* cld_idx = module_idx->loader_data();
      if (cld_idx->is_unloading()) {
        _reads->delete_at(idx);
      } else {
        // Update the need to walk this module's reads based on live modules
        set_read_walk_required(cld_idx);
      }
    }
  }
}

void ModuleEntry::module_reads_do(ModuleClosure* f) {
  if (has_reads_list()) {
    int reads_len = _reads->length();
    for (int i = 0; i < reads_len; ++i) {
      f->do_module(_reads->at(i));
    }
  }
}

void ModuleEntry::delete_reads() {
  delete _reads;
  _reads = NULL;
}

ModuleEntry* ModuleEntry::create_unnamed_module(ClassLoaderData* cld) {
  // The java.lang.Module for this loader's
  // corresponding unnamed module can be found in the java.lang.ClassLoader object.
  oop module = java_lang_ClassLoader::unnamedModule(cld->class_loader());

  // Ensure that the unnamed module was correctly set when the class loader was constructed.
  // Guarantee will cause a recognizable crash if the user code has circumvented calling the ClassLoader constructor.
  ResourceMark rm;
  guarantee(java_lang_Module::is_instance(module), "The unnamed module for ClassLoader %s, is null or not an instance of java.lang.Module. The class loader has not been initialized correctly.", cld->loader_name_and_id());

  ModuleEntry* unnamed_module = new_unnamed_module_entry(Handle(Thread::current(), module), cld);

  // Store pointer to the ModuleEntry in the unnamed module's java.lang.Module object.
  java_lang_Module::set_module_entry(module, unnamed_module);

  return unnamed_module;
}

ModuleEntry* ModuleEntry::create_boot_unnamed_module(ClassLoaderData* cld) {
  // For the boot loader, the java.lang.Module for the unnamed module
  // is not known until a call to JVM_SetBootLoaderUnnamedModule is made. At
  // this point initially create the ModuleEntry for the unnamed module.
  ModuleEntry* unnamed_module = new_unnamed_module_entry(Handle(), cld);
  return unnamed_module;
}

// When creating an unnamed module, this is called without holding the Module_lock.
// This is okay because the unnamed module gets created before the ClassLoaderData
// is available to other threads.
ModuleEntry* ModuleEntry::new_unnamed_module_entry(Handle module_handle, ClassLoaderData* cld) {
  ModuleEntry* entry = (ModuleEntry*) NEW_C_HEAP_ARRAY(char, sizeof(ModuleEntry), mtModule);

  // Initialize everything BasicHashtable would
  entry->set_next(NULL);
  entry->set_hash(0);
  entry->set_literal(NULL);

  // Initialize fields specific to a ModuleEntry
  entry->init();

  // Unnamed modules can read all other unnamed modules.
  entry->set_can_read_all_unnamed();

  if (!module_handle.is_null()) {
    entry->set_module(cld->add_handle(module_handle));
  }

  entry->set_loader_data(cld);
  entry->_is_open = true;

  return entry;
}

void ModuleEntry::delete_unnamed_module() {
  // Do not need unlink_entry() since the unnamed module is not in the hashtable
  FREE_C_HEAP_ARRAY(char, this);
}

ModuleEntryTable::ModuleEntryTable(int table_size)
  : Hashtable<Symbol*, mtModule>(table_size, sizeof(ModuleEntry))
{
}

ModuleEntryTable::~ModuleEntryTable() {
  // Walk through all buckets and all entries in each bucket,
  // freeing each entry.
  for (int i = 0; i < table_size(); ++i) {
    for (ModuleEntry* m = bucket(i); m != NULL;) {
      ModuleEntry* to_remove = m;
      // read next before freeing.
      m = m->next();

      ResourceMark rm;

      // Clean out the C heap allocated reads list first before freeing the entry
      to_remove->delete_reads();
      if (to_remove->name() != NULL) {
        to_remove->name()->decrement_refcount();
      }
      if (to_remove->version() != NULL) {
        to_remove->version()->decrement_refcount();
      }
      if (to_remove->location() != NULL) {
        to_remove->location()->decrement_refcount();
      }

      // Unlink from the Hashtable prior to freeing
      unlink_entry(to_remove);
      FREE_C_HEAP_ARRAY(char, to_remove);
    }
  }
  free_buckets();
}

ModuleEntry* ModuleEntryTable::new_entry(unsigned int hash, Handle module_handle, bool is_open, Symbol* name, Symbol* version, Symbol* location, ClassLoaderData* loader_data) {
  ModuleEntry* entry = (ModuleEntry*)Hashtable<Symbol*, mtModule>::allocate_new_entry(hash, name);

  // Initialize fields specific to a ModuleEntry
  entry->init();
  if (name != NULL) {
    name->increment_refcount();
  } else {
    // Unnamed modules can read all other unnamed modules.
    entry->set_can_read_all_unnamed();
  }

  if (!module_handle.is_null()) {
    entry->set_module(loader_data->add_handle(module_handle));
  }

  entry->set_loader_data(loader_data);
  entry->set_version(version);
  entry->set_location(location);
  entry->set_is_open(is_open);

  if (ClassLoader::is_in_patch_mod_entries(name)) {
    entry->set_is_patched();
  }

  return entry;
}

void ModuleEntryTable::add_entry(int index, ModuleEntry* new_entry) {
  Hashtable<Symbol*, mtModule>::add_entry(index, (HashtableEntry<Symbol*, mtModule>*)new_entry);
}

ModuleEntry* ModuleEntryTable::locked_create_entry_or_null(Handle module_handle, bool is_open, Symbol* module_name, Symbol* module_version, Symbol* module_location, ClassLoaderData* loader_data) {
  // Check if module already exists.
  if (lookup_only(module_name) != NULL) {
    return NULL;
  } else {
    ModuleEntry* entry = new_entry(compute_hash(module_name), module_handle, is_open, module_name, module_version, module_location, loader_data);
    add_entry(index_for(module_name), entry);
    return entry;
  }
}

// lookup_only by Symbol* to find a ModuleEntry.
ModuleEntry* ModuleEntryTable::lookup_only(Symbol* name) {
  int index = index_for(name);
  for (ModuleEntry* m = bucket(index); m != NULL; m = m->next()) {
    if (m->name()->fast_compare(name) == 0) {
      return m;
    }
  }
  return NULL;
}

// Remove dead modules from all other alive modules' reads list.
// This should only occur at class unloading.
void ModuleEntryTable::purge_all_module_reads() {
  for (int i = 0; i < table_size(); i++) {
    for (ModuleEntry* entry = bucket(i); entry != NULL; entry = entry->next()) {
      entry->purge_reads();
    }
  }
}

void ModuleEntryTable::finalize_javabase(Handle module_handle, Symbol* version, Symbol* location) {
  ClassLoaderData* boot_loader_data = ClassLoaderData::the_null_class_loader_data();
  ModuleEntryTable* module_table = boot_loader_data->modules();

  if (module_handle.is_null()) {
    fatal("Unable to finalize module definition for " JAVA_BASE_NAME);
  }

  // Set java.lang.Module, version and location for java.base
  ModuleEntry* jb_module = javabase_moduleEntry();
  jb_module->set_version(version);
  jb_module->set_location(location);
  // Once java.base's ModuleEntry _module field is set with the known
  // java.lang.Module, java.base is considered "defined" to the VM.
  jb_module->set_module(boot_loader_data->add_handle(module_handle));

  // Store pointer to the ModuleEntry for java.base in the java.lang.Module object.
  java_lang_Module::set_module_entry(module_handle(), jb_module);
}

// Within java.lang.Class instances there is a java.lang.Module field that must
// be set with the defining module.  During startup, prior to java.base's definition,
// classes needing their module field set are added to the fixup_module_list.
// Their module field is set once java.base's java.lang.Module is known to the VM.
void ModuleEntryTable::patch_javabase_entries(Handle module_handle) {
  if (module_handle.is_null()) {
    fatal("Unable to patch the module field of classes loaded prior to "
          JAVA_BASE_NAME "'s definition, invalid java.lang.Module");
  }

  // Do the fixups for the basic primitive types
  java_lang_Class::set_module(Universe::int_mirror(), module_handle());
  java_lang_Class::set_module(Universe::float_mirror(), module_handle());
  java_lang_Class::set_module(Universe::double_mirror(), module_handle());
  java_lang_Class::set_module(Universe::byte_mirror(), module_handle());
  java_lang_Class::set_module(Universe::bool_mirror(), module_handle());
  java_lang_Class::set_module(Universe::char_mirror(), module_handle());
  java_lang_Class::set_module(Universe::long_mirror(), module_handle());
  java_lang_Class::set_module(Universe::short_mirror(), module_handle());
  java_lang_Class::set_module(Universe::void_mirror(), module_handle());

  // Do the fixups for classes that have already been created.
  GrowableArray <Klass*>* list = java_lang_Class::fixup_module_field_list();
  int list_length = list->length();
  for (int i = 0; i < list_length; i++) {
    Klass* k = list->at(i);
    java_lang_Class::fixup_module_field(k, module_handle);
    k->class_loader_data()->dec_keep_alive();
  }

  delete java_lang_Class::fixup_module_field_list();
  java_lang_Class::set_fixup_module_field_list(NULL);
}

void ModuleEntryTable::print(outputStream* st) {
  st->print_cr("Module Entry Table (table_size=%d, entries=%d)", table_size(), number_of_entries());
  for (int i = 0; i < table_size(); i++) {
    for (ModuleEntry* probe = bucket(i); probe != NULL; probe = probe->next()) {
      probe->print(st);
    }
  }
}

void ModuleEntry::print(outputStream* st) {
  ResourceMark rm;
  st->print_cr("entry " PTR_FORMAT " name %s module " PTR_FORMAT " loader %s version %s location %s strict %s next " PTR_FORMAT,
               p2i(this),
               name() == NULL ? UNNAMED_MODULE : name()->as_C_string(),
               p2i(module()),
               loader_data()->loader_name_and_id(),
               version() != NULL ? version()->as_C_string() : "NULL",
               location() != NULL ? location()->as_C_string() : "NULL",
               BOOL_TO_STR(!can_read_all_unnamed()), p2i(next()));
}

#include "precompiled.hpp"

#include "classfile/classLoaderData.inline.hpp"
#include "classfile/placeholders.hpp"
#include "classfile/systemDictionary.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/fieldType.hpp"
#include "utilities/hashtable.inline.hpp"

// Placeholder methods

PlaceholderEntry* PlaceholderTable::new_entry(int hash, Symbol* name, ClassLoaderData* loader_data, bool havesupername, Symbol* supername) {
  PlaceholderEntry* entry = (PlaceholderEntry*)Hashtable<Symbol*, mtClass>::new_entry(hash, name);
  // Hashtable with Symbol* literal must increment and decrement refcount.
  name->increment_refcount();
  entry->set_loader_data(loader_data);
  entry->set_havesupername(havesupername);
  entry->set_supername(supername);
  entry->set_superThreadQ(NULL);
  entry->set_loadInstanceThreadQ(NULL);
  entry->set_defineThreadQ(NULL);
  entry->set_definer(NULL);
  entry->set_instance_klass(NULL);
  return entry;
}

void PlaceholderTable::free_entry(PlaceholderEntry* entry) {
  // decrement Symbol refcount here because Hashtable doesn't.
  entry->literal()->decrement_refcount();
  if (entry->supername() != NULL) entry->supername()->decrement_refcount();
  Hashtable<Symbol*, mtClass>::free_entry(entry);
}

// Placeholder objects represent classes currently being loaded.
// All threads examining the placeholder table must hold the
// SystemDictionary_lock, so we don't need special precautions
// on store ordering here.
void PlaceholderTable::add_entry(int index, unsigned int hash, Symbol* class_name, ClassLoaderData* loader_data, bool havesupername, Symbol* supername) {
  // Both readers and writers are locked so it's safe to just
  // create the placeholder and insert it in the list without a membar.
  PlaceholderEntry* entry = new_entry(hash, class_name, loader_data, havesupername, supername);
  add_entry(index, entry);
}

// Remove a placeholder object.
void PlaceholderTable::remove_entry(int index, unsigned int hash, Symbol* class_name, ClassLoaderData* loader_data) {
  PlaceholderEntry** p = bucket_addr(index);
  while (*p) {
    PlaceholderEntry *probe = *p;
    if (probe->hash() == hash && probe->equals(class_name, loader_data)) {
      // Delete entry
      *p = probe->next();
      free_entry(probe);
      return;
    }
    p = probe->next_addr();
  }
}

PlaceholderEntry* PlaceholderTable::get_entry(int index, unsigned int hash, Symbol* class_name, ClassLoaderData* loader_data) {
  for (PlaceholderEntry *place_probe = bucket(index); place_probe != NULL; place_probe = place_probe->next()) {
    if (place_probe->hash() == hash && place_probe->equals(class_name, loader_data)) {
      return place_probe;
    }
  }
  return NULL;
}

Symbol* PlaceholderTable::find_entry(int index, unsigned int hash, Symbol* class_name, ClassLoaderData* loader_data) {
  PlaceholderEntry* probe = get_entry(index, hash, class_name, loader_data);
  return (probe? probe->klassname(): (Symbol*)NULL);
}

  // find_and_add returns probe pointer - old or new
  // If no entry exists, add a placeholder entry
  // If entry exists, reuse entry
  // For both, push SeenThread for classloadAction
  // if havesupername: this is used for circularity for instanceklass loading
PlaceholderEntry* PlaceholderTable::find_and_add(int index, unsigned int hash, Symbol* name, ClassLoaderData* loader_data, classloadAction action, Symbol* supername, Thread* thread) {
  PlaceholderEntry* probe = get_entry(index, hash, name, loader_data);
  if (probe == NULL) {
    // Nothing found, add place holder
    add_entry(index, hash, name, loader_data, (action == LOAD_SUPER), supername);
    probe = get_entry(index, hash, name, loader_data);
  } else {
    if (action == LOAD_SUPER) {
      probe->set_havesupername(true);
      probe->set_supername(supername);
    }
  }
  if (probe) probe->add_seen_thread(thread, action);
  return probe;
}

// placeholder is used to track class loading internal states
// placeholder existence now for loading superclass/superinterface
// superthreadQ tracks class circularity, while loading superclass/superinterface
// loadInstanceThreadQ tracks load_instance_class calls
// definer() tracks the single thread that owns define token
// defineThreadQ tracks waiters on defining thread's results
// 1st claimant creates placeholder
// find_and_add adds SeenThread entry for appropriate queue
// All claimants remove SeenThread after completing action
// On removal: if definer and all queues empty, remove entry
// Note: you can be in both placeholders and systemDictionary
// Therefore - must always check SD first
// Ignores the case where entry is not found
void PlaceholderTable::find_and_remove(int index, unsigned int hash, Symbol* name, ClassLoaderData* loader_data, classloadAction action, Thread* thread) {
    PlaceholderEntry *probe = get_entry(index, hash, name, loader_data);
    if (probe != NULL) {
       probe->remove_seen_thread(thread, action);
       // If no other threads using this entry, and this thread is not using this entry for other states
       if ((probe->superThreadQ() == NULL) && (probe->loadInstanceThreadQ() == NULL) && (probe->defineThreadQ() == NULL) && (probe->definer() == NULL)) {
         remove_entry(index, hash, name, loader_data);
       }
    }
  }

PlaceholderTable::PlaceholderTable(int table_size) : Hashtable<Symbol*, mtClass>(table_size, sizeof(PlaceholderEntry)) { }

// Note, doesn't append a cr
// Can't call this print_on because HashtableEntry doesn't initialize its vptr
// and print_on is a virtual function so the vptr call crashes.
void PlaceholderEntry::print_entry(outputStream* st) const {
  klassname()->print_value_on(st);
  if (loader_data() != NULL) {
    st->print(", loader ");
    loader_data()->print_value_on(st);
  }
  if (supername() != NULL) {
    st->print(", supername ");
    supername()->print_value_on(st);
  }
  if (definer() != NULL) {
    st->print(", definer ");
    definer()->print_value_on(st);
  }
  if (instance_klass() != NULL) {
    st->print(", InstanceKlass ");
    instance_klass()->print_value_on(st);
  }
  st->cr();
  st->print("loadInstanceThreadQ threads:");
  loadInstanceThreadQ()->print_action_queue(st);
  st->cr();
  st->print("superThreadQ threads:");
  superThreadQ()->print_action_queue(st);
  st->cr();
  st->print("defineThreadQ threads:");
  defineThreadQ()->print_action_queue(st);
  st->cr();
}

void PlaceholderTable::print_on(outputStream* st) const {
  st->print_cr("Placeholder table (table_size=%d, placeholders=%d)", table_size(), number_of_entries());
  for (int pindex = 0; pindex < table_size(); pindex++) {
    for (PlaceholderEntry* probe = bucket(pindex); probe != NULL; probe = probe->next()) {
      st->print("%4d: placeholder ", pindex);
      probe->print_entry(st);
    }
  }
}

#include "precompiled.hpp"
#include "classfile/resolutionErrors.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/safepoint.hpp"
#include "utilities/hashtable.inline.hpp"

// add new entry to the table
void ResolutionErrorTable::add_entry(int index, unsigned int hash, const constantPoolHandle& pool, int cp_index, Symbol* error, Symbol* message) {
  ResolutionErrorEntry* entry = new_entry(hash, pool(), cp_index, error, message);
  add_entry(index, entry);
}

// find entry in the table
ResolutionErrorEntry* ResolutionErrorTable::find_entry(int index, unsigned int hash, const constantPoolHandle& pool, int cp_index) {
  for (ResolutionErrorEntry *error_probe = bucket(index); error_probe != NULL; error_probe = error_probe->next()) {
  if (error_probe->hash() == hash && error_probe->pool() == pool()) {
      return error_probe;
    }
  }
  return NULL;
}

void ResolutionErrorEntry::set_error(Symbol* e) {
  _error = e;
  _error->increment_refcount();
}

void ResolutionErrorEntry::set_message(Symbol* c) {
  _message = c;
  _message->increment_refcount();
}

// create new error entry
ResolutionErrorEntry* ResolutionErrorTable::new_entry(int hash, ConstantPool* pool,
                                                      int cp_index, Symbol* error,
                                                      Symbol* message)
{
  ResolutionErrorEntry* entry = (ResolutionErrorEntry*)Hashtable<ConstantPool*, mtClass>::new_entry(hash, pool);
  entry->set_cp_index(cp_index);
  entry->set_error(error);
  entry->set_message(message);

  return entry;
}

void ResolutionErrorTable::free_entry(ResolutionErrorEntry *entry) {
  // decrement error refcount
  entry->error()->decrement_refcount();
  entry->message()->decrement_refcount();
  Hashtable<ConstantPool*, mtClass>::free_entry(entry);
}

// create resolution error table
ResolutionErrorTable::ResolutionErrorTable(int table_size)
    : Hashtable<ConstantPool*, mtClass>(table_size, sizeof(ResolutionErrorEntry)) {
}

// RedefineClasses support - remove matching entry of a
// constant pool that is going away
void ResolutionErrorTable::delete_entry(ConstantPool* c) {
  for (int i = 0; i < table_size(); i++) {
    for (ResolutionErrorEntry** p = bucket_addr(i); *p != NULL; ) {
      ResolutionErrorEntry* entry = *p;
      if (entry->pool() == c) {
        *p = entry->next();
        free_entry(entry);
      } else {
        p = entry->next_addr();
      }
    }
  }
}

// Remove unloaded entries from the table
void ResolutionErrorTable::purge_resolution_errors() {
  for (int i = 0; i < table_size(); i++) {
    for (ResolutionErrorEntry** p = bucket_addr(i); *p != NULL; ) {
      ResolutionErrorEntry* entry = *p;
      ConstantPool* pool = entry->pool();

      if (pool->pool_holder()->is_loader_alive()) {
        p = entry->next_addr();
      } else {
        *p = entry->next();
        free_entry(entry);
      }
    }
  }
}

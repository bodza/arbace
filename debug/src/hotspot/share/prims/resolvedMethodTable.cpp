#include "precompiled.hpp"

#include "classfile/javaClasses.hpp"
#include "logging/log.hpp"
#include "memory/allocation.hpp"
#include "memory/resourceArea.hpp"
#include "oops/access.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/method.hpp"
#include "oops/symbol.hpp"
#include "oops/weakHandle.inline.hpp"
#include "prims/resolvedMethodTable.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "utilities/hashtable.inline.hpp"
#include "utilities/macros.hpp"

oop ResolvedMethodEntry::object() {
  return literal().resolve();
}

oop ResolvedMethodEntry::object_no_keepalive() {
  // The AS_NO_KEEPALIVE peeks at the oop without keeping it alive.
  // This is dangerous in general but is okay if the loaded oop does
  // not leak out past a thread transition where a safepoint can happen.
  // A subsequent oop_load without AS_NO_KEEPALIVE (the object() accessor)
  // keeps the oop alive before doing so.
  return literal().peek();
}

ResolvedMethodTable::ResolvedMethodTable()
  : Hashtable<ClassLoaderWeakHandle, mtClass>(_table_size, sizeof(ResolvedMethodEntry)) { }

oop ResolvedMethodTable::lookup(int index, unsigned int hash, Method* method) {
  for (ResolvedMethodEntry* p = bucket(index); p != NULL; p = p->next()) {
    if (p->hash() == hash) {

      // Peek the object to check if it is the right target.
      oop target = p->object_no_keepalive();

      // The method is in the table as a target already
      if (target != NULL && java_lang_invoke_ResolvedMethodName::vmtarget(target) == method) {
        ResourceMark rm;
        // The object() accessor makes sure the target object is kept alive before leaking out.
        return p->object();
      }
    }
  }
  return NULL;
}

unsigned int ResolvedMethodTable::compute_hash(Method* method) {
  unsigned int name_hash = method->name()->identity_hash();
  unsigned int signature_hash = method->signature()->identity_hash();
  return name_hash ^ signature_hash;
}

oop ResolvedMethodTable::lookup(Method* method) {
  unsigned int hash = compute_hash(method);
  int index = hash_to_index(hash);
  return lookup(index, hash, method);
}

oop ResolvedMethodTable::basic_add(Method* method, Handle rmethod_name) {
  unsigned int hash = compute_hash(method);
  int index = hash_to_index(hash);

  // One was added while aquiring the lock
  oop entry = lookup(index, hash, method);
  if (entry != NULL) {
    return entry;
  }

  ClassLoaderWeakHandle w = ClassLoaderWeakHandle::create(rmethod_name);
  ResolvedMethodEntry* p = (ResolvedMethodEntry*) Hashtable<ClassLoaderWeakHandle, mtClass>::new_entry(hash, w);
  Hashtable<ClassLoaderWeakHandle, mtClass>::add_entry(index, p);
  ResourceMark rm;
  return rmethod_name();
}

ResolvedMethodTable* ResolvedMethodTable::_the_table = NULL;

oop ResolvedMethodTable::find_method(Method* method) {
  oop entry = _the_table->lookup(method);
  return entry;
}

oop ResolvedMethodTable::add_method(Handle resolved_method_name) {
  MutexLocker ml(ResolvedMethodTable_lock);

  // Check if method has been redefined while taking out ResolvedMethodTable_lock, if so
  // use new method.
  Method* method = (Method*)java_lang_invoke_ResolvedMethodName::vmtarget(resolved_method_name());
  if (method->is_old()) {
    // Replace method with redefined version
    InstanceKlass* holder = method->method_holder();
    method = holder->method_with_idnum(method->method_idnum());
    java_lang_invoke_ResolvedMethodName::set_vmtarget(resolved_method_name(), method);
  }
  // Set flag in class to indicate this InstanceKlass has entries in the table
  // to avoid walking table during redefinition if none of the redefined classes
  // have any membernames in the table.
  method->method_holder()->set_has_resolved_methods();

  return _the_table->basic_add(method, resolved_method_name);
}

// Removing entries
int ResolvedMethodTable::_oops_removed = 0;
int ResolvedMethodTable::_oops_counted = 0;

// Serially invoke removed unused oops from the table.
// This is done late during GC.
void ResolvedMethodTable::unlink() {
  _oops_removed = 0;
  _oops_counted = 0;
  for (int i = 0; i < _the_table->table_size(); ++i) {
    ResolvedMethodEntry** p = _the_table->bucket_addr(i);
    ResolvedMethodEntry* entry = _the_table->bucket(i);
    while (entry != NULL) {
      _oops_counted++;
      oop l = entry->object_no_keepalive();
      if (l != NULL) {
        p = entry->next_addr();
      } else {
        // Entry has been removed.
        _oops_removed++;
        entry->literal().release();
        *p = entry->next();
        _the_table->free_entry(entry);
      }
      // get next entry
      entry = (ResolvedMethodEntry*)HashtableEntry<ClassLoaderWeakHandle, mtClass>::make_ptr(*p);
    }
  }
}

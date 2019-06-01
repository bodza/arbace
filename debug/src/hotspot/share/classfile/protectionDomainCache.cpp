#include "precompiled.hpp"

#include "classfile/protectionDomainCache.hpp"
#include "classfile/systemDictionary.hpp"
#include "logging/log.hpp"
#include "logging/logStream.hpp"
#include "memory/iterator.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "oops/weakHandle.inline.hpp"
#include "utilities/hashtable.inline.hpp"

unsigned int ProtectionDomainCacheTable::compute_hash(Handle protection_domain) {
  // Identity hash can safepoint, so keep protection domain in a Handle.
  return (unsigned int)(protection_domain->identity_hash());
}

int ProtectionDomainCacheTable::index_for(Handle protection_domain) {
  return hash_to_index(compute_hash(protection_domain));
}

ProtectionDomainCacheTable::ProtectionDomainCacheTable(int table_size)
  : Hashtable<ClassLoaderWeakHandle, mtClass>(table_size, sizeof(ProtectionDomainCacheEntry))
{
}

void ProtectionDomainCacheTable::unlink() {
  for (int i = 0; i < table_size(); ++i) {
    ProtectionDomainCacheEntry** p = bucket_addr(i);
    ProtectionDomainCacheEntry* entry = bucket(i);
    while (entry != NULL) {
      oop pd = entry->object_no_keepalive();
      if (pd != NULL) {
        p = entry->next_addr();
      } else {
        LogTarget(Debug, protectiondomain) lt;
        if (lt.is_enabled()) {
          LogStream ls(lt);
          ls.print_cr("protection domain unlinked at %d", i);
        }
        entry->literal().release();
        *p = entry->next();
        free_entry(entry);
      }
      entry = *p;
    }
  }
}

void ProtectionDomainCacheTable::print_on(outputStream* st) const {
  st->print_cr("Protection domain cache table (table_size=%d, classes=%d)", table_size(), number_of_entries());
  for (int index = 0; index < table_size(); index++) {
    for (ProtectionDomainCacheEntry* probe = bucket(index); probe != NULL; probe = probe->next()) {
      st->print_cr("%4d: protection_domain: " PTR_FORMAT, index, p2i(probe->object_no_keepalive()));
    }
  }
}

void ProtectionDomainCacheTable::verify() {
  verify_table<ProtectionDomainCacheEntry>("Protection Domain Table");
}

oop ProtectionDomainCacheEntry::object() {
  return literal().resolve();
}

oop ProtectionDomainEntry::object() {
  return _pd_cache->object();
}

// The object_no_keepalive() call peeks at the phantomly reachable oop without
// keeping it alive. This is okay to do in the VM thread state if it is not
// leaked out to become strongly reachable.
oop ProtectionDomainCacheEntry::object_no_keepalive() {
  return literal().peek();
}

oop ProtectionDomainEntry::object_no_keepalive() {
  return _pd_cache->object_no_keepalive();
}

void ProtectionDomainCacheEntry::verify() {
  guarantee(object_no_keepalive() == NULL || oopDesc::is_oop(object_no_keepalive()), "must be an oop");
}

ProtectionDomainCacheEntry* ProtectionDomainCacheTable::get(Handle protection_domain) {
  unsigned int hash = compute_hash(protection_domain);
  int index = hash_to_index(hash);

  ProtectionDomainCacheEntry* entry = find_entry(index, protection_domain);
  if (entry == NULL) {
    entry = add_entry(index, hash, protection_domain);
  }
  // keep entry alive
  (void)entry->object();
  return entry;
}

ProtectionDomainCacheEntry* ProtectionDomainCacheTable::find_entry(int index, Handle protection_domain) {
  for (ProtectionDomainCacheEntry* e = bucket(index); e != NULL; e = e->next()) {
    if (oopDesc::equals(e->object_no_keepalive(), protection_domain())) {
      return e;
    }
  }

  return NULL;
}

ProtectionDomainCacheEntry* ProtectionDomainCacheTable::add_entry(int index, unsigned int hash, Handle protection_domain) {
  ClassLoaderWeakHandle w = ClassLoaderWeakHandle::create(protection_domain);
  ProtectionDomainCacheEntry* p = new_entry(hash, w);
  Hashtable<ClassLoaderWeakHandle, mtClass>::add_entry(index, p);
  return p;
}

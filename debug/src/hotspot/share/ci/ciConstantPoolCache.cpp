#include "precompiled.hpp"

#include "ci/ciConstantPoolCache.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "memory/allocation.hpp"
#include "memory/allocation.inline.hpp"

// ciConstantPoolCache
//
// This class caches indexed constant pool lookups.

ciConstantPoolCache::ciConstantPoolCache(Arena* arena, int expected_size) {
  _elements = new (arena) GrowableArray<void*>(arena, expected_size, 0, 0);
  _keys = new (arena) GrowableArray<int>(arena, expected_size, 0, 0);
}

int ciConstantPoolCache::key_compare(const int& key, const int& elt) {
  if (key < elt)      return -1;
  else if (key > elt) return 1;
  else                return 0;
}

// Get the entry at some index
void* ciConstantPoolCache::get(int index) {
  bool found = false;
  int pos = _keys->find_sorted<int, ciConstantPoolCache::key_compare>(index, found);
  if (!found) {
    // This element is not present in the cache.
    return NULL;
  }
  return _elements->at(pos);
}

// Insert a ciObject into the table at some index.
void ciConstantPoolCache::insert(int index, void* elem) {
  bool found = false;
  int pos = _keys->find_sorted<int, ciConstantPoolCache::key_compare>(index, found);
  _keys->insert_before(pos, index);
  _elements->insert_before(pos, elem);
}

// Print debugging information about the cache.
void ciConstantPoolCache::print() {
  Unimplemented();
}

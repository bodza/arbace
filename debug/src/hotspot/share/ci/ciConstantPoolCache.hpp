#ifndef SHARE_VM_CI_CICONSTANTPOOLCACHE_HPP
#define SHARE_VM_CI_CICONSTANTPOOLCACHE_HPP

#include "memory/resourceArea.hpp"
#include "utilities/growableArray.hpp"

// ciConstantPoolCache
//
// The class caches indexed constant pool lookups.
//
// Usage note: this klass has nothing to do with ConstantPoolCache*.
class ciConstantPoolCache : public ResourceObj {
private:
  GrowableArray<int>*   _keys;
  GrowableArray<void*>* _elements;

  static int key_compare(const int& key, const int& elt);

public:
  ciConstantPoolCache(Arena* arena, int expected_size);

  // Get the element associated with some index.
  void* get(int index);

  // Associate an element with an index.
  void insert(int index, void* element);

  void print();
};

#endif

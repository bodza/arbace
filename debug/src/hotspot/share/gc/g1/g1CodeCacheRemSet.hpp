#ifndef SHARE_VM_GC_G1_G1CODECACHEREMSET_HPP
#define SHARE_VM_GC_G1_G1CODECACHEREMSET_HPP

class CodeBlobClosure;
class G1CodeRootSetTable;
class HeapRegion;
class nmethod;

// Implements storage for a set of code roots.
// All methods that modify the set are not thread-safe except if otherwise noted.
class G1CodeRootSet {
  friend class G1CodeRootSetTest;
 private:
  const static size_t SmallSize = 32;
  const static size_t Threshold = 24;
  const static size_t LargeSize = 512;

  G1CodeRootSetTable* _table;
  G1CodeRootSetTable* load_acquire_table();

  size_t _length;

  void move_to_large();
  void allocate_small_table();

 public:
  G1CodeRootSet() : _table(NULL), _length(0) { }
  ~G1CodeRootSet();

  static void purge();

  static size_t static_mem_size();

  void add(nmethod* method);

  bool remove(nmethod* method);

  // Safe to call without synchronization, but may return false negatives.
  bool contains(nmethod* method);

  void clear();

  void nmethods_do(CodeBlobClosure* blk) const;

  // Remove all nmethods which no longer contain pointers into our "owner" region
  void clean(HeapRegion* owner);

  bool is_empty() {
    bool empty = length() == 0;
    return empty;
  }

  // Length in elements
  size_t length() const { return _length; }

  // Memory size in bytes taken by this set.
  size_t mem_size();
};

#endif

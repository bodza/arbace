#ifndef SHARE_VM_MEMORY_FREELIST_HPP
#define SHARE_VM_MEMORY_FREELIST_HPP

// A class for maintaining a free list of Chunk's.  The FreeList
// maintains a the structure of the list (head, tail, etc.) plus
// statistics for allocations from the list.  The links between items
// are not part of FreeList.  The statistics are
// used to make decisions about coalescing Chunk's when they
// are swept during collection.
//
// See the corresponding .cpp file for a description of the specifics
// for that implementation.

class Mutex;

template <class Chunk_t>
class FreeList {
  friend class CompactibleFreeListSpace;
  friend class VMStructs;

 private:
  Chunk_t*      _head;          // Head of list of free chunks
  Chunk_t*      _tail;          // Tail of list of free chunks
  size_t        _size;          // Size in Heap words of each chunk
  ssize_t       _count;         // Number of entries in list

 protected:

  // Asserts false if the protecting lock (if any) is not held.
  void assert_proper_lock_protection() const { }

  void increment_count()    {
    _count++;
  }

  void decrement_count() {
    _count--;
  }

 public:
  // Constructor
  // Construct a list without any entries.
  FreeList();

  // Do initialization
  void initialize();

  // Reset the head, tail, and count of a free list.
  void reset();

  // Declare the current free list to be protected by the given lock.

  // Accessors.
  Chunk_t* head() const {
    assert_proper_lock_protection();
    return _head;
  }
  void set_head(Chunk_t* v) {
    assert_proper_lock_protection();
    _head = v;
  }
  // Set the head of the list and set the prev field of non-null
  // values to NULL.
  void link_head(Chunk_t* v);

  Chunk_t* tail() const {
    assert_proper_lock_protection();
    return _tail;
  }
  void set_tail(Chunk_t* v) {
    assert_proper_lock_protection();
    _tail = v;
  }
  // Set the tail of the list and set the next field of non-null
  // values to NULL.
  void link_tail(Chunk_t* v) {
    assert_proper_lock_protection();
    set_tail(v);
    if (v != NULL) {
      v->clear_next();
    }
  }

  // No locking checks in read-accessors: lock-free reads (only) are benign.
  // Readers are expected to have the lock if they are doing work that
  // requires atomicity guarantees in sections of code.
  size_t size() const {
    return _size;
  }
  void set_size(size_t v) {
    assert_proper_lock_protection();
    _size = v;
  }
  ssize_t count() const { return _count; }
  void set_count(ssize_t v) { _count = v; }

  size_t get_better_size() { return size(); }

  size_t returned_bytes() const { ShouldNotReachHere(); return 0; }
  void set_returned_bytes(size_t v) { }
  void increment_returned_bytes_by(size_t v) { }

  // Unlink head of list and return it.  Returns NULL if
  // the list is empty.
  Chunk_t* get_chunk_at_head();

  // Remove the first "n" or "count", whichever is smaller, chunks from the
  // list, setting "fl", which is required to be empty, to point to them.
  void getFirstNChunksFromList(size_t n, FreeList<Chunk_t>* fl);

  // Unlink this chunk from it's free list
  void remove_chunk(Chunk_t* fc);

  // Add this chunk to this free list.
  void return_chunk_at_head(Chunk_t* fc);
  void return_chunk_at_tail(Chunk_t* fc);

  // Similar to returnChunk* but also records some diagnostic
  // information.
  void return_chunk_at_head(Chunk_t* fc, bool record_return);
  void return_chunk_at_tail(Chunk_t* fc, bool record_return);

  // Prepend "fl" (whose size is required to be the same as that of "this")
  // to the front of "this" list.
  void prepend(FreeList<Chunk_t>* fl);

  // Verify that the chunk is in the list.
  // found.  Return NULL if "fc" is not found.
  bool verify_chunk_in_free_list(Chunk_t* fc) const;

  // Printing support
  static void print_labels_on(outputStream* st, const char* c);
  void print_on(outputStream* st, const char* c = NULL) const;
};

#endif

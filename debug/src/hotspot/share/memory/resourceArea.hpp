#ifndef SHARE_VM_MEMORY_RESOURCEAREA_HPP
#define SHARE_VM_MEMORY_RESOURCEAREA_HPP

#include "memory/allocation.hpp"
#include "runtime/thread.hpp"

//------------------------------ResourceArea-----------------------------------
// A ResourceArea is an Arena that supports safe usage of ResourceMark.
class ResourceArea: public Arena {
  friend class ResourceMark;
  friend class VMStructs;

public:
  ResourceArea(MEMFLAGS flags = mtThread) : Arena(flags) { }

  ResourceArea(size_t init_size, MEMFLAGS flags = mtThread) : Arena(flags, init_size) { }

  char* allocate_bytes(size_t size, AllocFailType alloc_failmode = AllocFailStrategy::EXIT_OOM);

  // Bias this resource area to specific memory type
  // (by default, ResourceArea is tagged as mtThread, per-thread general purpose storage)
  void bias_to(MEMFLAGS flags);
};

//------------------------------ResourceMark-----------------------------------
// A resource mark releases all resources allocated after it was constructed
// when the destructor is called.  Typically used as a local variable.
class ResourceMark: public StackObj {
protected:
  ResourceArea *_area;          // Resource area to stack allocate
  Chunk *_chunk;                // saved arena chunk
  char *_hwm, *_max;
  size_t _size_in_bytes;

  void initialize(Thread *thread) {
    _area = thread->resource_area();
    _chunk = _area->_chunk;
    _hwm = _area->_hwm;
    _max= _area->_max;
    _size_in_bytes = _area->size_in_bytes();
  }
 public:
  ResourceMark(Thread *thread) {
    initialize(thread);
  }

  ResourceMark() { initialize(Thread::current()); }

  ResourceMark( ResourceArea *r ) :
    _area(r), _chunk(r->_chunk), _hwm(r->_hwm), _max(r->_max) {
    _size_in_bytes = r->_size_in_bytes;
  }

  void reset_to_mark() {
    if (UseMallocOnly) free_malloced_objects();

    if (_chunk->next()) {       // Delete later chunks
      _area->set_size_in_bytes(size_in_bytes());
      _chunk->next_chop();
    }
    _area->_chunk = _chunk;     // Roll back arena to saved chunk
    _area->_hwm = _hwm;
    _area->_max = _max;

    // clear out this chunk (to detect allocation bugs)
    if (ZapResourceArea) memset(_hwm, badResourceValue, _max - _hwm);
  }

  ~ResourceMark() {
    reset_to_mark();
  }

 private:
  void free_malloced_objects()                                         { };
  size_t size_in_bytes() { return _size_in_bytes; }
};

#endif

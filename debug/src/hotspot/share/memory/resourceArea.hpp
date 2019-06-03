#ifndef SHARE_VM_MEMORY_RESOURCEAREA_HPP
#define SHARE_VM_MEMORY_RESOURCEAREA_HPP

#include "memory/allocation.hpp"
#include "runtime/thread.hpp"

//------------------------------ResourceArea-----------------------------------
// A ResourceArea is an Arena that supports safe usage of ResourceMark.
class ResourceArea: public Arena {
  friend class ResourceMark;
  friend class DeoptResourceMark;
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

  ResourceMark()               { initialize(Thread::current()); }

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

//------------------------------DeoptResourceMark-----------------------------------
// A deopt resource mark releases all resources allocated after it was constructed
// when the destructor is called.  Typically used as a local variable. It differs
// from a typical resource more in that it is C-Heap allocated so that deoptimization
// can use data structures that are arena based but are not amenable to vanilla
// ResourceMarks because deoptimization can not use a stack allocated mark. During
// deoptimization we go thru the following steps:
//
// 0: start in assembly stub and call either uncommon_trap/fetch_unroll_info
// 1: create the vframeArray (contains pointers to Resource allocated structures)
//   This allocates the DeoptResourceMark.
// 2: return to assembly stub and remove stub frame and deoptee frame and create
//    the new skeletal frames.
// 3: push new stub frame and call unpack_frames
// 4: retrieve information from the vframeArray to populate the skeletal frames
// 5: release the DeoptResourceMark
// 6: return to stub and eventually to interpreter
//
// With old style eager deoptimization the vframeArray was created by the vmThread there
// was no way for the vframeArray to contain resource allocated objects and so
// a complex set of data structures to simulate an array of vframes in CHeap memory
// was used. With new style lazy deoptimization the vframeArray is created in the
// the thread that will use it and we can use a much simpler scheme for the vframeArray
// leveraging existing data structures if we simply create a way to manage this one
// special need for a ResourceMark. If ResourceMark simply inherited from CHeapObj
// then existing ResourceMarks would work fine since no one use new to allocate them
// and they would be stack allocated. This leaves open the possibility of accidental
// misuse so we simple duplicate the ResourceMark functionality here.

class DeoptResourceMark: public CHeapObj<mtInternal> {
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
  DeoptResourceMark(Thread *thread) {
    initialize(thread);
  }

  DeoptResourceMark()               { initialize(Thread::current()); }

  DeoptResourceMark( ResourceArea *r ) :
    _area(r), _chunk(r->_chunk), _hwm(r->_hwm), _max(r->_max) {
    _size_in_bytes = _area->size_in_bytes();
  }

  void reset_to_mark() {
    if (UseMallocOnly) free_malloced_objects();

    if (_chunk->next()) {        // Delete later chunks
      _area->set_size_in_bytes(size_in_bytes());
      _chunk->next_chop();
    }
    _area->_chunk = _chunk;     // Roll back arena to saved chunk
    _area->_hwm = _hwm;
    _area->_max = _max;

    // clear out this chunk (to detect allocation bugs)
    if (ZapResourceArea) memset(_hwm, badResourceValue, _max - _hwm);
  }

  ~DeoptResourceMark() {
    reset_to_mark();
  }

 private:
  void free_malloced_objects()                                         { };
  size_t size_in_bytes() { return _size_in_bytes; };
};

#endif

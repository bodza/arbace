#include "precompiled.hpp"

#include "memory/allocation.inline.hpp"
#include "oops/constantPool.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/thread.inline.hpp"

// Copy constructors and destructors for metadata handles
// These do too much to inline.
#define DEF_METADATA_HANDLE_FN_NOINLINE(name, type) \
name##Handle::name##Handle(const name##Handle &h) { \
  _value = h._value; \
  if (_value != NULL) { \
    if (h._thread != NULL) { \
      _thread = h._thread; \
    } else { \
      _thread = Thread::current(); \
    } \
    _thread->metadata_handles()->push((Metadata*)_value); \
  } else { \
    _thread = NULL; \
  } \
} \
name##Handle& name##Handle::operator=(const name##Handle &s) { \
  remove(); \
  _value = s._value; \
  if (_value != NULL) { \
    if (s._thread != NULL) { \
      _thread = s._thread; \
    } else { \
      _thread = Thread::current(); \
    } \
    _thread->metadata_handles()->push((Metadata*)_value); \
  } else { \
    _thread = NULL; \
  } \
  return *this; \
} \
inline void name##Handle::remove() { \
  if (_value != NULL) { \
    int i = _thread->metadata_handles()->find_from_end((Metadata*)_value); \
    _thread->metadata_handles()->remove_at(i); \
  } \
} \
name##Handle::~name##Handle () { remove(); }

DEF_METADATA_HANDLE_FN_NOINLINE(method, Method)
DEF_METADATA_HANDLE_FN_NOINLINE(constantPool, ConstantPool)

static uintx chunk_oops_do(OopClosure* f, Chunk* chunk, char* chunk_top) {
  oop* bottom = (oop*) chunk->bottom();
  oop* top    = (oop*) chunk_top;
  uintx handles_visited = top - bottom;
  // during GC phase 3, a handle may be a forward pointer that
  // is not yet valid, so loosen the assertion
  while (bottom < top) {
    f->do_oop(bottom++);
  }
  return handles_visited;
}

void HandleArea::oops_do(OopClosure* f) {
  uintx handles_visited = 0;
  // First handle the current chunk. It is filled to the high water mark.
  handles_visited += chunk_oops_do(f, _chunk, _hwm);
  // Then handle all previous chunks. They are completely filled.
  Chunk* k = _first;
  while (k != _chunk) {
    handles_visited += chunk_oops_do(f, k, k->top());
    k = k->next();
  }

  if (_prev != NULL) _prev->oops_do(f);
}

void HandleMark::initialize(Thread* thread) {
  _thread = thread;
  // Save area
  _area  = thread->handle_area();
  // Save current top
  _chunk = _area->_chunk;
  _hwm   = _area->_hwm;
  _max   = _area->_max;
  _size_in_bytes = _area->_size_in_bytes;

  // Link this in the thread
  set_previous_handle_mark(thread->last_handle_mark());
  thread->set_last_handle_mark(this);
}

HandleMark::~HandleMark() {
  HandleArea* area = _area;   // help compilers with poor alias analysis

  // Delete later chunks
  if (_chunk->next()) {
    // reset arena size before delete chunks. Otherwise, the total
    // arena size could exceed total chunk size
    area->set_size_in_bytes(size_in_bytes());
    _chunk->next_chop();
  }
  // Roll back arena to saved top markers
  area->_chunk = _chunk;
  area->_hwm = _hwm;
  area->_max = _max;

  // Unlink this from the thread
  _thread->set_last_handle_mark(previous_handle_mark());
}

void* HandleMark::operator new(size_t size) throw() {
  return AllocateHeap(size, mtThread);
}

void* HandleMark::operator new [] (size_t size) throw() {
  return AllocateHeap(size, mtThread);
}

void HandleMark::operator delete(void* p) {
  FreeHeap(p);
}

void HandleMark::operator delete[](void* p) {
  FreeHeap(p);
}

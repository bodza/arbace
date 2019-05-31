#ifndef SHARE_VM_UTILITIES_CHUNKED_LIST_HPP
#define SHARE_VM_UTILITIES_CHUNKED_LIST_HPP

#include "memory/allocation.hpp"
#include "utilities/debug.hpp"

template <class T, MEMFLAGS F> class ChunkedList : public CHeapObj<F> {
  template <class U> friend class TestChunkedList;

  static const size_t BufferSize = 64;

  T  _values[BufferSize];
  T* _top;

  ChunkedList<T, F>* _next_used;
  ChunkedList<T, F>* _next_free;

  T const * end() const {
    return &_values[BufferSize];
  }

 public:
  ChunkedList<T, F>() : _top(_values), _next_used(NULL), _next_free(NULL) { }

  bool is_full() const {
    return _top == end();
  }

  void clear() {
    _top = _values;
    // Don't clear the next pointers since that would interfere
    // with other threads trying to iterate through the lists.
  }

  void push(T m) {
    *_top = m;
    _top++;
  }

  void set_next_used(ChunkedList<T, F>* buffer) { _next_used = buffer; }
  void set_next_free(ChunkedList<T, F>* buffer) { _next_free = buffer; }

  ChunkedList<T, F>* next_used() const          { return _next_used; }
  ChunkedList<T, F>* next_free() const          { return _next_free; }

  size_t size() const {
    return pointer_delta(_top, _values, sizeof(T));
  }

  T at(size_t i) {
    return _values[i];
  }
};

#endif

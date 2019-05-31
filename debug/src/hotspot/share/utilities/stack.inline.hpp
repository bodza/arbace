#ifndef SHARE_VM_UTILITIES_STACK_INLINE_HPP
#define SHARE_VM_UTILITIES_STACK_INLINE_HPP

#include "memory/allocation.inline.hpp"
#include "utilities/align.hpp"
#include "utilities/stack.hpp"
#include "utilities/copy.hpp"

template <MEMFLAGS F> StackBase<F>::StackBase(size_t segment_size, size_t max_cache_size,
                     size_t max_size):
  _seg_size(segment_size),
  _max_cache_size(max_cache_size),
  _max_size(adjust_max_size(max_size, segment_size))
{
}

template <MEMFLAGS F> size_t StackBase<F>::adjust_max_size(size_t max_size, size_t seg_size)
{
  const size_t limit = max_uintx - (seg_size - 1);
  if (max_size == 0 || max_size > limit) {
    max_size = limit;
  }
  return (max_size + seg_size - 1) / seg_size * seg_size;
}

template <class E, MEMFLAGS F>
Stack<E, F>::Stack(size_t segment_size, size_t max_cache_size, size_t max_size):
  StackBase<F>(adjust_segment_size(segment_size), max_cache_size, max_size)
{
  reset(true);
}

template <class E, MEMFLAGS F>
void Stack<E, F>::push(E item)
{
  if (this->_cur_seg_size == this->_seg_size) {
    push_segment();
  }
  this->_cur_seg[this->_cur_seg_size] = item;
  ++this->_cur_seg_size;
}

template <class E, MEMFLAGS F>
E Stack<E, F>::pop()
{
  if (this->_cur_seg_size == 1) {
    E tmp = _cur_seg[--this->_cur_seg_size];
    pop_segment();
    return tmp;
  }
  return this->_cur_seg[--this->_cur_seg_size];
}

template <class E, MEMFLAGS F>
void Stack<E, F>::clear(bool clear_cache)
{
  free_segments(_cur_seg);
  if (clear_cache) free_segments(_cache);
  reset(clear_cache);
}

template <class E, MEMFLAGS F>
size_t Stack<E, F>::adjust_segment_size(size_t seg_size)
{
  const size_t elem_sz = sizeof(E);
  const size_t ptr_sz = sizeof(E*);
  if (elem_sz < ptr_sz) {
    return align_up(seg_size * elem_sz, ptr_sz) / elem_sz;
  }
  return seg_size;
}

template <class E, MEMFLAGS F>
size_t Stack<E, F>::link_offset() const
{
  return align_up(this->_seg_size * sizeof(E), sizeof(E*));
}

template <class E, MEMFLAGS F>
size_t Stack<E, F>::segment_bytes() const
{
  return link_offset() + sizeof(E*);
}

template <class E, MEMFLAGS F>
E** Stack<E, F>::link_addr(E* seg) const
{
  return (E**) ((char*)seg + link_offset());
}

template <class E, MEMFLAGS F>
E* Stack<E, F>::get_link(E* seg) const
{
  return *link_addr(seg);
}

template <class E, MEMFLAGS F>
E* Stack<E, F>::set_link(E* new_seg, E* old_seg)
{
  *link_addr(new_seg) = old_seg;
  return new_seg;
}

template <class E, MEMFLAGS F>
E* Stack<E, F>::alloc(size_t bytes)
{
  return (E*) NEW_C_HEAP_ARRAY(char, bytes, F);
}

template <class E, MEMFLAGS F>
void Stack<E, F>::free(E* addr, size_t bytes)
{
  FREE_C_HEAP_ARRAY(char, (char*) addr);
}

// Stack is used by the GC code and in some hot paths a lot of the Stack
// code gets inlined. This is generally good, but when too much code has
// been inlined, no further inlining is allowed by GCC. Therefore we need
// to prevent parts of the slow path in Stack to be inlined to allow other
// code to be.
template <class E, MEMFLAGS F>
NOINLINE void Stack<E, F>::push_segment()
{
  E* next;
  if (this->_cache_size > 0) {
    // Use a cached segment.
    next = _cache;
    _cache = get_link(_cache);
    --this->_cache_size;
  } else {
    next = alloc(segment_bytes());
  }
  const bool at_empty_transition = is_empty();
  this->_cur_seg = set_link(next, _cur_seg);
  this->_cur_seg_size = 0;
  this->_full_seg_size += at_empty_transition ? 0 : this->_seg_size;
}

template <class E, MEMFLAGS F>
void Stack<E, F>::pop_segment()
{
  E* const prev = get_link(_cur_seg);
  if (this->_cache_size < this->_max_cache_size) {
    // Add the current segment to the cache.
    _cache = set_link(_cur_seg, _cache);
    ++this->_cache_size;
  } else {
    free(_cur_seg, segment_bytes());
  }
  const bool at_empty_transition = prev == NULL;
  this->_cur_seg = prev;
  this->_cur_seg_size = this->_seg_size;
  this->_full_seg_size -= at_empty_transition ? 0 : this->_seg_size;
}

template <class E, MEMFLAGS F>
void Stack<E, F>::free_segments(E* seg)
{
  const size_t bytes = segment_bytes();
  while (seg != NULL) {
    E* const prev = get_link(seg);
    free(seg, bytes);
    seg = prev;
  }
}

template <class E, MEMFLAGS F>
void Stack<E, F>::reset(bool reset_cache)
{
  this->_cur_seg_size = this->_seg_size; // So push() will alloc a new segment.
  this->_full_seg_size = 0;
  _cur_seg = NULL;
  if (reset_cache) {
    this->_cache_size = 0;
    _cache = NULL;
  }
}

template <class E, MEMFLAGS F>
E* ResourceStack<E, F>::alloc(size_t bytes)
{
  return (E*) resource_allocate_bytes(bytes);
}

template <class E, MEMFLAGS F>
void ResourceStack<E, F>::free(E* addr, size_t bytes)
{
  resource_free_bytes((char*) addr, bytes);
}

template <class E, MEMFLAGS F>
void StackIterator<E, F>::sync()
{
  _full_seg_size = _stack._full_seg_size;
  _cur_seg_size = _stack._cur_seg_size;
  _cur_seg = _stack._cur_seg;
}

template <class E, MEMFLAGS F>
E* StackIterator<E, F>::next_addr()
{
  if (_cur_seg_size == 1) {
    E* addr = _cur_seg;
    _cur_seg = _stack.get_link(_cur_seg);
    _cur_seg_size = _stack.segment_size();
    _full_seg_size -= _stack.segment_size();
    return addr;
  }
  return _cur_seg + --_cur_seg_size;
}

#endif

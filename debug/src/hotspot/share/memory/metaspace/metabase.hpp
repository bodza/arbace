#ifndef SHARE_MEMORY_METASPACE_METABASE_HPP
#define SHARE_MEMORY_METASPACE_METABASE_HPP

#include "utilities/globalDefinitions.hpp"

namespace metaspace {
// Super class of Metablock and Metachunk to allow them to
// be put on the FreeList and in the BinaryTreeDictionary.
template <class T>
class Metabase {
  size_t _word_size;
  T*     _next;
  T*     _prev;

 protected:
  Metabase(size_t word_size) : _word_size(word_size), _next(NULL), _prev(NULL) { }

 public:
  T* next() const         { return _next; }
  T* prev() const         { return _prev; }
  void set_next(T* v)     { _next = v; }
  void set_prev(T* v)     { _prev = v; }
  void clear_next()       { set_next(NULL); }
  void clear_prev()       { set_prev(NULL); }

  size_t size() const volatile { return _word_size; }
  void set_size(size_t v) { _word_size = v; }

  void link_next(T* ptr)  { set_next(ptr); }
  void link_prev(T* ptr)  { set_prev(ptr); }
  void link_after(T* ptr) {
    link_next(ptr);
    if (ptr != NULL) ptr->link_prev((T*)this);
  }

  uintptr_t* end() const        { return ((uintptr_t*) this) + size(); }

  bool cantCoalesce() const     { return false; }

  // Debug support
  bool verify_chunk_in_free_list(T* tc) const { return true; }
  bool verify_par_locked() { return true; }

  void assert_is_mangled() const { }

  bool is_free()                 { return true; }
};
}

#endif

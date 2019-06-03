#ifndef SHARE_VM_OOPS_ARRAY_HPP
#define SHARE_VM_OOPS_ARRAY_HPP

#include "memory/allocation.hpp"
#include "memory/metaspace.hpp"
#include "utilities/align.hpp"

// Array for metadata allocation

template <typename T>
class Array: public MetaspaceObj {
  friend class MetadataFactory;
  friend class MetaspaceShared;
  friend class VMStructs;
  friend class JVMCIVMStructs;
  friend class MethodHandleCompiler;           // special case
  friend class WhiteBox;
protected:
  int _length;                                 // the number of array elements
  T   _data[1];                                // the array memory

  void initialize(int length) {
    _length = length;
  }

 private:
  // Turn off copy constructor and assignment operator.
  Array(const Array<T>&);
  void operator=(const Array<T>&);

  void* operator new(size_t size, ClassLoaderData* loader_data, int length, TRAPS) throw() {
    size_t word_size = Array::size(length);
    return (void*) Metaspace::allocate(loader_data, word_size, MetaspaceObj::array_type(sizeof(T)), THREAD);
  }

  static size_t byte_sizeof(int length, size_t elm_byte_size) {
    return sizeof(Array<T>) + MAX2(length - 1, 0) * elm_byte_size;
  }
  static size_t byte_sizeof(int length) { return byte_sizeof(length, sizeof(T)); }

  // WhiteBox API helper.
  // Can't distinguish between array of length 0 and length 1,
  // will always return 0 in those cases.
  static int bytes_to_length(size_t bytes) {
    if (sizeof(Array<T>) >= bytes) {
      return 0;
    }

    size_t left = bytes - sizeof(Array<T>);
    size_t elements = left / sizeof(T);
    int length = (int)elements;
    return length;
  }

  explicit Array(int length) : _length(length) { }

  Array(int length, T init) : _length(length) {
    for (int i = 0; i < length; i++) {
      _data[i] = init;
    }
  }

 public:
  // standard operations
  int  length() const                 { return _length; }
  T* data()                           { return _data; }
  bool is_empty() const               { return length() == 0; }

  int index_of(const T& x) const {
    int i = length();
    while (i-- > 0 && _data[i] != x)
        ;
    return i;
  }

  // sort the array.
  bool contains(const T& x) const      { return index_of(x) >= 0; }

  T    at(int i) const                 { return _data[i]; }
  void at_put(const int i, const T& x) { _data[i] = x; }
  T*   adr_at(const int i)             { return &_data[i]; }
  int  find(const T& x)                { return index_of(x); }

  T at_acquire(const int which);
  void release_at_put(int which, T contents);

  static int size(int length) {
    size_t bytes = align_up(byte_sizeof(length), BytesPerWord);
    size_t words = bytes / BytesPerWord;

    return (int)words;
  }
  int size() {
    return size(_length);
  }

  static int length_offset_in_bytes() { return (int) (offset_of(Array<T>, _length)); }
  // Note, this offset don't have to be wordSize aligned.
  static int base_offset_in_bytes() { return (int) (offset_of(Array<T>, _data)); };

  // FIXME: How to handle this?
  void print_value_on(outputStream* st) const {
    st->print("Array<T>(" INTPTR_FORMAT ")", p2i(this));
  }
};

#endif

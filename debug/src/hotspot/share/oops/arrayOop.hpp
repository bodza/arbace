#ifndef SHARE_VM_OOPS_ARRAYOOP_HPP
#define SHARE_VM_OOPS_ARRAYOOP_HPP

#include "memory/universe.hpp"
#include "oops/oop.hpp"
#include "utilities/align.hpp"

// arrayOopDesc is the abstract baseclass for all arrays.  It doesn't
// declare pure virtual to enforce this because that would allocate a vtbl
// in each instance, which we don't want.

// The layout of array Oops is:
//
//  markOop
//  Klass*    // 32 bits if compressed but declared 64 in LP64.
//  length    // shares klass memory or allocated after declared fields.

class arrayOopDesc : public oopDesc {
  friend class VMStructs;
  friend class arrayOopDescTest;

  // Interpreter/Compiler offsets

  // Header size computation.
  // The header is considered the oop part of this type plus the length.
  // Returns the aligned header_size_in_bytes.  This is not equivalent to
  // sizeof(arrayOopDesc) which should not appear in the code.
  static int header_size_in_bytes() {
    size_t hs = align_up(length_offset_in_bytes() + sizeof(int),
                              HeapWordSize);
    return (int)hs;
  }

  // Check whether an element of a typeArrayOop with the given type must be
  // aligned 0 mod 8.  The typeArrayOop itself must be aligned at least this
  // strongly.
  static bool element_type_should_be_aligned(BasicType type) {
    return type == T_DOUBLE || type == T_LONG;
  }

 public:
  // The _length field is not declared in C++.  It is allocated after the
  // declared nonstatic fields in arrayOopDesc if not compressed, otherwise
  // it occupies the second half of the _klass field in oopDesc.
  static int length_offset_in_bytes() {
    return UseCompressedClassPointers ? klass_gap_offset_in_bytes() :
                               sizeof(arrayOopDesc);
  }

  // Returns the offset of the first element.
  static int base_offset_in_bytes(BasicType type) {
    return header_size(type) * HeapWordSize;
  }

  // Returns the address of the first element. The elements in the array will not
  // relocate from this address until a subsequent thread transition.
  inline void* base(BasicType type) const;
  inline void* base_raw(BasicType type) const; // GC barrier invariant

  template <typename T>
  static T* obj_offset_to_raw(arrayOop obj, size_t offset_in_bytes, T* raw) {
    if (obj != NULL) {
      char* base = reinterpret_cast<char*>((void*) obj);
      raw = reinterpret_cast<T*>(base + offset_in_bytes);
    } else {
    }
    return raw;
  }

  // Tells whether index is within bounds.
  bool is_within_bounds(int index) const        { return 0 <= index && index < length(); }

  // Accessors for instance variable which is not a C++ declared nonstatic
  // field.
  int length() const {
    return *(int*)(((intptr_t)this) + length_offset_in_bytes());
  }
  void set_length(int length) {
    set_length((HeapWord*)this, length);
  }
  static void set_length(HeapWord* mem, int length) {
    *(int*)(((char*)mem) + length_offset_in_bytes()) = length;
  }

  // Should only be called with constants as argument
  // (will not constant fold otherwise)
  // Returns the header size in words aligned to the requirements of the
  // array object type.
  static int header_size(BasicType type) {
    size_t typesize_in_bytes = header_size_in_bytes();
    return (int)(element_type_should_be_aligned(type)
      ? align_object_offset(typesize_in_bytes/HeapWordSize)
      : typesize_in_bytes/HeapWordSize);
  }

  // Return the maximum length of an array of BasicType.  The length can passed
  // to typeArrayOop::object_size(scale, length, header_size) without causing an
  // overflow. We also need to make sure that this will not overflow a size_t on
  // 32 bit platforms when we convert it to a byte size.
  static int32_t max_array_length(BasicType type) {

    const size_t max_element_words_per_size_t = align_down((SIZE_MAX/HeapWordSize - header_size(type)), MinObjAlignment);
    const size_t max_elements_per_size_t = HeapWordSize * max_element_words_per_size_t / type2aelembytes(type);
    if ((size_t)max_jint < max_elements_per_size_t) {
      // It should be ok to return max_jint here, but parts of the code
      // (CollectedHeap, Klass::oop_oop_iterate(), and more) uses an int for
      // passing around the size (in words) of an object. So, we need to avoid
      // overflowing an int when we add the header. See CRs 4718400 and 7110613.
      return align_down(max_jint - header_size(type), MinObjAlignment);
    }
    return (int32_t)max_elements_per_size_t;
  }
};

#endif

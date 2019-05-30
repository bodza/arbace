#ifndef SHARE_VM_OOPS_OBJARRAYOOP_HPP
#define SHARE_VM_OOPS_OBJARRAYOOP_HPP

#include "oops/arrayOop.hpp"
#include "utilities/align.hpp"

class Klass;

// An objArrayOop is an array containing oops.
// Evaluating "String arg[10]" will create an objArrayOop.

class objArrayOopDesc : public arrayOopDesc {
  friend class ObjArrayKlass;
  friend class Runtime1;
  friend class psPromotionManager;
  friend class CSetMarkOopClosure;
  friend class G1ParScanPartialArrayClosure;

  template <class T> T* obj_at_addr(int index) const;
  template <class T> T* obj_at_addr_raw(int index) const;

  template <class T>
  static ptrdiff_t obj_at_offset(int index) {
    return base_offset_in_bytes() + sizeof(T) * index;
  }

private:
  // Give size of objArrayOop in HeapWords minus the header
  static int array_size(int length) {
    const uint OopsPerHeapWord = HeapWordSize/heapOopSize;
    assert(OopsPerHeapWord >= 1 && (HeapWordSize % heapOopSize == 0), "Else the following (new) computation would be in error");
    uint res = ((uint)length + OopsPerHeapWord - 1)/OopsPerHeapWord;
    return res;
  }

 public:
  // Returns the offset of the first element.
  static int base_offset_in_bytes() {
    return arrayOopDesc::base_offset_in_bytes(T_OBJECT);
  }

  // base is the address following the header.
  HeapWord* base() const;
  HeapWord* base_raw() const;

  // Accessing
  oop obj_at(int index) const;

  void obj_at_put(int index, oop value);

  oop atomic_compare_exchange_oop(int index, oop exchange_value, oop compare_value);

  // Sizing
  static int header_size()    { return arrayOopDesc::header_size(T_OBJECT); }
  int object_size()           { return object_size(length()); }

  static int object_size(int length) {
    // This returns the object size in HeapWords.
    uint asz = array_size(length);
    uint osz = align_object_size(header_size() + asz);
    assert(osz >= asz,   "no overflow");
    assert((int)osz > 0, "no overflow");
    return (int)osz;
  }

  Klass* element_klass();

public:
  // special iterators for index ranges, returns size of object
  template <typename OopClosureType>
  void oop_iterate_range(OopClosureType* blk, int start, int end);
};

#endif

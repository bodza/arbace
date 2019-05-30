#ifndef SHARE_VM_OOPS_FIELDINFO_HPP
#define SHARE_VM_OOPS_FIELDINFO_HPP

#include "oops/constantPool.hpp"
#include "oops/typeArrayOop.hpp"
#include "classfile/vmSymbols.hpp"

// This class represents the field information contained in the fields
// array of an InstanceKlass.  Currently it's laid on top an array of
// Java shorts but in the future it could simply be used as a real
// array type.  FieldInfo generally shouldn't be used directly.
// Fields should be queried either through InstanceKlass or through
// the various FieldStreams.
class FieldInfo {
  friend class fieldDescriptor;
  friend class JavaFieldStream;
  friend class ClassFileParser;

 public:
  // fields
  // Field info extracted from the class file and stored
  // as an array of 6 shorts.

#define FIELDINFO_TAG_SIZE             2
#define FIELDINFO_TAG_BLANK            0
#define FIELDINFO_TAG_OFFSET           1
#define FIELDINFO_TAG_TYPE_PLAIN       2
#define FIELDINFO_TAG_TYPE_CONTENDED   3
#define FIELDINFO_TAG_MASK             3

  // Packed field has the tag, and can be either of:
  //    hi bits <--------------------------- lo bits
  //   |---------high---------|---------low---------|
  //    ..........................................00  - blank
  //    [------------------offset----------------]01  - real field offset
  //    ......................[-------type-------]10  - plain field with type
  //    [--contention_group--][-------type-------]11  - contended field with type and contention group
  enum FieldOffset {
    access_flags_offset      = 0,
    name_index_offset        = 1,
    signature_index_offset   = 2,
    initval_index_offset     = 3,
    low_packed_offset        = 4,
    high_packed_offset       = 5,
    field_slots              = 6
  };

 private:
  u2 _shorts[field_slots];

  void set_name_index(u2 val)                    { _shorts[name_index_offset] = val;         }
  void set_signature_index(u2 val)               { _shorts[signature_index_offset] = val;    }
  void set_initval_index(u2 val)                 { _shorts[initval_index_offset] = val;      }

  u2 name_index() const                          { return _shorts[name_index_offset];        }
  u2 signature_index() const                     { return _shorts[signature_index_offset];   }
  u2 initval_index() const                       { return _shorts[initval_index_offset];     }

 public:
  static FieldInfo* from_field_array(Array<u2>* fields, int index) {
    return ((FieldInfo*)fields->adr_at(index * field_slots));
  }
  static FieldInfo* from_field_array(u2* fields, int index) {
    return ((FieldInfo*)(fields + index * field_slots));
  }

  void initialize(u2 access_flags,
                  u2 name_index,
                  u2 signature_index,
                  u2 initval_index) {
    _shorts[access_flags_offset] = access_flags;
    _shorts[name_index_offset] = name_index;
    _shorts[signature_index_offset] = signature_index;
    _shorts[initval_index_offset] = initval_index;
    _shorts[low_packed_offset] = 0;
    _shorts[high_packed_offset] = 0;
  }

  u2 access_flags() const                        { return _shorts[access_flags_offset];            }
  u4 offset() const {
    u2 lo = _shorts[low_packed_offset];
    switch(lo & FIELDINFO_TAG_MASK) {
      case FIELDINFO_TAG_OFFSET:
        return build_int_from_shorts(_shorts[low_packed_offset], _shorts[high_packed_offset]) >> FIELDINFO_TAG_SIZE;
    }
    ShouldNotReachHere();
    return 0;
  }

  bool is_contended() const {
    u2 lo = _shorts[low_packed_offset];
    switch(lo & FIELDINFO_TAG_MASK) {
      case FIELDINFO_TAG_TYPE_PLAIN:
        return false;
      case FIELDINFO_TAG_TYPE_CONTENDED:
        return true;
    }
    ShouldNotReachHere();
    return false;
  }

  u2 contended_group() const {
    u2 lo = _shorts[low_packed_offset];
    switch(lo & FIELDINFO_TAG_MASK) {
      case FIELDINFO_TAG_TYPE_PLAIN:
        return 0;
      case FIELDINFO_TAG_TYPE_CONTENDED:
        return _shorts[high_packed_offset];
    }
    ShouldNotReachHere();
    return 0;
 }

  u2 allocation_type() const {
    u2 lo = _shorts[low_packed_offset];
    switch(lo & FIELDINFO_TAG_MASK) {
      case FIELDINFO_TAG_TYPE_PLAIN:
      case FIELDINFO_TAG_TYPE_CONTENDED:
        return (lo >> FIELDINFO_TAG_SIZE);
    }
    ShouldNotReachHere();
    return 0;
  }

  bool is_offset_set() const {
    return (_shorts[low_packed_offset] & FIELDINFO_TAG_MASK) == FIELDINFO_TAG_OFFSET;
  }

  Symbol* name(const constantPoolHandle& cp) const {
    int index = name_index();
    if (is_internal()) {
      return lookup_symbol(index);
    }
    return cp->symbol_at(index);
  }

  Symbol* signature(const constantPoolHandle& cp) const {
    int index = signature_index();
    if (is_internal()) {
      return lookup_symbol(index);
    }
    return cp->symbol_at(index);
  }

  void set_access_flags(u2 val)                  { _shorts[access_flags_offset] = val;             }
  void set_offset(u4 val)                        {
    val = val << FIELDINFO_TAG_SIZE; // make room for tag
    _shorts[low_packed_offset] = extract_low_short_from_int(val) | FIELDINFO_TAG_OFFSET;
    _shorts[high_packed_offset] = extract_high_short_from_int(val);
  }

  void set_allocation_type(int type) {
    u2 lo = _shorts[low_packed_offset];
    switch(lo & FIELDINFO_TAG_MASK) {
      case FIELDINFO_TAG_BLANK:
        _shorts[low_packed_offset] = ((type << FIELDINFO_TAG_SIZE)) & 0xFFFF;
        _shorts[low_packed_offset] &= ~FIELDINFO_TAG_MASK;
        _shorts[low_packed_offset] |= FIELDINFO_TAG_TYPE_PLAIN;
        return;
    }
    ShouldNotReachHere();
  }

  void set_contended_group(u2 val) {
    u2 lo = _shorts[low_packed_offset];
    switch(lo & FIELDINFO_TAG_MASK) {
      case FIELDINFO_TAG_TYPE_PLAIN:
        _shorts[low_packed_offset] |= FIELDINFO_TAG_TYPE_CONTENDED;
        _shorts[high_packed_offset] = val;
        return;
    }
    ShouldNotReachHere();
  }

  bool is_internal() const {
    return (access_flags() & JVM_ACC_FIELD_INTERNAL) != 0;
  }

  bool is_stable() const {
    return (access_flags() & JVM_ACC_FIELD_STABLE) != 0;
  }
  void set_stable(bool z) {
    if (z) _shorts[access_flags_offset] |=  JVM_ACC_FIELD_STABLE;
    else   _shorts[access_flags_offset] &= ~JVM_ACC_FIELD_STABLE;
  }

  Symbol* lookup_symbol(int symbol_index) const {
    assert(is_internal(), "only internal fields");
    return vmSymbols::symbol_at((vmSymbols::SID)symbol_index);
  }
};

#endif

#ifndef SHARE_VM_RUNTIME_JFIELDIDWORKAROUND_HPP
#define SHARE_VM_RUNTIME_JFIELDIDWORKAROUND_HPP

class jfieldIDWorkaround: AllStatic {
  // This workaround is because JVMTI doesn't have distinct entry points
  // for methods that use static jfieldIDs and instance jfieldIDs.
  // The workaround is to steal a low-order bit:
  //   a 1 means the jfieldID is an instance jfieldID,
  //             and the rest of the word is the offset of the field.
  //   a 0 means the jfieldID is a static jfieldID,
  //             and the rest of the word is the JNIid*.
  //
  // Another low-order bit is used to mark if an instance field
  // is accompanied by an indication of which class it applies to.
  //
  // Bit-format of a jfieldID (most significant first):
  //  address:30        instance=0:1 checked=0:1
  //  offset:30         instance=1:1 checked=0:1
  //  klass:23 offset:7 instance=1:1 checked=1:1
  //
  // If the offset does not fit in 7 bits, or if the fieldID is
  // not checked, then the checked bit is zero and the rest of
  // the word (30 bits) contains only the offset.
  //
 private:
  enum {
    checked_bits           = 1,
    instance_bits          = 1,
    address_bits           = BitsPerWord - checked_bits - instance_bits,

    large_offset_bits      = address_bits,  // unioned with address
    small_offset_bits      = 7,
    klass_bits             = address_bits - small_offset_bits,

    checked_shift          = 0,
    instance_shift         = checked_shift  + checked_bits,
    address_shift          = instance_shift + instance_bits,

    offset_shift           = address_shift,  // unioned with address
    klass_shift            = offset_shift + small_offset_bits,

    checked_mask_in_place  = right_n_bits(checked_bits)  << checked_shift,
    instance_mask_in_place = right_n_bits(instance_bits) << instance_shift,
    large_offset_mask      = right_n_bits(large_offset_bits),
    small_offset_mask      = right_n_bits(small_offset_bits),
    klass_mask             = right_n_bits(klass_bits)
    };

  // helper routines:
  static bool is_checked_jfieldID(jfieldID id) {
    uintptr_t as_uint = (uintptr_t) id;
    return ((as_uint & checked_mask_in_place) != 0);
  }
  static intptr_t raw_instance_offset(jfieldID id) {
    uintptr_t result = (uintptr_t) id >> address_shift;
    return (intptr_t)result;
  }
  static intptr_t encode_klass_hash(Klass* k, intptr_t offset);
  static bool klass_hash_ok(Klass* k, jfieldID id);
  static void verify_instance_jfieldID(Klass* k, jfieldID id);

 public:
  static bool is_valid_jfieldID(Klass* k, jfieldID id);

  static bool is_instance_jfieldID(Klass* k, jfieldID id) {
    uintptr_t as_uint = (uintptr_t) id;
    return ((as_uint & instance_mask_in_place) != 0);
  }
  static bool is_static_jfieldID(jfieldID id) {
    uintptr_t as_uint = (uintptr_t) id;
    return ((as_uint & instance_mask_in_place) == 0);
  }

  static jfieldID to_instance_jfieldID(Klass* k, int offset) {
    intptr_t as_uint = ((offset & large_offset_mask) << offset_shift) | instance_mask_in_place;
    return (jfieldID) as_uint;
  }

  static intptr_t from_instance_jfieldID(Klass* k, jfieldID id) {
    return raw_instance_offset(id);
  }

  static jfieldID to_static_jfieldID(JNIid* id) {
    jfieldID result = (jfieldID) id;
    return result;
  }

  static JNIid* from_static_jfieldID(jfieldID id) {
    JNIid* result = (JNIid*) id;
    return result;
  }

  static jfieldID to_jfieldID(InstanceKlass* k, int offset, bool is_static) {
    if (is_static) {
      JNIid *id = k->jni_id_for(offset);
      return jfieldIDWorkaround::to_static_jfieldID(id);
    } else {
      return jfieldIDWorkaround::to_instance_jfieldID(k, offset);
    }
  }
};

#endif

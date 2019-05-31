#ifndef SHARE_VM_CI_CICONSTANT_HPP
#define SHARE_VM_CI_CICONSTANT_HPP

#include "ci/ciClassList.hpp"
#include "ci/ciNullObject.hpp"

// ciConstant
//
// This class represents a constant value.
class ciConstant {
  friend class VMStructs;
private:
  friend class ciEnv;
  friend class ciField;

  BasicType _type;
  union {
    jint      _int;
    jlong     _long;
    jfloat    _float;
    jdouble   _double;
    ciObject* _object;
  } _value;

public:

  ciConstant() {
    _type = T_ILLEGAL; _value._long = -1;
  }
  ciConstant(BasicType type, jint value) {
    _type = type; _value._int = value;
  }
  ciConstant(jlong value) {
    _type = T_LONG; _value._long = value;
  }
  ciConstant(jfloat value) {
    _type = T_FLOAT; _value._float = value;
  }
  ciConstant(jdouble value) {
    _type = T_DOUBLE; _value._double = value;
  }
  ciConstant(BasicType type, ciObject* p) {
    _type = type; _value._object = p;
  }

  BasicType basic_type() const { return _type; }

  jboolean  as_boolean() {
    return (jboolean)_value._int;
  }
  jchar     as_char() {
    return (jchar)_value._int;
  }
  jbyte     as_byte() {
    return (jbyte)_value._int;
  }
  jshort    as_short() {
    return (jshort)_value._int;
  }
  jint      as_int() {
    return _value._int;
  }
  jlong     as_long() {
    return _value._long;
  }
  jfloat    as_float() {
    return _value._float;
  }
  jdouble   as_double() {
    return _value._double;
  }
  ciObject* as_object() const {
    return _value._object;
  }

  bool      is_null_or_zero() const {
    if (!is_java_primitive(basic_type())) {
      return as_object()->is_null_object();
    } else if (type2size[basic_type()] == 1) {
      // treat float bits as int, to avoid comparison with -0 and NaN
      return (_value._int == 0);
    } else if (type2size[basic_type()] == 2) {
      // treat double bits as long, to avoid comparison with -0 and NaN
      return (_value._long == 0);
    } else {
      return false;
    }
  }

  bool is_valid() const {
    return basic_type() != T_ILLEGAL;
  }
  // Debugging output
  void print();
};

#endif

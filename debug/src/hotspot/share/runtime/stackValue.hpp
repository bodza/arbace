#ifndef SHARE_VM_RUNTIME_STACKVALUE_HPP
#define SHARE_VM_RUNTIME_STACKVALUE_HPP

#include "code/location.hpp"
#include "runtime/handles.hpp"

class StackValue : public ResourceObj {
 private:
  BasicType _type;
  intptr_t  _integer_value; // Blank java stack slot value
  Handle    _handle_value;  // Java stack slot value interpreted as a Handle
 public:
  StackValue(intptr_t value) {
    _type              = T_INT;
    _integer_value     = value;
  }

  StackValue(Handle value, intptr_t scalar_replaced = 0) {
    _type                = T_OBJECT;
    _integer_value       = scalar_replaced;
    _handle_value        = value;
  }

  StackValue() {
    _type           = T_CONFLICT;
    _integer_value  = 0;
  }

  // Only used during deopt- preserve object type.
  StackValue(intptr_t o, BasicType t) {
    _type          = t;
    _integer_value = o;
  }

  Handle get_obj() const {
    return _handle_value;
  }

  bool obj_is_scalar_replaced() const {
    return _integer_value != 0;
  }

  void set_obj(Handle value) {
    _handle_value = value;
  }

  intptr_t get_int() const {
    return _integer_value;
  }

  // For special case in deopt.
  intptr_t get_int(BasicType t) const {
    return _integer_value;
  }

  void set_int(intptr_t value) {
    _integer_value = value;
  }

  BasicType type() const { return  _type; }

  bool equal(StackValue *value) {
    if (_type != value->_type) return false;
    if (_type == T_OBJECT)
      return (_handle_value == value->_handle_value);
    else {
      // [phh] compare only low addressed portions of intptr_t slots
      return (*(int *)&_integer_value == *(int *)&value->_integer_value);
    }
  }

  static StackValue* create_stack_value(const frame* fr, const RegisterMap* reg_map, ScopeValue* sv);
  static BasicLock*  resolve_monitor_lock(const frame* fr, Location location);
};

#endif

#include "precompiled.hpp"
#include "ci/ciConstant.hpp"
#include "ci/ciUtilities.hpp"
#include "memory/allocation.hpp"
#include "memory/allocation.inline.hpp"

// ciConstant
//
// This class represents a constant value.

// ------------------------------------------------------------------
// ciConstant::print
void ciConstant::print() {
  tty->print("<ciConstant type=%s value=",
             basictype_to_str(basic_type()));
  switch (basic_type()) {
  case T_BOOLEAN:
    tty->print("%s", bool_to_str(_value._int != 0));
    break;
  case T_CHAR:
  case T_BYTE:
  case T_SHORT:
  case T_INT:
    tty->print("%d", _value._int);
    break;
  case T_LONG:
    tty->print(INT64_FORMAT, (int64_t)(_value._long));
    break;
  case T_FLOAT:
    tty->print("%f", _value._float);
    break;
  case T_DOUBLE:
    tty->print("%lf", _value._double);
    break;
  case T_OBJECT:
  case T_ARRAY:
    _value._object->print();
    break;
  default:
    tty->print("ILLEGAL");
    break;
  }
  tty->print(">");
}

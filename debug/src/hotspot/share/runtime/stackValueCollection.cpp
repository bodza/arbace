#include "precompiled.hpp"

#include "runtime/stackValueCollection.hpp"

jint StackValueCollection::int_at(int slot) const {
  intptr_t val =  at(slot)->get_int();
  jint ival = *((jint*) (&val));
  return ival;
}

jlong StackValueCollection::long_at(int slot) const {
  return at(slot + 1)->get_int();
}

Handle StackValueCollection::obj_at(int slot) const {
  return at(slot)->get_obj();
}

jfloat StackValueCollection::float_at(int slot) const {
  intptr_t res = at(slot)->get_int();
  return *((jfloat*) (&res));
}

jdouble StackValueCollection::double_at(int slot) const {
  intptr_t res = at(slot + 1)->get_int();
  return *((jdouble*) (&res));
}

void StackValueCollection::set_int_at(int slot, jint value) {
  intptr_t val;
  *((jint*) (&val)) = value;
  at(slot)->set_int(val);
}

void StackValueCollection::set_long_at(int slot, jlong value) {
  at(slot + 1)->set_int(value);
}

void StackValueCollection::set_obj_at(int slot, Handle value) {
  at(slot)->set_obj(value);
}

void StackValueCollection::set_float_at(int slot, jfloat value) {
  union {
    intptr_t jd;
    jint array[2];
  } val;
  val.array[0] = *(jint*)(&value);
  val.array[1] = 0;
  at(slot)->set_int(val.jd);
}

void StackValueCollection::set_double_at(int slot, jdouble value) {
  at(slot + 1)->set_int(*(intptr_t*)(&value));
}

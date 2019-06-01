#include "precompiled.hpp"

#include "code/debugInfo.hpp"
#include "code/debugInfoRec.hpp"
#include "code/nmethod.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/thread.hpp"

// Constructors

DebugInfoWriteStream::DebugInfoWriteStream(DebugInformationRecorder* recorder, int initial_size)
: CompressedWriteStream(initial_size) {
  _recorder = recorder;
}

// Serializing oops

void DebugInfoWriteStream::write_handle(jobject h) {
  write_int(recorder()->oop_recorder()->find_index(h));
}

void DebugInfoWriteStream::write_metadata(Metadata* h) {
  write_int(recorder()->oop_recorder()->find_index(h));
}

oop DebugInfoReadStream::read_oop() {
  oop o = code()->oop_at(read_int());
  return o;
}

ScopeValue* DebugInfoReadStream::read_object_value() {
  int id = read_int();
  ObjectValue* result = new ObjectValue(id);
  // Cache the object since an object field could reference it.
  _obj_pool->push(result);
  result->read_object(this);
  return result;
}

ScopeValue* DebugInfoReadStream::get_cached_object() {
  int id = read_int();
  for (int i = _obj_pool->length() - 1; i >= 0; i--) {
    ObjectValue* ov = _obj_pool->at(i)->as_ObjectValue();
    if (ov->id() == id) {
      return ov;
    }
  }
  ShouldNotReachHere();
  return NULL;
}

// Serializing scope values

enum { LOCATION_CODE = 0, CONSTANT_INT_CODE = 1,  CONSTANT_OOP_CODE = 2,
                          CONSTANT_LONG_CODE = 3, CONSTANT_DOUBLE_CODE = 4,
                          OBJECT_CODE = 5,        OBJECT_ID_CODE = 6 };

ScopeValue* ScopeValue::read_from(DebugInfoReadStream* stream) {
  ScopeValue* result = NULL;
  switch(stream->read_int()) {
   case LOCATION_CODE:        result = new LocationValue(stream);        break;
   case CONSTANT_INT_CODE:    result = new ConstantIntValue(stream);     break;
   case CONSTANT_OOP_CODE:    result = new ConstantOopReadValue(stream); break;
   case CONSTANT_LONG_CODE:   result = new ConstantLongValue(stream);    break;
   case CONSTANT_DOUBLE_CODE: result = new ConstantDoubleValue(stream);  break;
   case OBJECT_CODE:          result = stream->read_object_value();      break;
   case OBJECT_ID_CODE:       result = stream->get_cached_object();      break;
   default: ShouldNotReachHere();
  }
  return result;
}

// LocationValue

LocationValue::LocationValue(DebugInfoReadStream* stream) {
  _location = Location(stream);
}

void LocationValue::write_on(DebugInfoWriteStream* stream) {
  stream->write_int(LOCATION_CODE);
  location().write_on(stream);
}

void LocationValue::print_on(outputStream* st) const {
  location().print_on(st);
}

// ObjectValue

void ObjectValue::set_value(oop value) {
  _value = Handle(Thread::current(), value);
}

void ObjectValue::read_object(DebugInfoReadStream* stream) {
  _klass = read_from(stream);
  int length = stream->read_int();
  for (int i = 0; i < length; i++) {
    ScopeValue* val = read_from(stream);
    _field_values.append(val);
  }
}

void ObjectValue::write_on(DebugInfoWriteStream* stream) {
  if (_visited) {
    stream->write_int(OBJECT_ID_CODE);
    stream->write_int(_id);
  } else {
    _visited = true;
    stream->write_int(OBJECT_CODE);
    stream->write_int(_id);
    _klass->write_on(stream);
    int length = _field_values.length();
    stream->write_int(length);
    for (int i = 0; i < length; i++) {
      _field_values.at(i)->write_on(stream);
    }
  }
}

void ObjectValue::print_on(outputStream* st) const {
  st->print("obj[%d]", _id);
}

void ObjectValue::print_fields_on(outputStream* st) const { }

// ConstantIntValue

ConstantIntValue::ConstantIntValue(DebugInfoReadStream* stream) {
  _value = stream->read_signed_int();
}

void ConstantIntValue::write_on(DebugInfoWriteStream* stream) {
  stream->write_int(CONSTANT_INT_CODE);
  stream->write_signed_int(value());
}

void ConstantIntValue::print_on(outputStream* st) const {
  st->print("%d", value());
}

// ConstantLongValue

ConstantLongValue::ConstantLongValue(DebugInfoReadStream* stream) {
  _value = stream->read_long();
}

void ConstantLongValue::write_on(DebugInfoWriteStream* stream) {
  stream->write_int(CONSTANT_LONG_CODE);
  stream->write_long(value());
}

void ConstantLongValue::print_on(outputStream* st) const {
  st->print(JLONG_FORMAT, value());
}

// ConstantDoubleValue

ConstantDoubleValue::ConstantDoubleValue(DebugInfoReadStream* stream) {
  _value = stream->read_double();
}

void ConstantDoubleValue::write_on(DebugInfoWriteStream* stream) {
  stream->write_int(CONSTANT_DOUBLE_CODE);
  stream->write_double(value());
}

void ConstantDoubleValue::print_on(outputStream* st) const {
  st->print("%f", value());
}

// ConstantOopWriteValue

void ConstantOopWriteValue::write_on(DebugInfoWriteStream* stream) {
  stream->write_int(CONSTANT_OOP_CODE);
  stream->write_handle(value());
}

void ConstantOopWriteValue::print_on(outputStream* st) const {
  // using ThreadInVMfromUnknown here since in case of JVMCI compiler,
  // thread is already in VM state.
  ThreadInVMfromUnknown tiv;
  JNIHandles::resolve(value())->print_value_on(st);
}

// ConstantOopReadValue

ConstantOopReadValue::ConstantOopReadValue(DebugInfoReadStream* stream) {
  _value = Handle(Thread::current(), stream->read_oop());
}

void ConstantOopReadValue::write_on(DebugInfoWriteStream* stream) {
  ShouldNotReachHere();
}

void ConstantOopReadValue::print_on(outputStream* st) const {
  value()()->print_value_on(st);
}

// MonitorValue

MonitorValue::MonitorValue(ScopeValue* owner, Location basic_lock, bool eliminated) {
  _owner       = owner;
  _basic_lock  = basic_lock;
  _eliminated  = eliminated;
}

MonitorValue::MonitorValue(DebugInfoReadStream* stream) {
  _basic_lock  = Location(stream);
  _owner       = ScopeValue::read_from(stream);
  _eliminated  = (stream->read_bool() != 0);
}

void MonitorValue::write_on(DebugInfoWriteStream* stream) {
  _basic_lock.write_on(stream);
  _owner->write_on(stream);
  stream->write_bool(_eliminated);
}

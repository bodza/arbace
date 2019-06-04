#include "precompiled.hpp"

#include "code/debugInfoRec.hpp"
#include "code/pcDesc.hpp"
#include "code/scopeDesc.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"

ScopeDesc::ScopeDesc(const CompiledMethod* code, int decode_offset, int obj_decode_offset, bool reexecute, bool rethrow_exception, bool return_oop) {
  _code          = code;
  _decode_offset = decode_offset;
  _objects       = decode_object_values(obj_decode_offset);
  _reexecute     = reexecute;
  _rethrow_exception = rethrow_exception;
  _return_oop    = return_oop;
  decode_body();
}

ScopeDesc::ScopeDesc(const CompiledMethod* code, int decode_offset, bool reexecute, bool rethrow_exception, bool return_oop) {
  _code          = code;
  _decode_offset = decode_offset;
  _objects       = decode_object_values(DebugInformationRecorder::serialized_null);
  _reexecute     = reexecute;
  _rethrow_exception = rethrow_exception;
  _return_oop    = return_oop;
  decode_body();
}

ScopeDesc::ScopeDesc(const ScopeDesc* parent) {
  _code          = parent->_code;
  _decode_offset = parent->_sender_decode_offset;
  _objects       = parent->_objects;
  _reexecute     = false; //reexecute only applies to the first scope
  _rethrow_exception = false;
  _return_oop    = false;
  decode_body();
}

void ScopeDesc::decode_body() {
  if (decode_offset() == DebugInformationRecorder::serialized_null) {
    // This is a sentinel record, which is only relevant to
    // approximate queries.  Decode a reasonable frame.
    _sender_decode_offset = DebugInformationRecorder::serialized_null;
    _method = _code->method();
    _bci = InvocationEntryBci;
    _locals_decode_offset = DebugInformationRecorder::serialized_null;
    _expressions_decode_offset = DebugInformationRecorder::serialized_null;
    _monitors_decode_offset = DebugInformationRecorder::serialized_null;
  } else {
    // decode header
    DebugInfoReadStream* stream  = stream_at(decode_offset());

    _sender_decode_offset = stream->read_int();
    _method = stream->read_method();
    _bci    = stream->read_bci();

    // decode offsets for body and sender
    _locals_decode_offset      = stream->read_int();
    _expressions_decode_offset = stream->read_int();
    _monitors_decode_offset    = stream->read_int();
  }
}

GrowableArray<ScopeValue*>* ScopeDesc::decode_scope_values(int decode_offset) {
  if (decode_offset == DebugInformationRecorder::serialized_null) return NULL;
  DebugInfoReadStream* stream = stream_at(decode_offset);
  int length = stream->read_int();
  GrowableArray<ScopeValue*>* result = new GrowableArray<ScopeValue*> (length);
  for (int index = 0; index < length; index++) {
    result->push(ScopeValue::read_from(stream));
  }
  return result;
}

GrowableArray<ScopeValue*>* ScopeDesc::decode_object_values(int decode_offset) {
  if (decode_offset == DebugInformationRecorder::serialized_null) return NULL;
  GrowableArray<ScopeValue*>* result = new GrowableArray<ScopeValue*>();
  DebugInfoReadStream* stream = new DebugInfoReadStream(_code, decode_offset, result);
  int length = stream->read_int();
  for (int index = 0; index < length; index++) {
    // Objects values are pushed to 'result' array during read so that
    // object's fields could reference it (OBJECT_ID_CODE).
    (void)ScopeValue::read_from(stream);
  }
  return result;
}

GrowableArray<MonitorValue*>* ScopeDesc::decode_monitor_values(int decode_offset) {
  if (decode_offset == DebugInformationRecorder::serialized_null) return NULL;
  DebugInfoReadStream* stream  = stream_at(decode_offset);
  int length = stream->read_int();
  GrowableArray<MonitorValue*>* result = new GrowableArray<MonitorValue*> (length);
  for (int index = 0; index < length; index++) {
    result->push(new MonitorValue(stream));
  }
  return result;
}

DebugInfoReadStream* ScopeDesc::stream_at(int decode_offset) const {
  return new DebugInfoReadStream(_code, decode_offset, _objects);
}

GrowableArray<ScopeValue*>* ScopeDesc::locals() {
  return decode_scope_values(_locals_decode_offset);
}

GrowableArray<ScopeValue*>* ScopeDesc::expressions() {
  return decode_scope_values(_expressions_decode_offset);
}

GrowableArray<MonitorValue*>* ScopeDesc::monitors() {
  return decode_monitor_values(_monitors_decode_offset);
}

GrowableArray<ScopeValue*>* ScopeDesc::objects() {
  return _objects;
}

bool ScopeDesc::is_top() const {
 return _sender_decode_offset == DebugInformationRecorder::serialized_null;
}

ScopeDesc* ScopeDesc::sender() const {
  if (is_top()) return NULL;
  return new ScopeDesc(this);
}

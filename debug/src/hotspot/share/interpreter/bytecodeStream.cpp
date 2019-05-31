#include "precompiled.hpp"
#include "interpreter/bytecodeStream.hpp"
#include "interpreter/bytecodes.hpp"
#include "runtime/handles.inline.hpp"

Bytecodes::Code RawBytecodeStream::raw_next_special(Bytecodes::Code code) {
  // set next bytecode position
  address bcp = RawBytecodeStream::bcp();
  address end = method()->code_base() + end_bci();
  int len = Bytecodes::raw_special_length_at(bcp, end);
  // Very large tableswitch or lookupswitch size can cause _next_bci to overflow.
  if (len <= 0 || (_bci > _end_bci - len) || (_bci - len >= _next_bci)) {
    code = Bytecodes::_illegal;
  } else {
    _next_bci += len;
    // set attributes
    _is_wide = false;
    // check for special (uncommon) cases
    if (code == Bytecodes::_wide) {
      if (bcp + 1 >= end) {
        code = Bytecodes::_illegal;
      } else {
        code = (Bytecodes::Code)bcp[1];
        _is_wide = true;
      }
    }
  }
  _raw_code = code;
  return code;
}

BaseBytecodeStream::BaseBytecodeStream(const methodHandle& method) : _method(method) {
  set_interval(0, _method->code_size());
  _is_raw = false;
}

#include "precompiled.hpp"

#include "classfile/stackMapFrame.hpp"
#include "classfile/verifier.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "runtime/handles.inline.hpp"
#include "utilities/globalDefinitions.hpp"

StackMapFrame::StackMapFrame(u2 max_locals, u2 max_stack, ClassVerifier* v) :
                      _offset(0), _locals_size(0), _stack_size(0),
                      _stack_mark(0), _flags(0), _max_locals(max_locals),
                      _max_stack(max_stack), _verifier(v) {
  Thread* thr = v->thread();
  _locals = NEW_RESOURCE_ARRAY_IN_THREAD(thr, VerificationType, max_locals);
  _stack = NEW_RESOURCE_ARRAY_IN_THREAD(thr, VerificationType, max_stack);
  int32_t i;
  for (i = 0; i < max_locals; i++) {
    _locals[i] = VerificationType::bogus_type();
  }
  for (i = 0; i < max_stack; i++) {
    _stack[i] = VerificationType::bogus_type();
  }
}

StackMapFrame* StackMapFrame::frame_in_exception_handler(u1 flags) {
  Thread* thr = _verifier->thread();
  VerificationType* stack = NEW_RESOURCE_ARRAY_IN_THREAD(thr, VerificationType, 1);
  StackMapFrame* frame = new StackMapFrame(_offset, flags, _locals_size, 0, _max_locals, _max_stack, _locals, stack, _verifier);
  return frame;
}

void StackMapFrame::initialize_object(VerificationType old_object, VerificationType new_object) {
  int32_t i;
  for (i = 0; i < _max_locals; i++) {
    if (_locals[i].equals(old_object)) {
      _locals[i] = new_object;
    }
  }
  for (i = 0; i < _stack_size; i++) {
    if (_stack[i].equals(old_object)) {
      _stack[i] = new_object;
    }
  }
  if (old_object == VerificationType::uninitialized_this_type()) {
    // "this" has been initialized - reset flags
    _flags = 0;
  }
}

VerificationType StackMapFrame::set_locals_from_arg(const methodHandle& m, VerificationType thisKlass, TRAPS) {
  SignatureStream ss(m->signature());
  int init_local_num = 0;
  if (!m->is_static()) {
    init_local_num++;
    // add one extra argument for instance method
    if (m->name() == vmSymbols::object_initializer_name() && thisKlass.name() != vmSymbols::java_lang_Object()) {
      _locals[0] = VerificationType::uninitialized_this_type();
      _flags |= FLAG_THIS_UNINIT;
    } else {
      _locals[0] = thisKlass;
    }
  }

  // local num may be greater than size of parameters because long/double occupies two slots
  while (!ss.at_return_type()) {
    init_local_num += _verifier->change_sig_to_verificationType(&ss, &_locals[init_local_num], CHECK_VERIFY_(verifier(), VerificationType::bogus_type()));
    ss.next();
  }
  _locals_size = init_local_num;

  switch (ss.type()) {
    case T_OBJECT:
    case T_ARRAY:
    {
      Symbol* sig = ss.as_symbol(CHECK_(VerificationType::bogus_type()));
      // Create another symbol to save as signature stream unreferences
      // this symbol.
      Symbol* sig_copy = verifier()->create_temporary_symbol(sig, 0, sig->utf8_length(), CHECK_(VerificationType::bogus_type()));
      return VerificationType::reference_type(sig_copy);
    }
    case T_INT:     return VerificationType::integer_type();
    case T_BYTE:    return VerificationType::byte_type();
    case T_CHAR:    return VerificationType::char_type();
    case T_SHORT:   return VerificationType::short_type();
    case T_BOOLEAN: return VerificationType::boolean_type();
    case T_FLOAT:   return VerificationType::float_type();
    case T_DOUBLE:  return VerificationType::double_type();
    case T_LONG:    return VerificationType::long_type();
    case T_VOID:    return VerificationType::bogus_type();
    default:
      ShouldNotReachHere();
  }
  return VerificationType::bogus_type();
}

void StackMapFrame::copy_locals(const StackMapFrame* src) {
  int32_t len = src->locals_size() < _locals_size ?
    src->locals_size() : _locals_size;
  for (int32_t i = 0; i < len; i++) {
    _locals[i] = src->locals()[i];
  }
}

void StackMapFrame::copy_stack(const StackMapFrame* src) {
  int32_t len = src->stack_size() < _stack_size ?
    src->stack_size() : _stack_size;
  for (int32_t i = 0; i < len; i++) {
    _stack[i] = src->stack()[i];
  }
}

// Returns the location of the first mismatch, or 'len' if there are no
// mismatches
int StackMapFrame::is_assignable_to(VerificationType* from, VerificationType* to, int32_t len, TRAPS) const {
  int32_t i = 0;
  for (i = 0; i < len; i++) {
    if (!to[i].is_assignable_from(from[i], verifier(), false, THREAD)) {
      break;
    }
  }
  return i;
}

bool StackMapFrame::is_assignable_to(const StackMapFrame* target, ErrorContext* ctx, TRAPS) const {
  if (_max_locals != target->max_locals()) {
    *ctx = ErrorContext::locals_size_mismatch(_offset, (StackMapFrame*)this, (StackMapFrame*)target);
    return false;
  }
  if (_stack_size != target->stack_size()) {
    *ctx = ErrorContext::stack_size_mismatch(_offset, (StackMapFrame*)this, (StackMapFrame*)target);
    return false;
  }
  // Only need to compare type elements up to target->locals() or target->stack().
  // The remaining type elements in this state can be ignored because they are
  // assignable to bogus type.
  int mismatch_loc;
  mismatch_loc = is_assignable_to(_locals, target->locals(), target->locals_size(), THREAD);
  if (mismatch_loc != target->locals_size()) {
    *ctx = ErrorContext::bad_type(target->offset(),
        TypeOrigin::local(mismatch_loc, (StackMapFrame*)this),
        TypeOrigin::sm_local(mismatch_loc, (StackMapFrame*)target));
    return false;
  }
  mismatch_loc = is_assignable_to(_stack, target->stack(), _stack_size, THREAD);
  if (mismatch_loc != _stack_size) {
    *ctx = ErrorContext::bad_type(target->offset(),
        TypeOrigin::stack(mismatch_loc, (StackMapFrame*)this),
        TypeOrigin::sm_stack(mismatch_loc, (StackMapFrame*)target));
    return false;
  }

  if ((_flags | target->flags()) == target->flags()) {
    return true;
  } else {
    *ctx = ErrorContext::bad_flags(target->offset(),
        (StackMapFrame*)this, (StackMapFrame*)target);
    return false;
  }
}

VerificationType StackMapFrame::pop_stack_ex(VerificationType type, TRAPS) {
  if (_stack_size <= 0) {
    verifier()->verify_error(ErrorContext::stack_underflow(_offset, this), "Operand stack underflow");
    return VerificationType::bogus_type();
  }
  VerificationType top = _stack[--_stack_size];
  bool subtype = type.is_assignable_from(top, verifier(), false, CHECK_(VerificationType::bogus_type()));
  if (!subtype) {
    verifier()->verify_error(ErrorContext::bad_type(_offset, stack_top_ctx(), TypeOrigin::implicit(type)), "Bad type on operand stack");
    return VerificationType::bogus_type();
  }
  return top;
}

VerificationType StackMapFrame::get_local(int32_t index, VerificationType type, TRAPS) {
  if (index >= _max_locals) {
    verifier()->verify_error(ErrorContext::bad_local_index(_offset, index), "Local variable table overflow");
    return VerificationType::bogus_type();
  }
  bool subtype = type.is_assignable_from(_locals[index],
    verifier(), false, CHECK_(VerificationType::bogus_type()));
  if (!subtype) {
    verifier()->verify_error(ErrorContext::bad_type(_offset, TypeOrigin::local(index, this), TypeOrigin::implicit(type)), "Bad local variable type");
    return VerificationType::bogus_type();
  }
  if (index >= _locals_size) { _locals_size = index + 1; }
  return _locals[index];
}

void StackMapFrame::get_local_2(int32_t index, VerificationType type1, VerificationType type2, TRAPS) {
  if (index >= _locals_size - 1) {
    verifier()->verify_error(ErrorContext::bad_local_index(_offset, index), "get long/double overflows locals");
    return;
  }
  bool subtype = type1.is_assignable_from(_locals[index], verifier(), false, CHECK);
  if (!subtype) {
    verifier()->verify_error(ErrorContext::bad_type(_offset, TypeOrigin::local(index, this), TypeOrigin::implicit(type1)), "Bad local variable type");
  } else {
    subtype = type2.is_assignable_from(_locals[index + 1], verifier(), false, CHECK);
    if (!subtype) {
      /* Unreachable? All local store routines convert a split long or double
       * into a TOP during the store.  So we should never end up seeing an
       * orphaned half.  */
      verifier()->verify_error(ErrorContext::bad_type(_offset, TypeOrigin::local(index + 1, this), TypeOrigin::implicit(type2)), "Bad local variable type");
    }
  }
}

void StackMapFrame::set_local(int32_t index, VerificationType type, TRAPS) {
  if (index >= _max_locals) {
    verifier()->verify_error(ErrorContext::bad_local_index(_offset, index), "Local variable table overflow");
    return;
  }
  // If type at index is double or long, set the next location to be unusable
  if (_locals[index].is_double() || _locals[index].is_long()) {
    _locals[index + 1] = VerificationType::bogus_type();
  }
  // If type at index is double_2 or long_2, set the previous location to be unusable
  if (_locals[index].is_double2() || _locals[index].is_long2()) {
    _locals[index - 1] = VerificationType::bogus_type();
  }
  _locals[index] = type;
  if (index >= _locals_size) {
    _locals_size = index + 1;
  }
}

void StackMapFrame::set_local_2(int32_t index, VerificationType type1, VerificationType type2, TRAPS) {
  if (index >= _max_locals - 1) {
    verifier()->verify_error(ErrorContext::bad_local_index(_offset, index), "Local variable table overflow");
    return;
  }
  // If type at index+1 is double or long, set the next location to be unusable
  if (_locals[index+1].is_double() || _locals[index+1].is_long()) {
    _locals[index + 2] = VerificationType::bogus_type();
  }
  // If type at index is double_2 or long_2, set the previous location to be unusable
  if (_locals[index].is_double2() || _locals[index].is_long2()) {
    _locals[index - 1] = VerificationType::bogus_type();
  }
  _locals[index] = type1;
  _locals[index+1] = type2;
  if (index >= _locals_size - 1) {
    _locals_size = index + 2;
  }
}

TypeOrigin StackMapFrame::stack_top_ctx() {
  return TypeOrigin::stack(_stack_size, this);
}

void StackMapFrame::print_on(outputStream* str) const {
  str->indent().print_cr("bci: @%d", _offset);
  str->indent().print_cr("flags: {%s }",
      flag_this_uninit() ? " flagThisUninit" : "");
  str->indent().print("locals: {");
  for (int32_t i = 0; i < _locals_size; ++i) {
    str->print(" ");
    _locals[i].print_on(str);
    if (i != _locals_size - 1) {
      str->print(",");
    }
  }
  str->print_cr(" }");
  str->indent().print("stack: {");
  for (int32_t j = 0; j < _stack_size; ++j) {
    str->print(" ");
    _stack[j].print_on(str);
    if (j != _stack_size - 1) {
      str->print(",");
    }
  }
  str->print_cr(" }");
}

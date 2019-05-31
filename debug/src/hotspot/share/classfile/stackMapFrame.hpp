#ifndef SHARE_VM_CLASSFILE_STACKMAPFRAME_HPP
#define SHARE_VM_CLASSFILE_STACKMAPFRAME_HPP

#include "classfile/verificationType.hpp"
#include "classfile/verifier.hpp"
#include "oops/method.hpp"
#include "runtime/handles.hpp"
#include "runtime/signature.hpp"
#include "utilities/exceptions.hpp"

// A StackMapFrame represents one frame in the stack map attribute.

class TypeContext;

enum {
  FLAG_THIS_UNINIT = 0x01
};

class StackMapFrame : public ResourceObj {
 private:
  int32_t _offset;

  // See comment in StackMapTable about _frame_count about why these
  // fields are int32_t instead of u2.
  int32_t _locals_size;  // number of valid type elements in _locals
  int32_t _stack_size;   // number of valid type elements in _stack

  int32_t _stack_mark;   // Records the size of the stack prior to an
                         // instruction modification, to allow rewinding
                         // when/if an error occurs.

  int32_t _max_locals;
  int32_t _max_stack;

  u1 _flags;
  VerificationType* _locals; // local variable type array
  VerificationType* _stack;  // operand stack type array

  ClassVerifier* _verifier;  // the verifier verifying this method

  StackMapFrame(const StackMapFrame& cp) :
      _offset(cp._offset), _locals_size(cp._locals_size),
      _stack_size(cp._stack_size), _stack_mark(cp._stack_mark),
      _max_locals(cp._max_locals), _max_stack(cp._max_stack),
      _flags(cp._flags) {
    _locals = NEW_RESOURCE_ARRAY(VerificationType, _max_locals);
    for (int i = 0; i < _max_locals; ++i) {
      if (i < _locals_size) {
        _locals[i] = cp._locals[i];
      } else {
        _locals[i] = VerificationType::bogus_type();
      }
    }
    int ss = MAX2(_stack_size, _stack_mark);
    _stack = NEW_RESOURCE_ARRAY(VerificationType, _max_stack);
    for (int i = 0; i < _max_stack; ++i) {
      if (i < ss) {
        _stack[i] = cp._stack[i];
      } else {
        _stack[i] = VerificationType::bogus_type();
      }
    }
    _verifier = NULL;
  }

 public:
  // constructors

  // This constructor is used by the type checker to allocate frames
  // in type state, which have _max_locals and _max_stack array elements
  // in _locals and _stack.
  StackMapFrame(u2 max_locals, u2 max_stack, ClassVerifier* verifier);

  // This constructor is used to initialize stackmap frames in stackmap table,
  // which have _locals_size and _stack_size array elements in _locals and _stack.
  StackMapFrame(int32_t offset,
                u1 flags,
                u2 locals_size,
                u2 stack_size,
                u2 max_locals,
                u2 max_stack,
                VerificationType* locals,
                VerificationType* stack,
                ClassVerifier* v) : _offset(offset), _flags(flags),
                                    _locals_size(locals_size),
                                    _stack_size(stack_size),
                                    _stack_mark(-1),
                                    _max_locals(max_locals),
                                    _max_stack(max_stack),
                                    _locals(locals), _stack(stack),
                                    _verifier(v) { }

  static StackMapFrame* copy(StackMapFrame* smf) {
    return new StackMapFrame(*smf);
  }

  inline void set_offset(int32_t offset)      { _offset = offset; }
  inline void set_verifier(ClassVerifier* v)  { _verifier = v; }
  inline void set_flags(u1 flags)             { _flags = flags; }
  inline void set_locals_size(u2 locals_size) { _locals_size = locals_size; }
  inline void set_stack_size(u2 stack_size)   { _stack_size = _stack_mark = stack_size; }
  inline void clear_stack()                   { _stack_size = 0; }
  inline int32_t offset()   const             { return _offset; }
  inline ClassVerifier* verifier() const      { return _verifier; }
  inline u1 flags() const                     { return _flags; }
  inline int32_t locals_size() const          { return _locals_size; }
  inline VerificationType* locals() const     { return _locals; }
  inline int32_t stack_size() const           { return _stack_size; }
  inline VerificationType* stack() const      { return _stack; }
  inline int32_t max_locals() const           { return _max_locals; }
  inline int32_t max_stack() const            { return _max_stack; }
  inline bool flag_this_uninit() const        { return _flags & FLAG_THIS_UNINIT; }

  // Set locals and stack types to bogus
  inline void reset() {
    int32_t i;
    for (i = 0; i < _max_locals; i++) {
      _locals[i] = VerificationType::bogus_type();
    }
    for (i = 0; i < _max_stack; i++) {
      _stack[i] = VerificationType::bogus_type();
    }
  }

  // Return a StackMapFrame with the same local variable array and empty stack.
  // Stack array is allocate with unused one element.
  StackMapFrame* frame_in_exception_handler(u1 flags);

  // Set local variable type array based on m's signature.
  VerificationType set_locals_from_arg(const methodHandle& m, VerificationType thisKlass, TRAPS);

  // Search local variable type array and stack type array.
  // Set every element with type of old_object to new_object.
  void initialize_object(VerificationType old_object, VerificationType new_object);

  // Copy local variable type array in src into this local variable type array.
  void copy_locals(const StackMapFrame* src);

  // Copy stack type array in src into this stack type array.
  void copy_stack(const StackMapFrame* src);

  // Return true if this stack map frame is assignable to target.
  bool is_assignable_to(const StackMapFrame* target, ErrorContext* ctx, TRAPS) const;

  inline void set_mark() {
    _stack_mark = _stack_size;
  }

  // Used when an error occurs and we want to reset the stack to the state
  // it was before operands were popped off.
  void restore() {
    if (_stack_mark != -1) {
      _stack_size = _stack_mark;
    }
  }

  // Push type into stack type array.
  inline void push_stack(VerificationType type, TRAPS) {
    if (_stack_size >= _max_stack) {
      verifier()->verify_error(ErrorContext::stack_overflow(_offset, this), "Operand stack overflow");
      return;
    }
    _stack[_stack_size++] = type;
  }

  inline void push_stack_2(VerificationType type1, VerificationType type2, TRAPS) {
    if (_stack_size >= _max_stack - 1) {
      verifier()->verify_error(ErrorContext::stack_overflow(_offset, this), "Operand stack overflow");
      return;
    }
    _stack[_stack_size++] = type1;
    _stack[_stack_size++] = type2;
  }

  // Pop and return the top type on stack without verifying.
  inline VerificationType pop_stack(TRAPS) {
    if (_stack_size <= 0) {
      verifier()->verify_error(ErrorContext::stack_underflow(_offset, this), "Operand stack underflow");
      return VerificationType::bogus_type();
    }
    VerificationType top = _stack[--_stack_size];
    return top;
  }

  // Pop and return the top type on stack type array after verifying it
  // is assignable to type.
  inline VerificationType pop_stack(VerificationType type, TRAPS) {
    if (_stack_size != 0) {
      VerificationType top = _stack[_stack_size - 1];
      bool subtype = type.is_assignable_from(top, verifier(), false, CHECK_(VerificationType::bogus_type()));
      if (subtype) {
        --_stack_size;
        return top;
      }
    }
    return pop_stack_ex(type, THREAD);
  }

  inline void pop_stack_2(VerificationType type1, VerificationType type2, TRAPS) {
    if (_stack_size >= 2) {
      VerificationType top1 = _stack[_stack_size - 1];
      bool subtype1 = type1.is_assignable_from(top1, verifier(), false, CHECK);
      VerificationType top2 = _stack[_stack_size - 2];
      bool subtype2 = type2.is_assignable_from(top2, verifier(), false, CHECK);
      if (subtype1 && subtype2) {
        _stack_size -= 2;
        return;
      }
    }
    pop_stack_ex(type1, THREAD);
    pop_stack_ex(type2, THREAD);
  }

  VerificationType local_at(int index) {
    return _locals[index];
  }

  VerificationType stack_at(int index) {
    return _stack[index];
  }

  // Uncommon case that throws exceptions.
  VerificationType pop_stack_ex(VerificationType type, TRAPS);

  // Return the type at index in local variable array after verifying
  // it is assignable to type.
  VerificationType get_local(int32_t index, VerificationType type, TRAPS);
  // For long/double.
  void get_local_2(int32_t index, VerificationType type1, VerificationType type2, TRAPS);

  // Set element at index in local variable array to type.
  void set_local(int32_t index, VerificationType type, TRAPS);
  // For long/double.
  void set_local_2(int32_t index, VerificationType type1, VerificationType type2, TRAPS);

  // Private auxiliary method used only in is_assignable_to(StackMapFrame).
  // Returns true if src is assignable to target.
  int is_assignable_to(VerificationType* src, VerificationType* target, int32_t len, TRAPS) const;

  TypeOrigin stack_top_ctx();

  void print_on(outputStream* str) const;
};

#endif

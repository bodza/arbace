#include "precompiled.hpp"

#include "c1/c1_IR.hpp"
#include "c1/c1_ValueStack.hpp"

// Implementation of ValueStack

ValueStack::ValueStack(IRScope* scope, ValueStack* caller_state)
: _scope(scope)
, _caller_state(caller_state)
, _bci(-99)
, _kind(Parsing)
, _locals(scope->method()->max_locals(), scope->method()->max_locals(), NULL)
, _stack(scope->method()->max_stack())
, _locks()
{
  verify();
}

ValueStack::ValueStack(ValueStack* copy_from, Kind kind, int bci)
  : _scope(copy_from->scope())
  , _caller_state(copy_from->caller_state())
  , _bci(bci)
  , _kind(kind)
  , _locals()
  , _stack()
  , _locks(copy_from->locks_size())
{
  if (kind != EmptyExceptionState) {
    // only allocate space if we need to copy the locals-array
    _locals = Values(copy_from->locals_size());
    _locals.appendAll(&copy_from->_locals);
  }

  if (kind != ExceptionState && kind != EmptyExceptionState) {
    if (kind == Parsing) {
      // stack will be modified, so reserve enough space to avoid resizing
      _stack = Values(scope()->method()->max_stack());
    } else {
      // stack will not be modified, so do not waste space
      _stack = Values(copy_from->stack_size());
    }
    _stack.appendAll(&copy_from->_stack);
  }

  _locks.appendAll(&copy_from->_locks);

  verify();
}

bool ValueStack::is_same(ValueStack* s) {
  if (scope() != s->scope()) return false;
  if (caller_state() != s->caller_state()) return false;

  if (locals_size() != s->locals_size()) return false;
  if (stack_size() != s->stack_size()) return false;
  if (locks_size() != s->locks_size()) return false;

  // compare each stack element with the corresponding stack element of s
  int index;
  Value value;
  for_each_stack_value(this, index, value) {
    if (value->type()->tag() != s->stack_at(index)->type()->tag()) return false;
  }
  for_each_lock_value(this, index, value) {
    if (value != s->lock_at(index)) return false;
  }
  return true;
}

void ValueStack::clear_locals() {
  for (int i = _locals.length() - 1; i >= 0; i--) {
    _locals.at_put(i, NULL);
  }
}

void ValueStack::pin_stack_for_linear_scan() {
  for_each_state_value(this, v,
    if (v->as_Constant() == NULL && v->as_Local() == NULL) {
      v->pin(Instruction::PinStackForStateSplit);
    }
  );
}

// apply function to all values of a list; factored out from values_do(f)
void ValueStack::apply(Values list, ValueVisitor* f) {
  for (int i = 0; i < list.length(); i++) {
    Value* va = list.adr_at(i);
    Value v0 = *va;
    if (v0 != NULL && !v0->type()->is_illegal()) {
      f->visit(va);
      if (v0->type()->is_double_word()) i++;
    }
  }
}

void ValueStack::values_do(ValueVisitor* f) {
  ValueStack* state = this;
  for_each_state(state) {
    apply(state->_locals, f);
    apply(state->_stack, f);
    apply(state->_locks, f);
  }
}

Values* ValueStack::pop_arguments(int argument_size) {
  int base = stack_size() - argument_size;
  Values* args = new Values(argument_size);
  for (int i = base; i < stack_size();) args->push(stack_at_inc(i));
  truncate_stack(base);
  return args;
}

int ValueStack::total_locks_size() const {
  int num_locks = 0;
  const ValueStack* state = this;
  for_each_state(state) {
    num_locks += state->locks_size();
  }
  return num_locks;
}

int ValueStack::lock(Value obj) {
  _locks.push(obj);
  int num_locks = total_locks_size();
  scope()->set_min_number_of_locks(num_locks);
  return num_locks - 1;
}

int ValueStack::unlock() {
  _locks.pop();
  return total_locks_size();
}

void ValueStack::setup_phi_for_stack(BlockBegin* b, int index) {
  ValueType* t = stack_at(index)->type();
  Value phi = new Phi(t, b, -index - 1);
  _stack.at_put(index, phi);
}

void ValueStack::setup_phi_for_local(BlockBegin* b, int index) {
  ValueType* t = local_at(index)->type();
  Value phi = new Phi(t, b, index);
  store_local(index, phi);
}

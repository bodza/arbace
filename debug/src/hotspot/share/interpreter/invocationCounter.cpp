#include "precompiled.hpp"

#include "interpreter/invocationCounter.hpp"
#include "runtime/frame.hpp"
#include "runtime/handles.inline.hpp"

// Implementation of InvocationCounter

void InvocationCounter::init() {
  _counter = 0;  // reset all the bits, including the sticky carry
  reset();
}

void InvocationCounter::reset() {
  // Only reset the state and don't make the method look like it's never
  // been executed
  set_state(wait_for_compile);
}

void InvocationCounter::set_carry() {
  set_carry_flag();
  // The carry bit now indicates that this counter had achieved a very
  // large value.  Now reduce the value, so that the method can be
  // executed many more times before re-entering the VM.
  int old_count = count();
  int new_count = MIN2(old_count, (int) (CompileThreshold / 2));
  // prevent from going to zero, to distinguish from never-executed methods
  if (new_count == 0)  new_count = 1;
  if (old_count != new_count)  set(state(), new_count);
}

void InvocationCounter::set_state(State state) {
  int init = _init[state];
  // prevent from going to zero, to distinguish from never-executed methods
  if (init == 0 && count() > 0)  init = 1;
  int carry = (_counter & carry_mask);    // the carry bit is sticky
  _counter = (init << number_of_noncount_bits) | carry | state;
}

void InvocationCounter::print() {
  tty->print_cr("invocation count: up = %d, limit = %d, carry = %s, state = %s", count(), limit(), carry() ? "true" : "false", state_as_string(state()));
}

void InvocationCounter::print_short() {
  tty->print(" [%d%s;%s]", count(), carry() ? "+carry" : "", state_as_short_string(state()));
}

// Initialization

int                       InvocationCounter::_init  [InvocationCounter::number_of_states];
InvocationCounter::Action InvocationCounter::_action[InvocationCounter::number_of_states];
int                       InvocationCounter::InterpreterInvocationLimit;

const char* InvocationCounter::state_as_string(State state) {
  switch (state) {
    case wait_for_nothing            : return "wait_for_nothing";
    case wait_for_compile            : return "wait_for_compile";
    default:
      ShouldNotReachHere();
      return NULL;
  }
}

const char* InvocationCounter::state_as_short_string(State state) {
  switch (state) {
    case wait_for_nothing            : return "not comp.";
    case wait_for_compile            : return "compileable";
    default:
      ShouldNotReachHere();
      return NULL;
  }
}

static address do_nothing(const methodHandle& method, TRAPS) {
  // dummy action for inactive invocation counters
  MethodCounters* mcs = method->method_counters();
  mcs->invocation_counter()->set_carry();
  mcs->invocation_counter()->set_state(InvocationCounter::wait_for_nothing);
  return NULL;
}

static address do_decay(const methodHandle& method, TRAPS) {
  // decay invocation counters so compilation gets delayed
  MethodCounters* mcs = method->method_counters();
  mcs->invocation_counter()->decay();
  return NULL;
}

void InvocationCounter::def(State state, int init, Action action) {
  _init  [state] = init;
  _action[state] = action;
}

address dummy_invocation_counter_overflow(const methodHandle& m, TRAPS) {
  ShouldNotReachHere();
  return NULL;
}

void InvocationCounter::reinitialize(bool delay_overflow) {
  // define states
  guarantee((int)number_of_states <= (int)state_limit, "adjust number_of_state_bits");
  def(wait_for_nothing, 0, do_nothing);
  if (delay_overflow) {
    def(wait_for_compile, 0, do_decay);
  } else {
    def(wait_for_compile, 0, dummy_invocation_counter_overflow);
  }

  InterpreterInvocationLimit = CompileThreshold << number_of_noncount_bits;
}

void invocationCounter_init() {
  InvocationCounter::reinitialize(DelayCompilationDuringStartup);
}

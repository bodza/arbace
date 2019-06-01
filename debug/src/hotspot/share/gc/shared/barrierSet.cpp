#include "precompiled.hpp"

#include "gc/shared/barrierSet.hpp"
#include "gc/shared/barrierSetAssembler.hpp"
#include "runtime/thread.hpp"
#include "utilities/macros.hpp"

BarrierSet* BarrierSet::_barrier_set = NULL;

void BarrierSet::set_barrier_set(BarrierSet* barrier_set) {
  _barrier_set = barrier_set;

  // The barrier set was not initialized when the this thread (the main thread)
  // was created, so the call to BarrierSet::on_thread_create() had to be deferred
  // until we have a barrier set. Now we have a barrier set, so we make the call.
  _barrier_set->on_thread_create(Thread::current());
}

// Called from init.cpp
void gc_barrier_stubs_init() {
  BarrierSet* bs = BarrierSet::barrier_set();
  BarrierSetAssembler* bs_assembler = bs->barrier_set_assembler();
  bs_assembler->barrier_stubs_init();
}

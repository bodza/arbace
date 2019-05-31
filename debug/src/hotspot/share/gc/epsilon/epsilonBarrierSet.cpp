#include "precompiled.hpp"
#include "runtime/thread.hpp"
#include "gc/epsilon/epsilonBarrierSet.hpp"
#include "gc/epsilon/epsilonThreadLocalData.hpp"
#include "gc/shared/collectorPolicy.hpp"
#include "gc/shared/barrierSet.hpp"
#include "gc/shared/barrierSetAssembler.hpp"
#include "utilities/macros.hpp"
#include "gc/shared/c1/barrierSetC1.hpp"

EpsilonBarrierSet::EpsilonBarrierSet() : BarrierSet(
          make_barrier_set_assembler<BarrierSetAssembler>(),
          make_barrier_set_c1<BarrierSetC1>(),
          make_barrier_set_c2<BarrierSetC2>(),
          BarrierSet::FakeRtti(BarrierSet::EpsilonBarrierSet)) { };

void EpsilonBarrierSet::on_thread_create(Thread *thread) {
  EpsilonThreadLocalData::create(thread);
}

void EpsilonBarrierSet::on_thread_destroy(Thread *thread) {
  EpsilonThreadLocalData::destroy(thread);
}

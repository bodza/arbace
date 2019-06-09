#ifndef SHARE_VM_GC_EPSILON_BARRIERSET_HPP
#define SHARE_VM_GC_EPSILON_BARRIERSET_HPP

#include "gc/shared/barrierSetAssembler.hpp"
#include "gc/shared/barrierSet.hpp"

// No interaction with application is required for Epsilon, and therefore
// the barrier set is empty.
class EpsilonBarrierSet: public BarrierSet {
  friend class VMStructs;

public:
  EpsilonBarrierSet();

  virtual void print_on(outputStream* st) const { }

  virtual void on_thread_create(Thread* thread);
  virtual void on_thread_destroy(Thread* thread);

  template <DecoratorSet decorators, typename BarrierSetT = EpsilonBarrierSet>
  class AccessBarrier: public BarrierSet::AccessBarrier<decorators, BarrierSetT> { };
};

template<>
struct BarrierSet::GetName<EpsilonBarrierSet> {
  static const BarrierSet::Name value = BarrierSet::EpsilonBarrierSet;
};

template<>
struct BarrierSet::GetType<BarrierSet::EpsilonBarrierSet> {
  typedef ::EpsilonBarrierSet type;
};

#endif

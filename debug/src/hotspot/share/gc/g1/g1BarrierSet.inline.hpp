#ifndef SHARE_VM_GC_G1_G1BARRIERSET_INLINE_HPP
#define SHARE_VM_GC_G1_G1BARRIERSET_INLINE_HPP

#include "gc/g1/g1BarrierSet.hpp"
#include "gc/g1/g1CardTable.hpp"
#include "gc/shared/accessBarrierSupport.inline.hpp"
#include "oops/access.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.hpp"

template <DecoratorSet decorators, typename T>
inline void G1BarrierSet::write_ref_field_pre(T* field) {
  if (HasDecorator<decorators, IS_DEST_UNINITIALIZED>::value || HasDecorator<decorators, AS_NO_KEEPALIVE>::value) {
    return;
  }

  T heap_oop = RawAccess<MO_VOLATILE>::oop_load(field);
  if (!CompressedOops::is_null(heap_oop)) {
    enqueue(CompressedOops::decode_not_null(heap_oop));
  }
}

template <DecoratorSet decorators, typename T>
inline void G1BarrierSet::write_ref_field_post(T* field, oop new_val) {
  volatile jbyte* byte = _card_table->byte_for(field);
  if (*byte != G1CardTable::g1_young_card_val()) {
    // Take a slow path for cards in old
    write_ref_field_post_slow(byte);
  }
}

inline void G1BarrierSet::enqueue_if_weak(DecoratorSet decorators, oop value) {
  // Loading from a weak or phantom reference needs enqueueing, as
  // the object may not have been reachable (part of the snapshot)
  // when marking started.
  const bool on_strong_oop_ref = (decorators & ON_STRONG_OOP_REF) != 0;
  const bool peek              = (decorators & AS_NO_KEEPALIVE) != 0;
  const bool needs_enqueue     = (!peek && !on_strong_oop_ref);

  if (needs_enqueue && value != NULL) {
    enqueue(value);
  }
}

template <DecoratorSet decorators, typename BarrierSetT>
template <typename T>
inline oop G1BarrierSet::AccessBarrier<decorators, BarrierSetT>::oop_load_not_in_heap(T* addr) {
  oop value = ModRef::oop_load_not_in_heap(addr);
  enqueue_if_weak(decorators, value);
  return value;
}

template <DecoratorSet decorators, typename BarrierSetT>
template <typename T>
inline oop G1BarrierSet::AccessBarrier<decorators, BarrierSetT>::oop_load_in_heap(T* addr) {
  oop value = ModRef::oop_load_in_heap(addr);
  enqueue_if_weak(decorators, value);
  return value;
}

template <DecoratorSet decorators, typename BarrierSetT>
inline oop G1BarrierSet::AccessBarrier<decorators, BarrierSetT>::oop_load_in_heap_at(oop base, ptrdiff_t offset) {
  oop value = ModRef::oop_load_in_heap_at(base, offset);
  enqueue_if_weak(AccessBarrierSupport::resolve_possibly_unknown_oop_ref_strength<decorators>(base, offset), value);
  return value;
}

template <DecoratorSet decorators, typename BarrierSetT>
template <typename T>
inline void G1BarrierSet::AccessBarrier<decorators, BarrierSetT>::oop_store_not_in_heap(T* addr, oop new_value) {
  // Apply SATB barriers for all non-heap references, to allow
  // concurrent scanning of such references.
  G1BarrierSet *bs = barrier_set_cast<G1BarrierSet>(BarrierSet::barrier_set());
  bs->write_ref_field_pre<decorators>(addr);
  Raw::oop_store(addr, new_value);
}

#endif

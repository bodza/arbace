#ifndef SHARE_VM_GC_G1_G1BARRIERSET_HPP
#define SHARE_VM_GC_G1_G1BARRIERSET_HPP

#include "gc/g1/dirtyCardQueue.hpp"
#include "gc/g1/satbMarkQueue.hpp"
#include "gc/shared/cardTableBarrierSet.hpp"

class DirtyCardQueueSet;
class CardTable;
class G1CardTable;

// This barrier is specialized to use a logging barrier to support
// snapshot-at-the-beginning marking.

class G1BarrierSet: public CardTableBarrierSet {
  friend class VMStructs;
 private:
  static SATBMarkQueueSet  _satb_mark_queue_set;
  static DirtyCardQueueSet _dirty_card_queue_set;

 public:
  G1BarrierSet(G1CardTable* table);
  ~G1BarrierSet() { }

  // Add "pre_val" to a set of objects that may have been disconnected from the
  // pre-marking object graph.
  static void enqueue(oop pre_val);

  static void enqueue_if_weak(DecoratorSet decorators, oop value);

  template <class T> void write_ref_array_pre_work(T* dst, size_t count);
  virtual void write_ref_array_pre(oop* dst, size_t count, bool dest_uninitialized);
  virtual void write_ref_array_pre(narrowOop* dst, size_t count, bool dest_uninitialized);

  template <DecoratorSet decorators, typename T>
  void write_ref_field_pre(T* field);

  // NB: if you do a whole-heap invalidation, the "usual invariant" defined
  // above no longer applies.
  void invalidate(MemRegion mr);

  void write_region(MemRegion mr)         { invalidate(mr); }
  void write_ref_array_work(MemRegion mr) { invalidate(mr); }

  template <DecoratorSet decorators, typename T>
  void write_ref_field_post(T* field, oop new_val);
  void write_ref_field_post_slow(volatile jbyte* byte);

  virtual void on_thread_create(Thread* thread);
  virtual void on_thread_destroy(Thread* thread);
  virtual void on_thread_attach(JavaThread* thread);
  virtual void on_thread_detach(JavaThread* thread);

  static SATBMarkQueueSet& satb_mark_queue_set() {
    return _satb_mark_queue_set;
  }

  static DirtyCardQueueSet& dirty_card_queue_set() {
    return _dirty_card_queue_set;
  }

  // Callbacks for runtime accesses.
  template <DecoratorSet decorators, typename BarrierSetT = G1BarrierSet>
  class AccessBarrier: public ModRefBarrierSet::AccessBarrier<decorators, BarrierSetT> {
    typedef ModRefBarrierSet::AccessBarrier<decorators, BarrierSetT> ModRef;
    typedef BarrierSet::AccessBarrier<decorators, BarrierSetT> Raw;

  public:
    // Needed for loads on non-heap weak references
    template <typename T>
    static oop oop_load_not_in_heap(T* addr);

    // Needed for non-heap stores
    template <typename T>
    static void oop_store_not_in_heap(T* addr, oop new_value);

    // Needed for weak references
    static oop oop_load_in_heap_at(oop base, ptrdiff_t offset);

    // Defensive: will catch weak oops at addresses in heap
    template <typename T>
    static oop oop_load_in_heap(T* addr);
  };
};

template<>
struct BarrierSet::GetName<G1BarrierSet> {
  static const BarrierSet::Name value = BarrierSet::G1BarrierSet;
};

template<>
struct BarrierSet::GetType<BarrierSet::G1BarrierSet> {
  typedef ::G1BarrierSet type;
};

#endif

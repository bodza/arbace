#include "precompiled.hpp"

#include "gc/shared/cardTableBarrierSetAssembler.hpp"
#include "gc/shared/cardTableBarrierSet.inline.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/space.inline.hpp"
#include "logging/log.hpp"
#include "memory/virtualspace.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/thread.hpp"
#include "services/memTracker.hpp"
#include "utilities/align.hpp"
#include "utilities/macros.hpp"
#include "gc/shared/c1/cardTableBarrierSetC1.hpp"

class CardTableBarrierSetC1;
class CardTableBarrierSetC2;

// This kind of "BarrierSet" allows a "CollectedHeap" to detect and
// enumerate ref fields that have been modified (since the last
// enumeration.)

CardTableBarrierSet::CardTableBarrierSet(BarrierSetAssembler* barrier_set_assembler,
                                         BarrierSetC1* barrier_set_c1,
                                         BarrierSetC2* barrier_set_c2,
                                         CardTable* card_table,
                                         const BarrierSet::FakeRtti& fake_rtti) :
  ModRefBarrierSet(barrier_set_assembler,
                   barrier_set_c1,
                   barrier_set_c2,
                   fake_rtti.add_tag(BarrierSet::CardTableBarrierSet)),
  _defer_initial_card_mark(false),
  _card_table(card_table)
{ }

CardTableBarrierSet::CardTableBarrierSet(CardTable* card_table) :
  ModRefBarrierSet(make_barrier_set_assembler<CardTableBarrierSetAssembler>(),
                   make_barrier_set_c1<CardTableBarrierSetC1>(),
                   make_barrier_set_c2<CardTableBarrierSetC2>(),
                   BarrierSet::FakeRtti(BarrierSet::CardTableBarrierSet)),
  _defer_initial_card_mark(false),
  _card_table(card_table)
{ }

void CardTableBarrierSet::initialize() {
  initialize_deferred_card_mark_barriers();
}

CardTableBarrierSet::~CardTableBarrierSet() {
  delete _card_table;
}

void CardTableBarrierSet::write_ref_array_work(MemRegion mr) {
  _card_table->dirty_MemRegion(mr);
}

void CardTableBarrierSet::invalidate(MemRegion mr) {
  _card_table->invalidate(mr);
}

void CardTableBarrierSet::print_on(outputStream* st) const {
  _card_table->print_on(st);
}

// Helper for ReduceInitialCardMarks. For performance,
// compiled code may elide card-marks for initializing stores
// to a newly allocated object along the fast-path. We
// compensate for such elided card-marks as follows:
// (a) Generational, non-concurrent collectors, such as
//     GenCollectedHeap(ParNew,DefNew,Tenured) and
//     ParallelScavengeHeap(ParallelGC, ParallelOldGC)
//     need the card-mark if and only if the region is
//     in the old gen, and do not care if the card-mark
//     succeeds or precedes the initializing stores themselves,
//     so long as the card-mark is completed before the next
//     scavenge. For all these cases, we can do a card mark
//     at the point at which we do a slow path allocation
//     in the old gen, i.e. in this call.
// (b) GenCollectedHeap(ConcurrentMarkSweepGeneration) requires
//     in addition that the card-mark for an old gen allocated
//     object strictly follow any associated initializing stores.
//     In these cases, the memRegion remembered below is
//     used to card-mark the entire region either just before the next
//     slow-path allocation by this thread or just before the next scavenge or
//     CMS-associated safepoint, whichever of these events happens first.
//     (The implicit assumption is that the object has been fully
//     initialized by this point, a fact that we assert when doing the
//     card-mark.)
// (c) G1CollectedHeap(G1) uses two kinds of write barriers. When a
//     G1 concurrent marking is in progress an SATB (pre-write-)barrier
//     is used to remember the pre-value of any store. Initializing
//     stores will not need this barrier, so we need not worry about
//     compensating for the missing pre-barrier here. Turning now
//     to the post-barrier, we note that G1 needs a RS update barrier
//     which simply enqueues a (sequence of) dirty cards which may
//     optionally be refined by the concurrent update threads. Note
//     that this barrier need only be applied to a non-young write,
//     but, like in CMS, because of the presence of concurrent refinement
//     (much like CMS' precleaning), must strictly follow the oop-store.
//     Thus, using the same protocol for maintaining the intended
//     invariants turns out, serendepitously, to be the same for both
//     G1 and CMS.
//
// For any future collector, this code should be reexamined with
// that specific collector in mind, and the documentation above suitably
// extended and updated.
void CardTableBarrierSet::on_slowpath_allocation_exit(JavaThread* thread, oop new_obj) {
  if (!ReduceInitialCardMarks) {
    return;
  }
  // If a previous card-mark was deferred, flush it now.
  flush_deferred_card_mark_barrier(thread);
  if (new_obj->is_typeArray() || _card_table->is_in_young(new_obj)) {
    // Arrays of non-references don't need a post-barrier.
    // The deferred_card_mark region should be empty
    // following the flush above.
  } else {
    MemRegion mr((HeapWord*)new_obj, new_obj->size());
    if (_defer_initial_card_mark) {
      // Defer the card mark
      thread->set_deferred_card_mark(mr);
    } else {
      // Do the card mark
      invalidate(mr);
    }
  }
}

void CardTableBarrierSet::initialize_deferred_card_mark_barriers() {
  // Used for ReduceInitialCardMarks (when COMPILER2 or JVMCI is used);
  // otherwise remains unused.
  _defer_initial_card_mark = is_server_compilation_mode_vm() && ReduceInitialCardMarks && (DeferInitialCardMark || card_mark_must_follow_store());
}

void CardTableBarrierSet::flush_deferred_card_mark_barrier(JavaThread* thread) {
  MemRegion deferred = thread->deferred_card_mark();
  if (!deferred.is_empty()) {
    write_region(deferred);
    // "Clear" the deferred_card_mark field
    thread->set_deferred_card_mark(MemRegion());
  }
}

void CardTableBarrierSet::on_thread_detach(JavaThread* thread) {
  // The deferred store barriers must all have been flushed to the
  // card-table (or other remembered set structure) before GC starts
  // processing the card-table (or other remembered set).
  flush_deferred_card_mark_barrier(thread);
}

bool CardTableBarrierSet::card_mark_must_follow_store() const {
 return _card_table->scanned_concurrently();
}

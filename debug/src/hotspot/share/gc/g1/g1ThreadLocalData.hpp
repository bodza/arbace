#ifndef SHARE_GC_G1_G1THREADLOCALDATA_HPP
#define SHARE_GC_G1_G1THREADLOCALDATA_HPP

#include "gc/g1/dirtyCardQueue.hpp"
#include "gc/g1/g1BarrierSet.hpp"
#include "gc/g1/satbMarkQueue.hpp"
#include "runtime/thread.hpp"
#include "utilities/debug.hpp"
#include "utilities/sizes.hpp"

class G1ThreadLocalData {
private:
  SATBMarkQueue  _satb_mark_queue;
  DirtyCardQueue _dirty_card_queue;

  G1ThreadLocalData() :
      _satb_mark_queue(&G1BarrierSet::satb_mark_queue_set()),
      _dirty_card_queue(&G1BarrierSet::dirty_card_queue_set()) {}

  static G1ThreadLocalData* data(Thread* thread) {
    assert(UseG1GC, "Sanity");
    return thread->gc_data<G1ThreadLocalData>();
  }

  static ByteSize satb_mark_queue_offset() {
    return Thread::gc_data_offset() + byte_offset_of(G1ThreadLocalData, _satb_mark_queue);
  }

  static ByteSize dirty_card_queue_offset() {
    return Thread::gc_data_offset() + byte_offset_of(G1ThreadLocalData, _dirty_card_queue);
  }

public:
  static void create(Thread* thread) {
    new (data(thread)) G1ThreadLocalData();
  }

  static void destroy(Thread* thread) {
    data(thread)->~G1ThreadLocalData();
  }

  static SATBMarkQueue& satb_mark_queue(Thread* thread) {
    return data(thread)->_satb_mark_queue;
  }

  static DirtyCardQueue& dirty_card_queue(Thread* thread) {
    return data(thread)->_dirty_card_queue;
  }

  static ByteSize satb_mark_queue_active_offset() {
    return satb_mark_queue_offset() + SATBMarkQueue::byte_offset_of_active();
  }

  static ByteSize satb_mark_queue_index_offset() {
    return satb_mark_queue_offset() + SATBMarkQueue::byte_offset_of_index();
  }

  static ByteSize satb_mark_queue_buffer_offset() {
    return satb_mark_queue_offset() + SATBMarkQueue::byte_offset_of_buf();
  }

  static ByteSize dirty_card_queue_index_offset() {
    return dirty_card_queue_offset() + DirtyCardQueue::byte_offset_of_index();
  }

  static ByteSize dirty_card_queue_buffer_offset() {
    return dirty_card_queue_offset() + DirtyCardQueue::byte_offset_of_buf();
  }
};

#endif

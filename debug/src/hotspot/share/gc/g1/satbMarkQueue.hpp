#ifndef SHARE_VM_GC_G1_SATBMARKQUEUE_HPP
#define SHARE_VM_GC_G1_SATBMARKQUEUE_HPP

#include "gc/g1/ptrQueue.hpp"
#include "memory/allocation.hpp"

class JavaThread;
class SATBMarkQueueSet;

// Base class for processing the contents of a SATB buffer.
class SATBBufferClosure : public StackObj {
protected:
  ~SATBBufferClosure() { }

public:
  // Process the SATB entries in the designated buffer range.
  virtual void do_buffer(void** buffer, size_t size) = 0;
};

// A PtrQueue whose elements are (possibly stale) pointers to object heads.
class SATBMarkQueue: public PtrQueue {
  friend class SATBMarkQueueSet;

private:
  // Filter out unwanted entries from the buffer.
  void filter();

public:
  SATBMarkQueue(SATBMarkQueueSet* qset, bool permanent = false);

  // Process queue entries and free resources.
  void flush();

  // Apply cl to the active part of the buffer.
  // Prerequisite: Must be at a safepoint.
  void apply_closure_and_empty(SATBBufferClosure* cl);

  // Overrides PtrQueue::should_enqueue_buffer(). See the method's
  // definition for more information.
  virtual bool should_enqueue_buffer();

  // Compiler support.
  static ByteSize byte_offset_of_index() {
    return PtrQueue::byte_offset_of_index<SATBMarkQueue>();
  }
  using PtrQueue::byte_width_of_index;

  static ByteSize byte_offset_of_buf() {
    return PtrQueue::byte_offset_of_buf<SATBMarkQueue>();
  }
  using PtrQueue::byte_width_of_buf;

  static ByteSize byte_offset_of_active() {
    return PtrQueue::byte_offset_of_active<SATBMarkQueue>();
  }
  using PtrQueue::byte_width_of_active;
};

class SATBMarkQueueSet: public PtrQueueSet {
  SATBMarkQueue _shared_satb_queue;

public:
  SATBMarkQueueSet();

  void initialize(Monitor* cbl_mon, Mutex* fl_lock, int process_completed_threshold, Mutex* lock);

  static void handle_zero_index_for_thread(JavaThread* t);

  // Apply "set_active(active)" to all SATB queues in the set. It should be
  // called only with the world stopped. The method will assert that the
  // SATB queues of all threads it visits, as well as the SATB queue
  // set itself, has an active value same as expected_active.
  void set_active_all_threads(bool active, bool expected_active);

  // Filter all the currently-active SATB buffers.
  void filter_thread_buffers();

  // If there exists some completed buffer, pop and process it, and
  // return true.  Otherwise return false.  Processing a buffer
  // consists of applying the closure to the active range of the
  // buffer; the leading entries may be excluded due to filtering.
  bool apply_closure_to_completed_buffer(SATBBufferClosure* cl);

  SATBMarkQueue* shared_satb_queue() { return &_shared_satb_queue; }

  // If a marking is being abandoned, reset any unprocessed log buffers.
  void abandon_partial_marking();
};

#endif

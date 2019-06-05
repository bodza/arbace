#include "precompiled.hpp"

#include "gc/shared/threadLocalAllocBuffer.inline.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/threadSMR.hpp"
#include "utilities/copy.hpp"

// Thread-Local Edens support

// static member initialization
size_t           ThreadLocalAllocBuffer::_max_size       = 0;
int              ThreadLocalAllocBuffer::_reserve_for_allocation_prefetch = 0;
unsigned         ThreadLocalAllocBuffer::_target_refills = 0;
GlobalTLABStats* ThreadLocalAllocBuffer::_global_stats   = NULL;

void ThreadLocalAllocBuffer::clear_before_allocation() {
  _slow_refill_waste += (unsigned)remaining();
  make_parsable(true);   // also retire the TLAB
}

size_t ThreadLocalAllocBuffer::remaining() {
  if (end() == NULL) {
    return 0;
  }

  return pointer_delta(hard_end(), top());
}

void ThreadLocalAllocBuffer::accumulate_statistics_before_gc() {
  global_stats()->initialize();

  for (JavaThreadIteratorWithHandle jtiwh; JavaThread *thread = jtiwh.next(); ) {
    thread->tlab().accumulate_statistics();
    thread->tlab().initialize_statistics();
  }

  // Publish new stats if some allocation occurred.
  if (global_stats()->allocation() != 0) {
    global_stats()->publish();
    global_stats()->print();
  }
}

void ThreadLocalAllocBuffer::accumulate_statistics() {
  Thread* thread = myThread();
  size_t capacity = Universe::heap()->tlab_capacity(thread);
  size_t used     = Universe::heap()->tlab_used(thread);

  _gc_waste += (unsigned)remaining();
  size_t total_allocated = thread->allocated_bytes();
  size_t allocated_since_last_gc = total_allocated - _allocated_before_last_gc;
  _allocated_before_last_gc = total_allocated;

  if (_number_of_refills > 0) {
    // Update allocation history if a reasonable amount of eden was allocated.
    bool update_allocation_history = used > 0.5 * capacity;

    if (update_allocation_history) {
      // Average the fraction of eden allocated in a tlab by this
      // thread for use in the next resize operation.
      // _gc_waste is not subtracted because it's included in
      // "used".
      // The result can be larger than 1.0 due to direct to old allocations.
      // These allocations should ideally not be counted but since it is not possible
      // to filter them out here we just cap the fraction to be at most 1.0.
      double alloc_frac = MIN2(1.0, (double) allocated_since_last_gc / used);
      _allocation_fraction.sample(alloc_frac);
    }
    global_stats()->update_allocating_threads();
    global_stats()->update_number_of_refills(_number_of_refills);
    global_stats()->update_allocation(_allocated_size);
    global_stats()->update_gc_waste(_gc_waste);
    global_stats()->update_slow_refill_waste(_slow_refill_waste);
    global_stats()->update_fast_refill_waste(_fast_refill_waste);
  }
  global_stats()->update_slow_allocations(_slow_allocations);
}

// Fills the current tlab with a dummy filler array to create
// an illusion of a contiguous Eden and optionally retires the tlab.
// Waste accounting should be done in caller as appropriate; see,
// for example, clear_before_allocation().
void ThreadLocalAllocBuffer::make_parsable(bool retire, bool zap) {
  if (end() != NULL) {
    invariants();

    if (retire) {
      myThread()->incr_allocated_bytes(used_bytes());
    }

    Universe::heap()->fill_with_dummy_object(top(), hard_end(), retire && zap);

    if (retire || ZeroTLAB) {  // "Reset" the TLAB
      set_start(NULL);
      set_top(NULL);
      set_pf_top(NULL);
      set_end(NULL);
      set_allocation_end(NULL);
    }
  }
}

void ThreadLocalAllocBuffer::resize_all_tlabs() {
  if (ResizeTLAB) {
    for (JavaThreadIteratorWithHandle jtiwh; JavaThread *thread = jtiwh.next(); ) {
      thread->tlab().resize();
    }
  }
}

void ThreadLocalAllocBuffer::resize() {
  // Compute the next tlab size using expected allocation amount
  size_t alloc = (size_t)(_allocation_fraction.average() * (Universe::heap()->tlab_capacity(myThread()) / HeapWordSize));
  size_t new_size = alloc / _target_refills;

  new_size = MIN2(MAX2(new_size, min_size()), max_size());

  size_t aligned_new_size = align_object_size(new_size);

  set_desired_size(aligned_new_size);
  set_refill_waste_limit(initial_refill_waste_limit());
}

void ThreadLocalAllocBuffer::initialize_statistics() {
  _number_of_refills = 0;
  _fast_refill_waste = 0;
  _slow_refill_waste = 0;
  _gc_waste          = 0;
  _slow_allocations  = 0;
  _allocated_size    = 0;
}

void ThreadLocalAllocBuffer::fill(HeapWord* start, HeapWord* top, size_t new_size) {
  _number_of_refills++;
  _allocated_size += new_size;

  initialize(start, top, start + new_size - alignment_reserve());

  // Reset amount of internal fragmentation
  set_refill_waste_limit(initial_refill_waste_limit());
}

void ThreadLocalAllocBuffer::initialize(HeapWord* start, HeapWord* top, HeapWord* end) {
  set_start(start);
  set_top(top);
  set_pf_top(top);
  set_end(end);
  set_allocation_end(end);
  invariants();
}

void ThreadLocalAllocBuffer::initialize() {
  initialize(NULL,                    // start
             NULL,                    // top
             NULL);                   // end

  set_desired_size(initial_desired_size());

  // Following check is needed because at startup the main
  // thread is initialized before the heap is.  The initialization for
  // this thread is redone in startup_initialization below.
  if (Universe::heap() != NULL) {
    size_t capacity   = Universe::heap()->tlab_capacity(myThread()) / HeapWordSize;
    double alloc_frac = desired_size() * target_refills() / (double) capacity;
    _allocation_fraction.sample(alloc_frac);
  }

  set_refill_waste_limit(initial_refill_waste_limit());

  initialize_statistics();
}

void ThreadLocalAllocBuffer::startup_initialization() {
  // Assuming each thread's active tlab is, on average,
  // 1/2 full at a GC
  _target_refills = 100 / (2 * TLABWasteTargetPercent);
  _target_refills = MAX2(_target_refills, (unsigned)1U);

  _global_stats = new GlobalTLABStats();

  // During jvm startup, the main thread is initialized
  // before the heap is initialized.  So reinitialize it now.
  guarantee(Thread::current()->is_Java_thread(), "tlab initialization thread not Java thread");
  Thread::current()->tlab().initialize();
}

size_t ThreadLocalAllocBuffer::initial_desired_size() {
  size_t init_sz = 0;

  if (TLABSize > 0) {
    init_sz = TLABSize / HeapWordSize;
  } else if (global_stats() != NULL) {
    // Initial size is a function of the average number of allocating threads.
    unsigned nof_threads = global_stats()->allocating_threads_avg();

    init_sz  = (Universe::heap()->tlab_capacity(myThread()) / HeapWordSize) / (nof_threads * target_refills());
    init_sz = align_object_size(init_sz);
  }
  init_sz = MIN2(MAX2(init_sz, min_size()), max_size());
  return init_sz;
}

void ThreadLocalAllocBuffer::set_sample_end() {
  size_t heap_words_remaining = pointer_delta(_end, _top);
  size_t bytes_until_sample = myThread()->heap_sampler().bytes_until_sample();
  size_t words_until_sample = bytes_until_sample / HeapWordSize;

  if (heap_words_remaining > words_until_sample) {
    HeapWord* new_end = _top + words_until_sample;
    set_end(new_end);
    _bytes_since_last_sample_point = bytes_until_sample;
  } else {
    _bytes_since_last_sample_point = heap_words_remaining * HeapWordSize;
  }
}

Thread* ThreadLocalAllocBuffer::myThread() {
  return (Thread*)(((char *)this) + in_bytes(start_offset()) - in_bytes(Thread::tlab_start_offset()));
}

void ThreadLocalAllocBuffer::set_back_allocation_end() {
  _end = _allocation_end;
}

HeapWord* ThreadLocalAllocBuffer::hard_end() {
  return _allocation_end + alignment_reserve();
}

GlobalTLABStats::GlobalTLABStats() :
  _allocating_threads_avg(TLABAllocationWeight) {
  initialize();

  _allocating_threads_avg.sample(1); // One allocating thread at startup
}

void GlobalTLABStats::initialize() {
  // Clear counters summarizing info from all threads
  _allocating_threads      = 0;
  _total_refills           = 0;
  _max_refills             = 0;
  _total_allocation        = 0;
  _total_gc_waste          = 0;
  _max_gc_waste            = 0;
  _total_slow_refill_waste = 0;
  _max_slow_refill_waste   = 0;
  _total_fast_refill_waste = 0;
  _max_fast_refill_waste   = 0;
  _total_slow_allocations  = 0;
  _max_slow_allocations    = 0;
}

void GlobalTLABStats::publish() {
  _allocating_threads_avg.sample(_allocating_threads);
}

void GlobalTLABStats::print() { }

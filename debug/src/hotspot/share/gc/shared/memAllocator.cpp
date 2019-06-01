#include "precompiled.hpp"

#include "classfile/javaClasses.hpp"
#include "gc/shared/allocTracer.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/memAllocator.hpp"
#include "gc/shared/threadLocalAllocBuffer.inline.hpp"
#include "memory/universe.hpp"
#include "oops/arrayOop.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/thread.inline.hpp"
#include "services/lowMemoryDetector.hpp"
#include "utilities/align.hpp"
#include "utilities/copy.hpp"

class MemAllocator::Allocation: StackObj {
  friend class MemAllocator;

  const MemAllocator& _allocator;
  Thread*             _thread;
  oop*                _obj_ptr;
  bool                _overhead_limit_exceeded;
  bool                _allocated_outside_tlab;
  size_t              _allocated_tlab_size;
  bool                _tlab_end_reset_for_sample;

  bool check_out_of_memory();
  void verify_before();
  void verify_after();
  void notify_allocation();
  void notify_allocation_jvmti_allocation_event();
  void notify_allocation_jvmti_sampler();
  void notify_allocation_low_memory_detector();
  void notify_allocation_jfr_sampler();
  void notify_allocation_dtrace_sampler();
  void check_for_bad_heap_word_value() const;

  class PreserveObj;

public:
  Allocation(const MemAllocator& allocator, oop* obj_ptr)
    : _allocator(allocator),
      _thread(Thread::current()),
      _obj_ptr(obj_ptr),
      _overhead_limit_exceeded(false),
      _allocated_outside_tlab(false),
      _allocated_tlab_size(0),
      _tlab_end_reset_for_sample(false)
  {
    verify_before();
  }

  ~Allocation() {
    if (!check_out_of_memory()) {
      verify_after();
      notify_allocation();
    }
  }

  oop obj() const { return *_obj_ptr; }
};

class MemAllocator::Allocation::PreserveObj: StackObj {
  HandleMark _handle_mark;
  Handle     _handle;
  oop* const _obj_ptr;

public:
  PreserveObj(Thread* thread, oop* obj_ptr)
    : _handle_mark(thread),
      _handle(thread, *obj_ptr),
      _obj_ptr(obj_ptr)
  {
    *obj_ptr = NULL;
  }

  ~PreserveObj() {
    *_obj_ptr = _handle();
  }

  oop operator()() const {
    return _handle();
  }
};

bool MemAllocator::Allocation::check_out_of_memory() {
  Thread* THREAD = _thread;

  if (obj() != NULL) {
    return false;
  }

  if (!_overhead_limit_exceeded) {
    // -XX:+HeapDumpOnOutOfMemoryError and -XX:OnOutOfMemoryError support
    report_java_out_of_memory("Java heap space");

    THROW_OOP_(Universe::out_of_memory_error_java_heap(), true);
  } else {
    // -XX:+HeapDumpOnOutOfMemoryError and -XX:OnOutOfMemoryError support
    report_java_out_of_memory("GC overhead limit exceeded");

    THROW_OOP_(Universe::out_of_memory_error_gc_overhead_limit(), true);
  }
}

void MemAllocator::Allocation::verify_before() {
  // Clear unhandled oops for memory allocation.  Memory allocation might
  // not take out a lock if from tlab, so clear here.
  Thread* THREAD = _thread;
  CHECK_UNHANDLED_OOPS_ONLY(THREAD->clear_unhandled_oops();)
}

void MemAllocator::Allocation::verify_after() { }

void MemAllocator::Allocation::check_for_bad_heap_word_value() const { }

void MemAllocator::Allocation::notify_allocation_jvmti_sampler() {
  if (!ThreadHeapSampler::enabled()) {
    // Sampling disabled
    return;
  }

  if (!_allocated_outside_tlab && _allocated_tlab_size == 0 && !_tlab_end_reset_for_sample) {
    // Sample if it's a non-TLAB allocation, or a TLAB allocation that either refills the TLAB
    // or expands it due to taking a sampler induced slow path.
    return;
  }

  if (_tlab_end_reset_for_sample || _allocated_tlab_size != 0) {
    _thread->tlab().set_sample_end();
  }
}

void MemAllocator::Allocation::notify_allocation_low_memory_detector() {
  // support low memory notifications (no-op if not enabled)
  LowMemoryDetector::detect_low_memory_for_collected_pools();
}

void MemAllocator::Allocation::notify_allocation_jfr_sampler() {
  HeapWord* mem = (HeapWord*)obj();
  size_t size_in_bytes = _allocator._word_size * HeapWordSize;

  if (_allocated_outside_tlab) {
    AllocTracer::send_allocation_outside_tlab(_allocator._klass, mem, size_in_bytes, _thread);
  } else if (_allocated_tlab_size != 0) {
    // TLAB was refilled
    AllocTracer::send_allocation_in_new_tlab(_allocator._klass, mem, _allocated_tlab_size * HeapWordSize, size_in_bytes, _thread);
  }
}

void MemAllocator::Allocation::notify_allocation_dtrace_sampler() {
  if (DTraceAllocProbes) {
    // support for Dtrace object alloc event (no-op most of the time)
    Klass* klass = _allocator._klass;
    size_t word_size = _allocator._word_size;
    if (klass != NULL && klass->name() != NULL) {
      SharedRuntime::dtrace_object_alloc(obj(), (int)word_size);
    }
  }
}

void MemAllocator::Allocation::notify_allocation() {
  notify_allocation_low_memory_detector();
  notify_allocation_jfr_sampler();
  notify_allocation_dtrace_sampler();
  notify_allocation_jvmti_sampler();
}

HeapWord* MemAllocator::allocate_outside_tlab(Allocation& allocation) const {
  allocation._allocated_outside_tlab = true;
  HeapWord* mem = _heap->mem_allocate(_word_size, &allocation._overhead_limit_exceeded);
  if (mem == NULL) {
    return mem;
  }

  size_t size_in_bytes = _word_size * HeapWordSize;
  _thread->incr_allocated_bytes(size_in_bytes);

  return mem;
}

HeapWord* MemAllocator::allocate_inside_tlab(Allocation& allocation) const {

  // Try allocating from an existing TLAB.
  HeapWord* mem = _thread->tlab().allocate(_word_size);
  if (mem != NULL) {
    return mem;
  }

  // Try refilling the TLAB and allocating the object in it.
  return allocate_inside_tlab_slow(allocation);
}

HeapWord* MemAllocator::allocate_inside_tlab_slow(Allocation& allocation) const {
  HeapWord* mem = NULL;
  ThreadLocalAllocBuffer& tlab = _thread->tlab();

  if (ThreadHeapSampler::enabled()) {
    // Try to allocate the sampled object from TLAB, it is possible a sample
    // point was put and the TLAB still has space.
    tlab.set_back_allocation_end();
    mem = tlab.allocate(_word_size);
    if (mem != NULL) {
      allocation._tlab_end_reset_for_sample = true;
      return mem;
    }
  }

  // Retain tlab and allocate object in shared space if
  // the amount free in the tlab is too large to discard.
  if (tlab.free() > tlab.refill_waste_limit()) {
    tlab.record_slow_allocation(_word_size);
    return NULL;
  }

  // Discard tlab and allocate a new one.
  // To minimize fragmentation, the last TLAB may be smaller than the rest.
  size_t new_tlab_size = tlab.compute_size(_word_size);

  tlab.clear_before_allocation();

  if (new_tlab_size == 0) {
    return NULL;
  }

  // Allocate a new TLAB requesting new_tlab_size. Any size
  // between minimal and new_tlab_size is accepted.
  size_t min_tlab_size = ThreadLocalAllocBuffer::compute_min_size(_word_size);
  mem = _heap->allocate_new_tlab(min_tlab_size, new_tlab_size, &allocation._allocated_tlab_size);
  if (mem == NULL) {
    return NULL;
  }

  if (ZeroTLAB) {
    // ..and clear it.
    Copy::zero_to_words(mem, allocation._allocated_tlab_size);
  } else {
    // ...and zap just allocated object.
  }

  tlab.fill(mem, mem + _word_size, allocation._allocated_tlab_size);
  return mem;
}

HeapWord* MemAllocator::mem_allocate(Allocation& allocation) const {
  if (UseTLAB) {
    HeapWord* result = allocate_inside_tlab(allocation);
    if (result != NULL) {
      return result;
    }
  }

  return allocate_outside_tlab(allocation);
}

oop MemAllocator::allocate() const {
  oop obj = NULL;
  {
    Allocation allocation(*this, &obj);
    HeapWord* mem = mem_allocate(allocation);
    if (mem != NULL) {
      obj = initialize(mem);
    }
  }
  return obj;
}

void MemAllocator::mem_clear(HeapWord* mem) const {
  const size_t hs = oopDesc::header_size();
  oopDesc::set_klass_gap(mem, 0);
  Copy::fill_to_aligned_words(mem + hs, _word_size - hs);
}

oop MemAllocator::finish(HeapWord* mem) const {
  if (UseBiasedLocking) {
    oopDesc::set_mark_raw(mem, _klass->prototype_header());
  } else {
    // May be bootstrapping
    oopDesc::set_mark_raw(mem, markOopDesc::prototype());
  }
  // Need a release store to ensure array/class length, mark word, and
  // object zeroing are visible before setting the klass non-NULL, for
  // concurrent collectors.
  oopDesc::release_set_klass(mem, _klass);
  return oop(mem);
}

oop ObjAllocator::initialize(HeapWord* mem) const {
  mem_clear(mem);
  return finish(mem);
}

MemRegion ObjArrayAllocator::obj_memory_range(oop obj) const {
  if (_do_zero) {
    return MemAllocator::obj_memory_range(obj);
  }
  ArrayKlass* array_klass = ArrayKlass::cast(_klass);
  const size_t hs = arrayOopDesc::header_size(array_klass->element_type());
  return MemRegion(((HeapWord*)obj) + hs, _word_size - hs);
}

oop ObjArrayAllocator::initialize(HeapWord* mem) const {
  // Set array length before setting the _klass field because a
  // non-NULL klass field indicates that the object is parsable by
  // concurrent GC.
  if (_do_zero) {
    mem_clear(mem);
  }
  arrayOopDesc::set_length(mem, _length);
  return finish(mem);
}

oop ClassAllocator::initialize(HeapWord* mem) const {
  // Set oop_size field before setting the _klass field because a
  // non-NULL _klass field indicates that the object is parsable by
  // concurrent GC.
  mem_clear(mem);
  java_lang_Class::set_oop_size(mem, (int)_word_size);
  return finish(mem);
}

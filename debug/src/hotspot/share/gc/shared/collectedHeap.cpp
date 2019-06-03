#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "gc/shared/barrierSet.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "gc/shared/gcLocker.inline.hpp"
#include "gc/shared/gcHeapSummary.hpp"
#include "gc/shared/gcTrace.hpp"
#include "gc/shared/gcWhen.hpp"
#include "gc/shared/memAllocator.hpp"
#include "gc/shared/vmGCOperations.hpp"
#include "memory/metaspace.hpp"
#include "memory/resourceArea.hpp"
#include "oops/instanceMirrorKlass.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/init.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/threadSMR.hpp"
#include "runtime/vmThread.hpp"
#include "utilities/align.hpp"
#include "utilities/copy.hpp"

class ClassLoaderData;

size_t CollectedHeap::_filler_array_max_size = 0;

template <>
void EventLogBase<GCMessage>::print(outputStream* st, GCMessage& m) {
  st->print_cr("GC heap %s", m.is_before ? "before" : "after");
  st->print_raw(m);
}

VirtualSpaceSummary CollectedHeap::create_heap_space_summary() {
  size_t capacity_in_words = capacity() / HeapWordSize;

  return VirtualSpaceSummary(reserved_region().start(), reserved_region().start() + capacity_in_words, reserved_region().end());
}

GCHeapSummary CollectedHeap::create_heap_summary() {
  VirtualSpaceSummary heap_space = create_heap_space_summary();
  return GCHeapSummary(heap_space, used());
}

MetaspaceSummary CollectedHeap::create_metaspace_summary() {
  const MetaspaceSizes meta_space(MetaspaceUtils::committed_bytes(), MetaspaceUtils::used_bytes(), MetaspaceUtils::reserved_bytes());
  const MetaspaceSizes data_space(MetaspaceUtils::committed_bytes(Metaspace::NonClassType), MetaspaceUtils::used_bytes(Metaspace::NonClassType), MetaspaceUtils::reserved_bytes(Metaspace::NonClassType));
  const MetaspaceSizes class_space(MetaspaceUtils::committed_bytes(Metaspace::ClassType), MetaspaceUtils::used_bytes(Metaspace::ClassType), MetaspaceUtils::reserved_bytes(Metaspace::ClassType));

  const MetaspaceChunkFreeListSummary& ms_chunk_free_list_summary = MetaspaceUtils::chunk_free_list_summary(Metaspace::NonClassType);
  const MetaspaceChunkFreeListSummary& class_chunk_free_list_summary = MetaspaceUtils::chunk_free_list_summary(Metaspace::ClassType);

  return MetaspaceSummary(MetaspaceGC::capacity_until_GC(), meta_space, data_space, class_space, ms_chunk_free_list_summary, class_chunk_free_list_summary);
}

void CollectedHeap::print_heap_before_gc() {
  Universe::print_heap_before_gc();
  if (_gc_heap_log != NULL) {
    _gc_heap_log->log_heap_before(this);
  }
}

void CollectedHeap::print_heap_after_gc() {
  Universe::print_heap_after_gc();
  if (_gc_heap_log != NULL) {
    _gc_heap_log->log_heap_after(this);
  }
}

void CollectedHeap::print_on_error(outputStream* st) const {
  st->print_cr("Heap:");
  print_extended_on(st);
  st->cr();

  BarrierSet::barrier_set()->print_on(st);
}

void CollectedHeap::trace_heap(GCWhen::Type when, const GCTracer* gc_tracer) {
  const GCHeapSummary& heap_summary = create_heap_summary();
  gc_tracer->report_gc_heap_summary(when, heap_summary);

  const MetaspaceSummary& metaspace_summary = create_metaspace_summary();
  gc_tracer->report_metaspace_summary(when, metaspace_summary);
}

void CollectedHeap::trace_heap_before_gc(const GCTracer* gc_tracer) {
  trace_heap(GCWhen::BeforeGC, gc_tracer);
}

void CollectedHeap::trace_heap_after_gc(const GCTracer* gc_tracer) {
  trace_heap(GCWhen::AfterGC, gc_tracer);
}

// WhiteBox API support for concurrent collectors.  These are the
// default implementations, for collectors which don't support this
// feature.
bool CollectedHeap::supports_concurrent_phase_control() const {
  return false;
}

const char* const* CollectedHeap::concurrent_phases() const {
  static const char* const result[] = { NULL };
  return result;
}

bool CollectedHeap::request_concurrent_phase(const char* phase) {
  return false;
}

bool CollectedHeap::is_oop(oop object) const {
  if (!check_obj_alignment(object)) {
    return false;
  }

  if (!is_in_reserved(object)) {
    return false;
  }

  if (is_in_reserved(object->klass_or_null())) {
    return false;
  }

  return true;
}

// Memory state functions.

CollectedHeap::CollectedHeap() :
  _is_gc_active(false),
  _total_collections(0),
  _total_full_collections(0),
  _gc_cause(GCCause::_no_gc),
  _gc_lastcause(GCCause::_no_gc)
{
  const size_t max_len = size_t(arrayOopDesc::max_array_length(T_INT));
  const size_t elements_per_word = HeapWordSize / sizeof(jint);
  _filler_array_max_size = align_object_size(filler_array_hdr_size() + max_len / elements_per_word);

  if (UsePerfData) {
    EXCEPTION_MARK;

    // create the gc cause jvmstat counters
    _perf_gc_cause = PerfDataManager::create_string_variable(SUN_GC, "cause", 80, GCCause::to_string(_gc_cause), CHECK);
    _perf_gc_lastcause = PerfDataManager::create_string_variable(SUN_GC, "lastCause", 80, GCCause::to_string(_gc_lastcause), CHECK);
  }

  _gc_heap_log = NULL;
}

// This interface assumes that it's being called by the
// vm thread. It collects the heap assuming that the
// heap lock is already held and that we are executing in
// the context of the vm thread.
void CollectedHeap::collect_as_vm_thread(GCCause::Cause cause) {
  GCCauseSetter gcs(this, cause);
  switch (cause) {
    case GCCause::_heap_inspection:
    case GCCause::_heap_dump:
    case GCCause::_metadata_GC_threshold : {
      HandleMark hm;
      do_full_collection(false);        // don't clear all soft refs
      break;
    }
    case GCCause::_metadata_GC_clear_soft_refs: {
      HandleMark hm;
      do_full_collection(true);         // do clear all soft refs
      break;
    }
    default:
      ShouldNotReachHere(); // Unexpected use of this function
  }
}

MetaWord* CollectedHeap::satisfy_failed_metadata_allocation(ClassLoaderData* loader_data, size_t word_size, Metaspace::MetadataType mdtype) {
  uint loop_count = 0;
  uint gc_count = 0;
  uint full_gc_count = 0;

  do {
    MetaWord* result = loader_data->metaspace_non_null()->allocate(word_size, mdtype);
    if (result != NULL) {
      return result;
    }

    if (GCLocker::is_active_and_needs_gc()) {
      // If the GCLocker is active, just expand and allocate.
      // If that does not succeed, wait if this thread is not
      // in a critical section itself.
      result = loader_data->metaspace_non_null()->expand_and_allocate(word_size, mdtype);
      if (result != NULL) {
        return result;
      }
      JavaThread* jthr = JavaThread::current();
      if (!jthr->in_critical()) {
        // Wait for JNI critical section to be exited
        GCLocker::stall_until_clear();
        // The GC invoked by the last thread leaving the critical
        // section will be a young collection and a full collection
        // is (currently) needed for unloading classes so continue
        // to the next iteration to get a full GC.
        continue;
      } else {
        return NULL;
      }
    }

    {  // Need lock to get self consistent gc_count's
      MutexLocker ml(Heap_lock);
      gc_count      = Universe::heap()->total_collections();
      full_gc_count = Universe::heap()->total_full_collections();
    }

    // Generate a VM operation
    VM_CollectForMetadataAllocation op(loader_data, word_size, mdtype, gc_count, full_gc_count, GCCause::_metadata_GC_threshold);
    VMThread::execute(&op);

    // If GC was locked out, try again. Check before checking success because the
    // prologue could have succeeded and the GC still have been locked out.
    if (op.gc_locked()) {
      continue;
    }

    if (op.prologue_succeeded()) {
      return op.result();
    }
    loop_count++;
  } while (true);  // Until a GC is done
}

size_t CollectedHeap::max_tlab_size() const {
  // TLABs can't be bigger than we can fill with a int[Integer.MAX_VALUE].
  // This restriction could be removed by enabling filling with multiple arrays.
  // If we compute that the reasonable way as
  //    header_size + ((sizeof(jint) * max_jint) / HeapWordSize)
  // we'll overflow on the multiply, so we do the divide first.
  // We actually lose a little by dividing first,
  // but that just makes the TLAB  somewhat smaller than the biggest array,
  // which is fine, since we'll be able to fill that.
  size_t max_int_size = typeArrayOopDesc::header_size(T_INT) + sizeof(jint) * ((juint) max_jint / (size_t) HeapWordSize);
  return align_down(max_int_size, MinObjAlignment);
}

size_t CollectedHeap::filler_array_hdr_size() {
  return align_object_offset(arrayOopDesc::header_size(T_INT)); // align to Long
}

size_t CollectedHeap::filler_array_min_size() {
  return align_object_size(filler_array_hdr_size()); // align to MinObjAlignment
}

void CollectedHeap::fill_with_array(HeapWord* start, size_t words, bool zap) {
  const size_t payload_size = words - filler_array_hdr_size();
  const size_t len = payload_size * HeapWordSize / sizeof(jint);

  ObjArrayAllocator allocator(Universe::intArrayKlassObj(), words, (int)len, /* do_zero */ false);
  allocator.initialize(start);
}

void CollectedHeap::fill_with_object_impl(HeapWord* start, size_t words, bool zap) {
  if (words >= filler_array_min_size()) {
    fill_with_array(start, words, zap);
  } else if (words > 0) {
    ObjAllocator allocator(SystemDictionary::Object_klass(), words);
    allocator.initialize(start);
  }
}

void CollectedHeap::fill_with_object(HeapWord* start, size_t words, bool zap) {
  HandleMark hm;  // Free handles before leaving.
  fill_with_object_impl(start, words, zap);
}

void CollectedHeap::fill_with_objects(HeapWord* start, size_t words, bool zap) {
  HandleMark hm;  // Free handles before leaving.

  // Multiple objects may be required depending on the filler array maximum size. Fill
  // the range up to that with objects that are filler_array_max_size sized. The
  // remainder is filled with a single object.
  const size_t min = min_fill_size();
  const size_t max = filler_array_max_size();
  while (words > max) {
    const size_t cur = (words - max) >= min ? max : max - min;
    fill_with_array(start, cur, zap);
    start += cur;
    words -= cur;
  }

  fill_with_object_impl(start, words, zap);
}

void CollectedHeap::fill_with_dummy_object(HeapWord* start, HeapWord* end, bool zap) {
  CollectedHeap::fill_with_object(start, end, zap);
}

HeapWord* CollectedHeap::allocate_new_tlab(size_t min_size, size_t requested_size, size_t* actual_size) {
  guarantee(false, "thread-local allocation buffers not supported");
  return NULL;
}

oop CollectedHeap::obj_allocate(Klass* klass, int size, TRAPS) {
  ObjAllocator allocator(klass, size, THREAD);
  return allocator.allocate();
}

oop CollectedHeap::array_allocate(Klass* klass, int size, int length, bool do_zero, TRAPS) {
  ObjArrayAllocator allocator(klass, size, length, do_zero, THREAD);
  return allocator.allocate();
}

oop CollectedHeap::class_allocate(Klass* klass, int size, TRAPS) {
  ClassAllocator allocator(klass, size, THREAD);
  return allocator.allocate();
}

void CollectedHeap::ensure_parsability(bool retire_tlabs) {
  // The second disjunct in the assertion below makes a concession
  // for the start-up verification done while the VM is being
  // created. Callers be careful that you know that mutators
  // aren't going to interfere -- for instance, this is permissible
  // if we are still single-threaded and have either not yet
  // started allocating (nothing much to verify) or we have
  // started allocating but are now a full-fledged JavaThread
  // (and have thus made our TLAB's) available for filling.
  const bool use_tlab = UseTLAB;
  // The main thread starts allocating via a TLAB even before it
  // has added itself to the threads list at vm boot-up.
  JavaThreadIteratorWithHandle jtiwh;
  BarrierSet *bs = BarrierSet::barrier_set();
  for (; JavaThread *thread = jtiwh.next(); ) {
     if (use_tlab) thread->tlab().make_parsable(retire_tlabs);
     bs->make_parsable(thread);
  }
}

void CollectedHeap::accumulate_statistics_all_tlabs() {
  if (UseTLAB) {
    ThreadLocalAllocBuffer::accumulate_statistics_before_gc();
  }
}

void CollectedHeap::resize_all_tlabs() {
  if (UseTLAB) {
    ThreadLocalAllocBuffer::resize_all_tlabs();
  }
}

void CollectedHeap::full_gc_dump(GCTimer* timer, bool before) { }

void CollectedHeap::pre_full_gc_dump(GCTimer* timer) {
  full_gc_dump(timer, true);
}

void CollectedHeap::post_full_gc_dump(GCTimer* timer) {
  full_gc_dump(timer, false);
}

void CollectedHeap::initialize_reserved_region(HeapWord *start, HeapWord *end) {
  // It is important to do this in a way such that concurrent readers can't
  // temporarily think something is in the heap.  (Seen this happen in asserts.)
  _reserved.set_word_size(0);
  _reserved.set_start(start);
  _reserved.set_end(end);
}

void CollectedHeap::post_initialize() {
  initialize_serviceability();
}

bool CollectedHeap::supports_object_pinning() const {
  return false;
}

oop CollectedHeap::pin_object(JavaThread* thread, oop obj) {
  ShouldNotReachHere();
  return NULL;
}

void CollectedHeap::unpin_object(JavaThread* thread, oop obj) {
  ShouldNotReachHere();
}

void CollectedHeap::deduplicate_string(oop str) {
  // Do nothing, unless overridden in subclass.
}

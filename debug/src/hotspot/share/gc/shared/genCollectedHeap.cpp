#include "precompiled.hpp"
#include "aot/aotLoader.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/stringTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/codeCache.hpp"
#include "code/icBuffer.hpp"
#include "gc/shared/adaptiveSizePolicy.hpp"
#include "gc/shared/cardTableBarrierSet.hpp"
#include "gc/shared/cardTableRS.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "gc/shared/collectorCounters.hpp"
#include "gc/shared/gcId.hpp"
#include "gc/shared/gcLocker.hpp"
#include "gc/shared/gcPolicyCounters.hpp"
#include "gc/shared/gcTrace.hpp"
#include "gc/shared/gcTraceTime.inline.hpp"
#include "gc/shared/genCollectedHeap.hpp"
#include "gc/shared/genOopClosures.inline.hpp"
#include "gc/shared/generationSpec.hpp"
#include "gc/shared/oopStorageParState.inline.hpp"
#include "gc/shared/space.hpp"
#include "gc/shared/strongRootsScope.hpp"
#include "gc/shared/vmGCOperations.hpp"
#include "gc/shared/weakProcessor.hpp"
#include "gc/shared/workgroup.hpp"
#include "memory/filemap.hpp"
#include "memory/metaspaceCounters.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/flags/flagSetting.hpp"
#include "runtime/handles.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/vmThread.hpp"
#include "services/management.hpp"
#include "services/memoryService.hpp"
#include "utilities/debug.hpp"
#include "utilities/formatBuffer.hpp"
#include "utilities/macros.hpp"
#include "utilities/stack.inline.hpp"
#include "utilities/vmError.hpp"

GenCollectedHeap::GenCollectedHeap(GenCollectorPolicy *policy,
                                   Generation::Name young,
                                   Generation::Name old,
                                   const char* policy_counters_name) :
  CollectedHeap(),
  _rem_set(NULL),
  _young_gen_spec(new GenerationSpec(young,
                                     policy->initial_young_size(),
                                     policy->max_young_size(),
                                     policy->gen_alignment())),
  _old_gen_spec(new GenerationSpec(old,
                                   policy->initial_old_size(),
                                   policy->max_old_size(),
                                   policy->gen_alignment())),
  _gen_policy(policy),
  _soft_ref_gen_policy(),
  _gc_policy_counters(new GCPolicyCounters(policy_counters_name, 2, 2)),
  _process_strong_tasks(new SubTasksDone(GCH_PS_NumElements)),
  _full_collections_completed(0) {
}

jint GenCollectedHeap::initialize() {
  // While there are no constraints in the GC code that HeapWordSize
  // be any particular value, there are multiple other areas in the
  // system which believe this to be true (e.g. oop->object_size in some
  // cases incorrectly returns the size in wordSize units rather than
  // HeapWordSize).
  guarantee(HeapWordSize == wordSize, "HeapWordSize must equal wordSize");

  // Allocate space for the heap.

  char* heap_address;
  ReservedSpace heap_rs;

  size_t heap_alignment = collector_policy()->heap_alignment();

  heap_address = allocate(heap_alignment, &heap_rs);

  if (!heap_rs.is_reserved()) {
    vm_shutdown_during_initialization("Could not reserve enough space for object heap");
    return JNI_ENOMEM;
  }

  initialize_reserved_region((HeapWord*)heap_rs.base(), (HeapWord*)(heap_rs.base() + heap_rs.size()));

  _rem_set = create_rem_set(reserved_region());
  _rem_set->initialize();
  CardTableBarrierSet *bs = new CardTableBarrierSet(_rem_set);
  bs->initialize();
  BarrierSet::set_barrier_set(bs);

  ReservedSpace young_rs = heap_rs.first_part(_young_gen_spec->max_size(), false, false);
  _young_gen = _young_gen_spec->init(young_rs, rem_set());
  heap_rs = heap_rs.last_part(_young_gen_spec->max_size());

  ReservedSpace old_rs = heap_rs.first_part(_old_gen_spec->max_size(), false, false);
  _old_gen = _old_gen_spec->init(old_rs, rem_set());
  clear_incremental_collection_failed();

  return JNI_OK;
}

CardTableRS* GenCollectedHeap::create_rem_set(const MemRegion& reserved_region) {
  return new CardTableRS(reserved_region, false /* scan_concurrently */);
}

void GenCollectedHeap::initialize_size_policy(size_t init_eden_size, size_t init_promo_size, size_t init_survivor_size) {
  const double max_gc_pause_sec = ((double) MaxGCPauseMillis) / 1000.0;
  _size_policy = new AdaptiveSizePolicy(init_eden_size,
                                        init_promo_size,
                                        init_survivor_size,
                                        max_gc_pause_sec,
                                        GCTimeRatio);
}

char* GenCollectedHeap::allocate(size_t alignment,
                                 ReservedSpace* heap_rs) {
  // Now figure out the total size.
  const size_t pageSize = UseLargePages ? os::large_page_size() : os::vm_page_size();

  // Check for overflow.
  size_t total_reserved = _young_gen_spec->max_size() + _old_gen_spec->max_size();
  if (total_reserved < _young_gen_spec->max_size()) {
    vm_exit_during_initialization("The size of the object heap + VM data exceeds the maximum representable size");
  }

  *heap_rs = Universe::reserve_heap(total_reserved, alignment);

  os::trace_page_sizes("Heap",
                       collector_policy()->min_heap_byte_size(),
                       total_reserved,
                       alignment,
                       heap_rs->base(),
                       heap_rs->size());

  return heap_rs->base();
}

void GenCollectedHeap::post_initialize() {
  CollectedHeap::post_initialize();
  ref_processing_init();
}

void GenCollectedHeap::ref_processing_init() {
  _young_gen->ref_processor_init();
  _old_gen->ref_processor_init();
}

GenerationSpec* GenCollectedHeap::young_gen_spec() const {
  return _young_gen_spec;
}

GenerationSpec* GenCollectedHeap::old_gen_spec() const {
  return _old_gen_spec;
}

size_t GenCollectedHeap::capacity() const {
  return _young_gen->capacity() + _old_gen->capacity();
}

size_t GenCollectedHeap::used() const {
  return _young_gen->used() + _old_gen->used();
}

void GenCollectedHeap::save_used_regions() {
  _old_gen->save_used_region();
  _young_gen->save_used_region();
}

size_t GenCollectedHeap::max_capacity() const {
  return _young_gen->max_capacity() + _old_gen->max_capacity();
}

// Update the _full_collections_completed counter
// at the end of a stop-world full GC.
unsigned int GenCollectedHeap::update_full_collections_completed() {
  MonitorLockerEx ml(FullGCCount_lock, Mutex::_no_safepoint_check_flag);
  _full_collections_completed = _total_full_collections;
  ml.notify_all();
  return _full_collections_completed;
}

// Update the _full_collections_completed counter, as appropriate,
// at the end of a concurrent GC cycle. Note the conditional update
// below to allow this method to be called by a concurrent collector
// without synchronizing in any manner with the VM thread (which
// may already have initiated a STW full collection "concurrently").
unsigned int GenCollectedHeap::update_full_collections_completed(unsigned int count) {
  MonitorLockerEx ml(FullGCCount_lock, Mutex::_no_safepoint_check_flag);
  if (count > _full_collections_completed) {
    _full_collections_completed = count;
    ml.notify_all();
  }
  return _full_collections_completed;
}

// Return true if any of the following is true:
// . the allocation won't fit into the current young gen heap
// . gc locker is occupied (jni critical section)
// . heap memory is tight -- the most recent previous collection
//   was a full collection because a partial collection (would
//   have) failed and is likely to fail again
bool GenCollectedHeap::should_try_older_generation_allocation(size_t word_size) const {
  size_t young_capacity = _young_gen->capacity_before_gc();
  return    (word_size > heap_word_size(young_capacity)) || GCLocker::is_active_and_needs_gc() || incremental_collection_failed();
}

HeapWord* GenCollectedHeap::expand_heap_and_allocate(size_t size, bool   is_tlab) {
  HeapWord* result = NULL;
  if (_old_gen->should_allocate(size, is_tlab)) {
    result = _old_gen->expand_and_allocate(size, is_tlab);
  }
  if (result == NULL) {
    if (_young_gen->should_allocate(size, is_tlab)) {
      result = _young_gen->expand_and_allocate(size, is_tlab);
    }
  }
  return result;
}

HeapWord* GenCollectedHeap::mem_allocate_work(size_t size,
                                              bool is_tlab,
                                              bool* gc_overhead_limit_was_exceeded) {
  // In general gc_overhead_limit_was_exceeded should be false so
  // set it so here and reset it to true only if the gc time
  // limit is being exceeded as checked below.
  *gc_overhead_limit_was_exceeded = false;

  HeapWord* result = NULL;

  // Loop until the allocation is satisfied, or unsatisfied after GC.
  for (uint try_count = 1, gclocker_stalled_count = 0; /* return or throw */; try_count += 1) {
    HandleMark hm; // Discard any handles allocated in each iteration.

    // First allocation attempt is lock-free.
    Generation *young = _young_gen;
    if (young->should_allocate(size, is_tlab)) {
      result = young->par_allocate(size, is_tlab);
      if (result != NULL) {
        return result;
      }
    }
    uint gc_count_before;  // Read inside the Heap_lock locked region.
    {
      MutexLocker ml(Heap_lock);
      log_trace(gc, alloc)("GenCollectedHeap::mem_allocate_work: attempting locked slow path allocation");
      // Note that only large objects get a shot at being
      // allocated in later generations.
      bool first_only = !should_try_older_generation_allocation(size);

      result = attempt_allocation(size, is_tlab, first_only);
      if (result != NULL) {
        return result;
      }

      if (GCLocker::is_active_and_needs_gc()) {
        if (is_tlab) {
          return NULL;  // Caller will retry allocating individual object.
        }
        if (!is_maximal_no_gc()) {
          // Try and expand heap to satisfy request.
          result = expand_heap_and_allocate(size, is_tlab);
          // Result could be null if we are out of space.
          if (result != NULL) {
            return result;
          }
        }

        if (gclocker_stalled_count > GCLockerRetryAllocationCount) {
          return NULL; // We didn't get to do a GC and we didn't get any memory.
        }

        // If this thread is not in a jni critical section, we stall
        // the requestor until the critical section has cleared and
        // GC allowed. When the critical section clears, a GC is
        // initiated by the last thread exiting the critical section; so
        // we retry the allocation sequence from the beginning of the loop,
        // rather than causing more, now probably unnecessary, GC attempts.
        JavaThread* jthr = JavaThread::current();
        if (!jthr->in_critical()) {
          MutexUnlocker mul(Heap_lock);
          // Wait for JNI critical section to be exited
          GCLocker::stall_until_clear();
          gclocker_stalled_count += 1;
          continue;
        } else {
          if (CheckJNICalls) {
            fatal("Possible deadlock due to allocating while in jni critical section");
          }
          return NULL;
        }
      }

      // Read the gc count while the heap lock is held.
      gc_count_before = total_collections();
    }

    VM_GenCollectForAllocation op(size, is_tlab, gc_count_before);
    VMThread::execute(&op);
    if (op.prologue_succeeded()) {
      result = op.result();
      if (op.gc_locked()) {
         continue;  // Retry and/or stall as necessary.
      }

      // Allocation has failed and a collection
      // has been done.  If the gc time limit was exceeded the
      // this time, return NULL so that an out-of-memory
      // will be thrown.  Clear gc_overhead_limit_exceeded
      // so that the overhead exceeded does not persist.

      const bool limit_exceeded = size_policy()->gc_overhead_limit_exceeded();
      const bool softrefs_clear = soft_ref_policy()->all_soft_refs_clear();

      if (limit_exceeded && softrefs_clear) {
        *gc_overhead_limit_was_exceeded = true;
        size_policy()->set_gc_overhead_limit_exceeded(false);
        if (op.result() != NULL) {
          CollectedHeap::fill_with_object(op.result(), size);
        }
        return NULL;
      }
      return result;
    }

    // Give a warning if we seem to be looping forever.
    if ((QueuedAllocationWarningCount > 0) && (try_count % QueuedAllocationWarningCount == 0)) {
          log_warning(gc, ergo)("GenCollectedHeap::mem_allocate_work retries %d times, size=" SIZE_FORMAT " %s", try_count, size, is_tlab ? "(TLAB)" : "");
    }
  }
}

HeapWord* GenCollectedHeap::attempt_allocation(size_t size, bool is_tlab, bool first_only) {
  HeapWord* res = NULL;

  if (_young_gen->should_allocate(size, is_tlab)) {
    res = _young_gen->allocate(size, is_tlab);
    if (res != NULL || first_only) {
      return res;
    }
  }

  if (_old_gen->should_allocate(size, is_tlab)) {
    res = _old_gen->allocate(size, is_tlab);
  }

  return res;
}

HeapWord* GenCollectedHeap::mem_allocate(size_t size,
                                         bool* gc_overhead_limit_was_exceeded) {
  return mem_allocate_work(size,
                           false /* is_tlab */,
                           gc_overhead_limit_was_exceeded);
}

bool GenCollectedHeap::must_clear_all_soft_refs() {
  return _gc_cause == GCCause::_metadata_GC_clear_soft_refs || _gc_cause == GCCause::_wb_full_gc;
}

void GenCollectedHeap::collect_generation(Generation* gen, bool full, size_t size, bool is_tlab, bool run_verification, bool clear_soft_refs, bool restore_marks_for_biased_locking) {
  FormatBuffer<> title("Collect gen: %s", gen->short_name());
  GCTraceTime(Trace, gc, phases) t1(title);
  TraceCollectorStats tcs(gen->counters());
  TraceMemoryManagerStats tmms(gen->gc_manager(), gc_cause());

  gen->stat_record()->invocations++;
  gen->stat_record()->accumulated_time.start();

  // Must be done anew before each collection because
  // a previous collection will do mangling and will
  // change top of some spaces.
  record_gen_tops_before_GC();

  log_trace(gc)("%s invoke=%d size=" SIZE_FORMAT, heap()->is_young_gen(gen) ? "Young" : "Old", gen->stat_record()->invocations, size * HeapWordSize);

  if (run_verification && VerifyBeforeGC) {
    HandleMark hm;  // Discard invalid handles created during verification
    Universe::verify("Before GC");
  }

  if (restore_marks_for_biased_locking) {
    // We perform this mark word preservation work lazily
    // because it's only at this point that we know whether we
    // absolutely have to do it; we want to avoid doing it for
    // scavenge-only collections where it's unnecessary
    BiasedLocking::preserve_marks();
  }

  // Do collection work
  {
    // Note on ref discovery: For what appear to be historical reasons,
    // GCH enables and disabled (by enqueing) refs discovery.
    // In the future this should be moved into the generation's
    // collect method so that ref discovery and enqueueing concerns
    // are local to a generation. The collect method could return
    // an appropriate indication in the case that notification on
    // the ref lock was needed. This will make the treatment of
    // weak refs more uniform (and indeed remove such concerns
    // from GCH). XXX

    HandleMark hm;  // Discard invalid handles created during gc
    save_marks();   // save marks for all gens
    // We want to discover references, but not process them yet.
    // This mode is disabled in process_discovered_references if the
    // generation does some collection work, or in
    // enqueue_discovered_references if the generation returns
    // without doing any work.
    ReferenceProcessor* rp = gen->ref_processor();
    // If the discovery of ("weak") refs in this generation is
    // atomic wrt other collectors in this configuration, we
    // are guaranteed to have empty discovered ref lists.
    if (rp->discovery_is_atomic()) {
      rp->enable_discovery();
      rp->setup_policy(clear_soft_refs);
    } else {
      // collect() below will enable discovery as appropriate
    }
    gen->collect(full, clear_soft_refs, size, is_tlab);
    if (!rp->enqueuing_is_done()) {
      rp->disable_discovery();
    } else {
      rp->set_enqueuing_is_done(false);
    }
    rp->verify_no_references_recorded();
  }

  gen->stat_record()->accumulated_time.stop();

  update_gc_stats(gen, full);

  if (run_verification && VerifyAfterGC) {
    HandleMark hm;  // Discard invalid handles created during verification
    Universe::verify("After GC");
  }
}

void GenCollectedHeap::do_collection(bool full, bool clear_all_soft_refs, size_t size, bool is_tlab, GenerationType max_generation) {
  ResourceMark rm;

  guarantee(!is_gc_active(), "collection is not reentrant");

  if (GCLocker::check_active_before_gc()) {
    return; // GC is disabled (e.g. JNI GetXXXCritical operation)
  }

  GCIdMark gc_id_mark;

  const bool do_clear_all_soft_refs = clear_all_soft_refs || soft_ref_policy()->should_clear_all_soft_refs();

  ClearedAllSoftRefs casr(do_clear_all_soft_refs, soft_ref_policy());

  const size_t metadata_prev_used = MetaspaceUtils::used_bytes();

  print_heap_before_gc();

  {
    FlagSetting fl(_is_gc_active, true);

    bool complete = full && (max_generation == OldGen);
    bool old_collects_young = complete && !ScavengeBeforeFullGC;
    bool do_young_collection = !old_collects_young && _young_gen->should_collect(full, size, is_tlab);

    FormatBuffer<> gc_string("%s", "Pause ");
    if (do_young_collection) {
      gc_string.append("Young");
    } else {
      gc_string.append("Full");
    }

    GCTraceCPUTime tcpu;
    GCTraceTime(Info, gc) t(gc_string, NULL, gc_cause(), true);

    gc_prologue(complete);
    increment_total_collections(complete);

    size_t young_prev_used = _young_gen->used();
    size_t old_prev_used = _old_gen->used();

    bool run_verification = total_collections() >= VerifyGCStartAt;

    bool prepared_for_verification = false;
    bool collected_old = false;

    if (do_young_collection) {
      if (run_verification && VerifyGCLevel <= 0 && VerifyBeforeGC) {
        prepare_for_verify();
        prepared_for_verification = true;
      }

      collect_generation(_young_gen, full, size, is_tlab, run_verification && VerifyGCLevel <= 0, do_clear_all_soft_refs, false);

      if (size > 0 && (!is_tlab || _young_gen->supports_tlab_allocation()) && size * HeapWordSize <= _young_gen->unsafe_max_alloc_nogc()) {
        // Allocation request was met by young GC.
        size = 0;
      }
    }

    bool must_restore_marks_for_biased_locking = false;

    if (max_generation == OldGen && _old_gen->should_collect(full, size, is_tlab)) {
      if (!complete) {
        // The full_collections increment was missed above.
        increment_total_full_collections();
      }

      if (!prepared_for_verification && run_verification && VerifyGCLevel <= 1 && VerifyBeforeGC) {
        prepare_for_verify();
      }

      if (do_young_collection) {
        // We did a young GC. Need a new GC id for the old GC.
        GCIdMark gc_id_mark;
        GCTraceTime(Info, gc) t("Pause Full", NULL, gc_cause(), true);
        collect_generation(_old_gen, full, size, is_tlab, run_verification && VerifyGCLevel <= 1, do_clear_all_soft_refs, true);
      } else {
        // No young GC done. Use the same GC id as was set up earlier in this method.
        collect_generation(_old_gen, full, size, is_tlab, run_verification && VerifyGCLevel <= 1, do_clear_all_soft_refs, true);
      }

      must_restore_marks_for_biased_locking = true;
      collected_old = true;
    }

    // Update "complete" boolean wrt what actually transpired --
    // for instance, a promotion failure could have led to
    // a whole heap collection.
    complete = complete || collected_old;

    print_heap_change(young_prev_used, old_prev_used);
    MetaspaceUtils::print_metaspace_change(metadata_prev_used);

    // Adjust generation sizes.
    if (collected_old) {
      _old_gen->compute_new_size();
    }
    _young_gen->compute_new_size();

    if (complete) {
      // Delete metaspaces for unloaded class loaders and clean up loader_data graph
      ClassLoaderDataGraph::purge();
      MetaspaceUtils::verify_metrics();
      // Resize the metaspace capacity after full collections
      MetaspaceGC::compute_new_size();
      update_full_collections_completed();
    }

    // Track memory usage and detect low memory after GC finishes
    MemoryService::track_memory_usage();

    gc_epilogue(complete);

    if (must_restore_marks_for_biased_locking) {
      BiasedLocking::restore_marks();
    }
  }

  print_heap_after_gc();

#ifdef TRACESPINNING
  ParallelTaskTerminator::print_termination_counts();
#endif
}

void GenCollectedHeap::register_nmethod(nmethod* nm) {
  CodeCache::register_scavenge_root_nmethod(nm);
}

void GenCollectedHeap::verify_nmethod(nmethod* nm) {
  CodeCache::verify_scavenge_root_nmethod(nm);
}

HeapWord* GenCollectedHeap::satisfy_failed_allocation(size_t size, bool is_tlab) {
  GCCauseSetter x(this, GCCause::_allocation_failure);
  HeapWord* result = NULL;

  if (GCLocker::is_active_and_needs_gc()) {
    // GC locker is active; instead of a collection we will attempt
    // to expand the heap, if there's room for expansion.
    if (!is_maximal_no_gc()) {
      result = expand_heap_and_allocate(size, is_tlab);
    }
    return result;   // Could be null if we are out of space.
  } else if (!incremental_collection_will_fail(false /* don't consult_young */)) {
    // Do an incremental collection.
    do_collection(false,                     // full
                  false,                     // clear_all_soft_refs
                  size,                      // size
                  is_tlab,                   // is_tlab
                  GenCollectedHeap::OldGen); // max_generation
  } else {
    log_trace(gc)(" :: Trying full because partial may fail :: ");
    // Try a full collection; see delta for bug id 6266275
    // for the original code and why this has been simplified
    // with from-space allocation criteria modified and
    // such allocation moved out of the safepoint path.
    do_collection(true,                      // full
                  false,                     // clear_all_soft_refs
                  size,                      // size
                  is_tlab,                   // is_tlab
                  GenCollectedHeap::OldGen); // max_generation
  }

  result = attempt_allocation(size, is_tlab, false /*first_only*/);

  if (result != NULL) {
    return result;
  }

  // OK, collection failed, try expansion.
  result = expand_heap_and_allocate(size, is_tlab);
  if (result != NULL) {
    return result;
  }

  // If we reach this point, we're really out of memory. Try every trick
  // we can to reclaim memory. Force collection of soft references. Force
  // a complete compaction of the heap. Any additional methods for finding
  // free memory should be here, especially if they are expensive. If this
  // attempt fails, an OOM exception will be thrown.
  {
    UIntFlagSetting flag_change(MarkSweepAlwaysCompactCount, 1); // Make sure the heap is fully compacted

    do_collection(true,                      // full
                  true,                      // clear_all_soft_refs
                  size,                      // size
                  is_tlab,                   // is_tlab
                  GenCollectedHeap::OldGen); // max_generation
  }

  result = attempt_allocation(size, is_tlab, false /* first_only */);
  if (result != NULL) {
    return result;
  }

  // What else?  We might try synchronous finalization later.  If the total
  // space available is large enough for the allocation, then a more
  // complete compaction phase than we've tried so far might be
  // appropriate.
  return NULL;
}

void GenCollectedHeap::process_roots(StrongRootsScope* scope, ScanningOption so, OopClosure* strong_roots, CLDClosure* strong_cld_closure, CLDClosure* weak_cld_closure, CodeBlobToOopClosure* code_roots) {
  // General roots.
  // _n_termination for _process_strong_tasks should be set up stream
  // in a method not running in a GC worker.  Otherwise the GC worker
  // could be trying to change the termination condition while the task
  // is executing in another GC worker.

  if (!_process_strong_tasks->is_task_claimed(GCH_PS_ClassLoaderDataGraph_oops_do)) {
    ClassLoaderDataGraph::roots_cld_do(strong_cld_closure, weak_cld_closure);
  }

  // Only process code roots from thread stacks if we aren't visiting the entire CodeCache anyway
  CodeBlobToOopClosure* roots_from_code_p = (so & SO_AllCodeCache) ? NULL : code_roots;

  bool is_par = scope->n_threads() > 1;
  Threads::possibly_parallel_oops_do(is_par, strong_roots, roots_from_code_p);

  if (!_process_strong_tasks->is_task_claimed(GCH_PS_Universe_oops_do)) {
    Universe::oops_do(strong_roots);
  }
  // Global (strong) JNI handles
  if (!_process_strong_tasks->is_task_claimed(GCH_PS_JNIHandles_oops_do)) {
    JNIHandles::oops_do(strong_roots);
  }

  if (!_process_strong_tasks->is_task_claimed(GCH_PS_ObjectSynchronizer_oops_do)) {
    ObjectSynchronizer::oops_do(strong_roots);
  }
  if (!_process_strong_tasks->is_task_claimed(GCH_PS_Management_oops_do)) {
    Management::oops_do(strong_roots);
  }
  if (UseAOT && !_process_strong_tasks->is_task_claimed(GCH_PS_aot_oops_do)) {
    AOTLoader::oops_do(strong_roots);
  }

  if (!_process_strong_tasks->is_task_claimed(GCH_PS_SystemDictionary_oops_do)) {
    SystemDictionary::oops_do(strong_roots);
  }

  if (!_process_strong_tasks->is_task_claimed(GCH_PS_CodeCache_oops_do)) {
    if (so & SO_ScavengeCodeCache) {

      // We only visit parts of the CodeCache when scavenging.
      CodeCache::scavenge_root_nmethods_do(code_roots);
    }
    if (so & SO_AllCodeCache) {

      // CMSCollector uses this to do intermediate-strength collections.
      // We scan the entire code cache, since CodeCache::do_unloading is not called.
      CodeCache::blobs_do(code_roots);
    }
  }
}

void GenCollectedHeap::process_string_table_roots(StrongRootsScope* scope, OopClosure* root_closure, OopStorage::ParState<false, false>* par_state_string) {
  // All threads execute the following. A specific chunk of buckets
  // from the StringTable are the individual tasks.

  if (scope->n_threads() > 1) {
    StringTable::possibly_parallel_oops_do(par_state_string, root_closure);
  } else {
    StringTable::oops_do(root_closure);
  }
}

void GenCollectedHeap::young_process_roots(StrongRootsScope* scope, OopsInGenClosure* root_closure, OopsInGenClosure* old_gen_closure, CLDClosure* cld_closure, OopStorage::ParState<false, false>* par_state_string) {
  MarkingCodeBlobClosure mark_code_closure(root_closure, CodeBlobToOopClosure::FixRelocations);

  process_roots(scope, SO_ScavengeCodeCache, root_closure,
                cld_closure, cld_closure, &mark_code_closure);
  process_string_table_roots(scope, root_closure, par_state_string);

  if (!_process_strong_tasks->is_task_claimed(GCH_PS_younger_gens)) {
    root_closure->reset_generation();
  }

  // When collection is parallel, all threads get to cooperate to do
  // old generation scanning.
  old_gen_closure->set_generation(_old_gen);
  rem_set()->younger_refs_iterate(_old_gen, old_gen_closure, scope->n_threads());
  old_gen_closure->reset_generation();

  _process_strong_tasks->all_tasks_completed(scope->n_threads());
}

void GenCollectedHeap::full_process_roots(StrongRootsScope* scope, bool is_adjust_phase, ScanningOption so, bool only_strong_roots, OopsInGenClosure* root_closure, CLDClosure* cld_closure, OopStorage::ParState<false, false>* par_state_string) {
  MarkingCodeBlobClosure mark_code_closure(root_closure, is_adjust_phase);
  CLDClosure* weak_cld_closure = only_strong_roots ? NULL : cld_closure;

  process_roots(scope, so, root_closure, cld_closure, weak_cld_closure, &mark_code_closure);
  if (is_adjust_phase) {
    // We never treat the string table as roots during marking
    // for the full gc, so we only need to process it during
    // the adjust phase.
    process_string_table_roots(scope, root_closure, par_state_string);
  }

  _process_strong_tasks->all_tasks_completed(scope->n_threads());
}

void GenCollectedHeap::gen_process_weak_roots(OopClosure* root_closure) {
  WeakProcessor::oops_do(root_closure);
  _young_gen->ref_processor()->weak_oops_do(root_closure);
  _old_gen->ref_processor()->weak_oops_do(root_closure);
}

bool GenCollectedHeap::no_allocs_since_save_marks() {
  return _young_gen->no_allocs_since_save_marks() && _old_gen->no_allocs_since_save_marks();
}

bool GenCollectedHeap::supports_inline_contig_alloc() const {
  return _young_gen->supports_inline_contig_alloc();
}

HeapWord* volatile* GenCollectedHeap::top_addr() const {
  return _young_gen->top_addr();
}

HeapWord** GenCollectedHeap::end_addr() const {
  return _young_gen->end_addr();
}

// public collection interfaces

void GenCollectedHeap::collect(GCCause::Cause cause) {
  if (cause == GCCause::_wb_young_gc) {
    // Young collection for the WhiteBox API.
    collect(cause, YoungGen);
  } else {
    // Stop-the-world full collection.
    collect(cause, OldGen);
  }
}

void GenCollectedHeap::collect(GCCause::Cause cause, GenerationType max_generation) {
  // The caller doesn't have the Heap_lock
  MutexLocker ml(Heap_lock);
  collect_locked(cause, max_generation);
}

void GenCollectedHeap::collect_locked(GCCause::Cause cause) {
  // The caller has the Heap_lock
  collect_locked(cause, OldGen);
}

// this is the private collection interface
// The Heap_lock is expected to be held on entry.

void GenCollectedHeap::collect_locked(GCCause::Cause cause, GenerationType max_generation) {
  // Read the GC count while holding the Heap_lock
  unsigned int gc_count_before      = total_collections();
  unsigned int full_gc_count_before = total_full_collections();
  {
    MutexUnlocker mu(Heap_lock);  // give up heap lock, execute gets it back
    VM_GenCollectFull op(gc_count_before, full_gc_count_before,
                         cause, max_generation);
    VMThread::execute(&op);
  }
}

void GenCollectedHeap::do_full_collection(bool clear_all_soft_refs) {
   do_full_collection(clear_all_soft_refs, OldGen);
}

void GenCollectedHeap::do_full_collection(bool clear_all_soft_refs, GenerationType last_generation) {
  GenerationType local_last_generation;
  if (!incremental_collection_will_fail(false /* don't consult_young */) && gc_cause() == GCCause::_gc_locker) {
    local_last_generation = YoungGen;
  } else {
    local_last_generation = last_generation;
  }

  do_collection(true,                   // full
                clear_all_soft_refs,    // clear_all_soft_refs
                0,                      // size
                false,                  // is_tlab
                local_last_generation); // last_generation
  // Hack XXX FIX ME !!!
  // A scavenge may not have been attempted, or may have
  // been attempted and failed, because the old gen was too full
  if (local_last_generation == YoungGen && gc_cause() == GCCause::_gc_locker && incremental_collection_will_fail(false /* don't consult_young */)) {
    log_debug(gc, jni)("GC locker: Trying a full collection because scavenge failed");
    // This time allow the old gen to be collected as well
    do_collection(true,                // full
                  clear_all_soft_refs, // clear_all_soft_refs
                  0,                   // size
                  false,               // is_tlab
                  OldGen);             // last_generation
  }
}

bool GenCollectedHeap::is_in_young(oop p) {
  bool result = ((HeapWord*)p) < _old_gen->reserved().start();
  return result;
}

// Returns "TRUE" iff "p" points into the committed areas of the heap.
bool GenCollectedHeap::is_in(const void* p) const {
  return _young_gen->is_in(p) || _old_gen->is_in(p);
}

void GenCollectedHeap::oop_iterate(OopIterateClosure* cl) {
  _young_gen->oop_iterate(cl);
  _old_gen->oop_iterate(cl);
}

void GenCollectedHeap::object_iterate(ObjectClosure* cl) {
  _young_gen->object_iterate(cl);
  _old_gen->object_iterate(cl);
}

void GenCollectedHeap::safe_object_iterate(ObjectClosure* cl) {
  _young_gen->safe_object_iterate(cl);
  _old_gen->safe_object_iterate(cl);
}

Space* GenCollectedHeap::space_containing(const void* addr) const {
  Space* res = _young_gen->space_containing(addr);
  if (res != NULL) {
    return res;
  }
  res = _old_gen->space_containing(addr);
  return res;
}

HeapWord* GenCollectedHeap::block_start(const void* addr) const {
  if (_young_gen->is_in_reserved(addr)) {
    return _young_gen->block_start(addr);
  }

  return _old_gen->block_start(addr);
}

size_t GenCollectedHeap::block_size(const HeapWord* addr) const {
  if (_young_gen->is_in_reserved(addr)) {
    return _young_gen->block_size(addr);
  }

  return _old_gen->block_size(addr);
}

bool GenCollectedHeap::block_is_obj(const HeapWord* addr) const {
  if (_young_gen->is_in_reserved(addr)) {
    return _young_gen->block_is_obj(addr);
  }

  return _old_gen->block_is_obj(addr);
}

bool GenCollectedHeap::supports_tlab_allocation() const {
  return _young_gen->supports_tlab_allocation();
}

size_t GenCollectedHeap::tlab_capacity(Thread* thr) const {
  if (_young_gen->supports_tlab_allocation()) {
    return _young_gen->tlab_capacity();
  }
  return 0;
}

size_t GenCollectedHeap::tlab_used(Thread* thr) const {
  if (_young_gen->supports_tlab_allocation()) {
    return _young_gen->tlab_used();
  }
  return 0;
}

size_t GenCollectedHeap::unsafe_max_tlab_alloc(Thread* thr) const {
  if (_young_gen->supports_tlab_allocation()) {
    return _young_gen->unsafe_max_tlab_alloc();
  }
  return 0;
}

HeapWord* GenCollectedHeap::allocate_new_tlab(size_t min_size,
                                              size_t requested_size,
                                              size_t* actual_size) {
  bool gc_overhead_limit_was_exceeded;
  HeapWord* result = mem_allocate_work(requested_size /* size */,
                                       true /* is_tlab */,
                                       &gc_overhead_limit_was_exceeded);
  if (result != NULL) {
    *actual_size = requested_size;
  }

  return result;
}

// Requires "*prev_ptr" to be non-NULL.  Deletes and a block of minimal size
// from the list headed by "*prev_ptr".
static ScratchBlock *removeSmallestScratch(ScratchBlock **prev_ptr) {
  bool first = true;
  size_t min_size = 0;   // "first" makes this conceptually infinite.
  ScratchBlock **smallest_ptr, *smallest;
  ScratchBlock  *cur = *prev_ptr;
  while (cur) {
    if (first || cur->num_words < min_size) {
      smallest_ptr = prev_ptr;
      smallest     = cur;
      min_size     = smallest->num_words;
      first        = false;
    }
    prev_ptr = &cur->next;
    cur     =  cur->next;
  }
  smallest      = *smallest_ptr;
  *smallest_ptr = smallest->next;
  return smallest;
}

// Sort the scratch block list headed by res into decreasing size order,
// and set "res" to the result.
static void sort_scratch_list(ScratchBlock*& list) {
  ScratchBlock* sorted = NULL;
  ScratchBlock* unsorted = list;
  while (unsorted) {
    ScratchBlock *smallest = removeSmallestScratch(&unsorted);
    smallest->next  = sorted;
    sorted          = smallest;
  }
  list = sorted;
}

ScratchBlock* GenCollectedHeap::gather_scratch(Generation* requestor,
                                               size_t max_alloc_words) {
  ScratchBlock* res = NULL;
  _young_gen->contribute_scratch(res, requestor, max_alloc_words);
  _old_gen->contribute_scratch(res, requestor, max_alloc_words);
  sort_scratch_list(res);
  return res;
}

void GenCollectedHeap::release_scratch() {
  _young_gen->reset_scratch();
  _old_gen->reset_scratch();
}

class GenPrepareForVerifyClosure: public GenCollectedHeap::GenClosure {
  void do_generation(Generation* gen) {
    gen->prepare_for_verify();
  }
};

void GenCollectedHeap::prepare_for_verify() {
  ensure_parsability(false);        // no need to retire TLABs
  GenPrepareForVerifyClosure blk;
  generation_iterate(&blk, false);
}

void GenCollectedHeap::generation_iterate(GenClosure* cl, bool old_to_young) {
  if (old_to_young) {
    cl->do_generation(_old_gen);
    cl->do_generation(_young_gen);
  } else {
    cl->do_generation(_young_gen);
    cl->do_generation(_old_gen);
  }
}

bool GenCollectedHeap::is_maximal_no_gc() const {
  return _young_gen->is_maximal_no_gc() && _old_gen->is_maximal_no_gc();
}

void GenCollectedHeap::save_marks() {
  _young_gen->save_marks();
  _old_gen->save_marks();
}

GenCollectedHeap* GenCollectedHeap::heap() {
  CollectedHeap* heap = Universe::heap();
  return (GenCollectedHeap*) heap;
}

void GenCollectedHeap::verify(VerifyOption option /* ignored */) {
  log_debug(gc, verify)("%s", _old_gen->name());
  _old_gen->verify();

  log_debug(gc, verify)("%s", _old_gen->name());
  _young_gen->verify();

  log_debug(gc, verify)("RemSet");
  rem_set()->verify();
}

void GenCollectedHeap::print_on(outputStream* st) const {
  _young_gen->print_on(st);
  _old_gen->print_on(st);
  MetaspaceUtils::print_on(st);
}

void GenCollectedHeap::gc_threads_do(ThreadClosure* tc) const {
}

void GenCollectedHeap::print_gc_threads_on(outputStream* st) const {
}

void GenCollectedHeap::print_tracing_info() const {
  if (log_is_enabled(Debug, gc, heap, exit)) {
    LogStreamHandle(Debug, gc, heap, exit) lsh;
    _young_gen->print_summary_info_on(&lsh);
    _old_gen->print_summary_info_on(&lsh);
  }
}

void GenCollectedHeap::print_heap_change(size_t young_prev_used, size_t old_prev_used) const {
  log_info(gc, heap)("%s: " SIZE_FORMAT "K->" SIZE_FORMAT "K("  SIZE_FORMAT "K)",
                     _young_gen->short_name(), young_prev_used / K, _young_gen->used() /K, _young_gen->capacity() /K);
  log_info(gc, heap)("%s: " SIZE_FORMAT "K->" SIZE_FORMAT "K("  SIZE_FORMAT "K)",
                     _old_gen->short_name(), old_prev_used / K, _old_gen->used() /K, _old_gen->capacity() /K);
}

class GenGCPrologueClosure: public GenCollectedHeap::GenClosure {
 private:
  bool _full;
 public:
  void do_generation(Generation* gen) {
    gen->gc_prologue(_full);
  }
  GenGCPrologueClosure(bool full) : _full(full) { };
};

void GenCollectedHeap::gc_prologue(bool full) {

  // Fill TLAB's and such
  CollectedHeap::accumulate_statistics_all_tlabs();
  ensure_parsability(true);   // retire TLABs

  // Walk generations
  GenGCPrologueClosure blk(full);
  generation_iterate(&blk, false);  // not old-to-young.
};

class GenGCEpilogueClosure: public GenCollectedHeap::GenClosure {
 private:
  bool _full;
 public:
  void do_generation(Generation* gen) {
    gen->gc_epilogue(_full);
  }
  GenGCEpilogueClosure(bool full) : _full(full) { };
};

void GenCollectedHeap::gc_epilogue(bool full) {
  size_t actual_gap = pointer_delta((HeapWord*) (max_uintx-3), *(end_addr()));
  guarantee(is_client_compilation_mode_vm() || actual_gap > (size_t)FastAllocateSizeLimit, "inline allocation wraps");

  resize_all_tlabs();

  GenGCEpilogueClosure blk(full);
  generation_iterate(&blk, false);  // not old-to-young.

  if (!CleanChunkPoolAsync) {
    Chunk::clean_chunk_pool();
  }

  MetaspaceCounters::update_performance_counters();
  CompressedClassSpaceCounters::update_performance_counters();
};

class GenEnsureParsabilityClosure: public GenCollectedHeap::GenClosure {
 public:
  void do_generation(Generation* gen) {
    gen->ensure_parsability();
  }
};

void GenCollectedHeap::ensure_parsability(bool retire_tlabs) {
  CollectedHeap::ensure_parsability(retire_tlabs);
  GenEnsureParsabilityClosure ep_cl;
  generation_iterate(&ep_cl, false);
}

oop GenCollectedHeap::handle_failed_promotion(Generation* old_gen,
                                              oop obj,
                                              size_t obj_size) {
  guarantee(old_gen == _old_gen, "We only get here with an old generation");
  HeapWord* result = NULL;

  result = old_gen->expand_and_allocate(obj_size, false);

  if (result != NULL) {
    Copy::aligned_disjoint_words((HeapWord*)obj, result, obj_size);
  }
  return oop(result);
}

class GenTimeOfLastGCClosure: public GenCollectedHeap::GenClosure {
  jlong _time;   // in ms
  jlong _now;    // in ms

 public:
  GenTimeOfLastGCClosure(jlong now) : _time(now), _now(now) { }

  jlong time() { return _time; }

  void do_generation(Generation* gen) {
    _time = MIN2(_time, gen->time_of_last_gc(_now));
  }
};

jlong GenCollectedHeap::millis_since_last_gc() {
  // javaTimeNanos() is guaranteed to be monotonically non-decreasing
  // provided the underlying platform provides such a time source
  // (and it is bug free). So we still have to guard against getting
  // back a time later than 'now'.
  jlong now = os::javaTimeNanos() / NANOSECS_PER_MILLISEC;
  GenTimeOfLastGCClosure tolgc_cl(now);
  // iterate over generations getting the oldest
  // time that a generation was collected
  generation_iterate(&tolgc_cl, false);

  jlong retVal = now - tolgc_cl.time();
  if (retVal < 0) {
    log_warning(gc)("millis_since_last_gc() would return : " JLONG_FORMAT ". returning zero instead.", retVal);
    return 0;
  }
  return retVal;
}

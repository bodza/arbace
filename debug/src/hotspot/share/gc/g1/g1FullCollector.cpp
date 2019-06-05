#include "precompiled.hpp"

#include "code/codeCache.hpp"
#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1CollectorPolicy.hpp"
#include "gc/g1/g1FullCollector.hpp"
#include "gc/g1/g1FullGCAdjustTask.hpp"
#include "gc/g1/g1FullGCCompactTask.hpp"
#include "gc/g1/g1FullGCMarker.inline.hpp"
#include "gc/g1/g1FullGCMarkTask.hpp"
#include "gc/g1/g1FullGCPrepareTask.hpp"
#include "gc/g1/g1FullGCReferenceProcessorExecutor.hpp"
#include "gc/g1/g1FullGCScope.hpp"
#include "gc/g1/g1OopClosures.hpp"
#include "gc/g1/g1Policy.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "gc/shared/adaptiveSizePolicy.hpp"
#include "gc/shared/preservedMarks.hpp"
#include "gc/shared/referenceProcessor.hpp"
#include "gc/shared/weakProcessor.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/handles.inline.hpp"
#include "utilities/debug.hpp"

static void clear_and_activate_derived_pointers() {
  DerivedPointerTable::clear();
}

static void deactivate_derived_pointers() {
  DerivedPointerTable::set_active(false);
}

static void update_derived_pointers() {
  DerivedPointerTable::update_pointers();
}

G1CMBitMap* G1FullCollector::mark_bitmap() {
  return _heap->concurrent_mark()->next_mark_bitmap();
}

ReferenceProcessor* G1FullCollector::reference_processor() {
  return _heap->ref_processor_stw();
}

uint G1FullCollector::calc_active_workers() {
  G1CollectedHeap* heap = G1CollectedHeap::heap();
  uint max_worker_count = heap->workers()->total_workers();
  // Only calculate number of workers if UseDynamicNumberOfGCThreads
  // is enabled, otherwise use max.
  if (!UseDynamicNumberOfGCThreads) {
    return max_worker_count;
  }

  // Consider G1HeapWastePercent to decide max number of workers. Each worker
  // will in average cause half a region waste.
  uint max_wasted_regions_allowed = ((heap->num_regions() * G1HeapWastePercent) / 100);
  uint waste_worker_count = MAX2((max_wasted_regions_allowed * 2) , 1u);
  uint heap_waste_worker_limit = MIN2(waste_worker_count, max_worker_count);

  // Also consider HeapSizePerGCThread by calling AdaptiveSizePolicy to calculate
  // the number of workers.
  uint current_active_workers = heap->workers()->active_workers();
  uint adaptive_worker_limit = AdaptiveSizePolicy::calc_active_workers(max_worker_count, current_active_workers, 0);

  // Update active workers to the lower of the limits.
  uint worker_count = MIN2(heap_waste_worker_limit, adaptive_worker_limit);
  worker_count = heap->workers()->update_active_workers(worker_count);

  return worker_count;
}

G1FullCollector::G1FullCollector(G1CollectedHeap* heap, GCMemoryManager* memory_manager, bool explicit_gc, bool clear_soft_refs) :
    _heap(heap),
    _scope(memory_manager, explicit_gc, clear_soft_refs),
    _num_workers(calc_active_workers()),
    _oop_queue_set(_num_workers),
    _array_queue_set(_num_workers),
    _preserved_marks_set(true),
    _serial_compaction_point(),
    _is_alive(heap->concurrent_mark()->next_mark_bitmap()),
    _is_alive_mutator(heap->ref_processor_stw(), &_is_alive),
    _always_subject_to_discovery(),
    _is_subject_mutator(heap->ref_processor_stw(), &_always_subject_to_discovery) {
  _preserved_marks_set.init(_num_workers);
  _markers = NEW_C_HEAP_ARRAY(G1FullGCMarker*, _num_workers, mtGC);
  _compaction_points = NEW_C_HEAP_ARRAY(G1FullGCCompactionPoint*, _num_workers, mtGC);
  for (uint i = 0; i < _num_workers; i++) {
    _markers[i] = new G1FullGCMarker(i, _preserved_marks_set.get(i), mark_bitmap());
    _compaction_points[i] = new G1FullGCCompactionPoint();
    _oop_queue_set.register_queue(i, marker(i)->oop_stack());
    _array_queue_set.register_queue(i, marker(i)->objarray_stack());
  }
}

G1FullCollector::~G1FullCollector() {
  for (uint i = 0; i < _num_workers; i++) {
    delete _markers[i];
    delete _compaction_points[i];
  }
  FREE_C_HEAP_ARRAY(G1FullGCMarker*, _markers);
  FREE_C_HEAP_ARRAY(G1FullGCCompactionPoint*, _compaction_points);
}

void G1FullCollector::prepare_collection() {
  _heap->g1_policy()->record_full_collection_start();

  _heap->print_heap_regions();

  _heap->abort_concurrent_cycle();

  _heap->gc_prologue(true);
  _heap->prepare_heap_for_full_collection();

  reference_processor()->enable_discovery();
  reference_processor()->setup_policy(scope()->should_clear_soft_refs());

  // When collecting the permanent generation Method*s may be moving,
  // so we either have to flush all bcp data or convert it into bci.
  CodeCache::gc_prologue();

  // We should save the marks of the currently locked biased monitors.
  // The marking doesn't preserve the marks of biased objects.
  BiasedLocking::preserve_marks();

  // Clear and activate derived pointer collection.
  clear_and_activate_derived_pointers();
}

void G1FullCollector::collect() {
  phase1_mark_live_objects();

  // Don't add any more derived pointers during later phases
  deactivate_derived_pointers();

  phase2_prepare_compaction();
  phase3_adjust_pointers();
  phase4_do_compaction();
}

void G1FullCollector::complete_collection() {
  // Restore all marks.
  restore_marks();

  // When the pointers have been adjusted and moved, we can
  // update the derived pointer table.
  update_derived_pointers();

  BiasedLocking::restore_marks();
  CodeCache::gc_epilogue();

  _heap->prepare_heap_for_mutators();

  _heap->g1_policy()->record_full_collection_end();
  _heap->gc_epilogue(true);

  _heap->print_heap_after_full_collection(scope()->heap_transition());
}

void G1FullCollector::phase1_mark_live_objects() {
  // Recursively traverse all live objects and mark them.

  // Do the actual marking.
  G1FullGCMarkTask marking_task(this);
  run_task(&marking_task);

  // Process references discovered during marking.
  G1FullGCReferenceProcessingExecutor reference_processing(this);
  reference_processing.execute(scope()->timer());

  // Weak oops cleanup.
  WeakProcessor::weak_oops_do(&_is_alive, &do_nothing_cl);

  // Class unloading and cleanup.
  if (ClassUnloading) {
    // Unload classes and purge the SystemDictionary.
    bool purged_class = SystemDictionary::do_unloading(scope()->timer());
    _heap->complete_cleaning(&_is_alive, purged_class);
  } else {
    // If no class unloading just clean out strings and symbols.
    _heap->partial_cleaning(&_is_alive, true, true, G1StringDedup::is_enabled());
  }
}

void G1FullCollector::phase2_prepare_compaction() {
  G1FullGCPrepareTask task(this);
  run_task(&task);

  // To avoid OOM when there is memory left.
  if (!task.has_freed_regions()) {
    task.prepare_serial_compaction();
  }
}

void G1FullCollector::phase3_adjust_pointers() {
  // Adjust the pointers to reflect the new locations
  G1FullGCAdjustTask task(this);
  run_task(&task);
}

void G1FullCollector::phase4_do_compaction() {
  // Compact the heap using the compaction queues created in phase 2.
  G1FullGCCompactTask task(this);
  run_task(&task);

  // Serial compact to avoid OOM when very few free regions.
  if (serial_compaction_point()->has_regions()) {
    task.serial_compaction();
  }
}

void G1FullCollector::restore_marks() {
  SharedRestorePreservedMarksTaskExecutor task_executor(_heap->workers());
  _preserved_marks_set.restore(&task_executor);
  _preserved_marks_set.reclaim();
}

void G1FullCollector::run_task(AbstractGangTask* task) {
  _heap->workers()->run_task(task, _num_workers);
}

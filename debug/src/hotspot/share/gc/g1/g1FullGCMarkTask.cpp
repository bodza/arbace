#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1FullCollector.hpp"
#include "gc/g1/g1FullGCMarker.hpp"
#include "gc/g1/g1FullGCMarkTask.hpp"
#include "gc/g1/g1FullGCOopClosures.inline.hpp"
#include "gc/g1/g1FullGCReferenceProcessorExecutor.hpp"
#include "gc/shared/referenceProcessor.hpp"
#include "memory/iterator.inline.hpp"

G1FullGCMarkTask::G1FullGCMarkTask(G1FullCollector* collector) :
    G1FullGCTask("G1 Parallel Marking Task", collector),
    _root_processor(G1CollectedHeap::heap(), collector->workers()),
    _terminator(collector->workers(), collector->array_queue_set()) {
  // Need cleared claim bits for the roots processing
  ClassLoaderDataGraph::clear_claimed_marks();
}

void G1FullGCMarkTask::work(uint worker_id) {
  Ticks start = Ticks::now();
  ResourceMark rm;
  G1FullGCMarker* marker = collector()->marker(worker_id);
  MarkingCodeBlobClosure code_closure(marker->mark_closure(), !CodeBlobToOopClosure::FixRelocations);

  if (ClassUnloading) {
    _root_processor.process_strong_roots(marker->mark_closure(), marker->cld_closure(), &code_closure);
  } else {
    _root_processor.process_all_roots_no_string_table(marker->mark_closure(), marker->cld_closure(), &code_closure);
  }

  // Mark stack is populated, now process and drain it.
  marker->complete_marking(collector()->oop_queue_set(), collector()->array_queue_set(), &_terminator);

  // This is the point where the entire marking should have completed.
  log_task("Marking task", worker_id, start);
}

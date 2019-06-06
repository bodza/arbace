#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1FullCollector.hpp"
#include "gc/g1/g1FullGCMarker.hpp"
#include "gc/g1/g1FullGCOopClosures.inline.hpp"
#include "gc/g1/g1FullGCReferenceProcessorExecutor.hpp"
#include "gc/shared/referenceProcessor.hpp"
#include "gc/shared/referenceProcessorPhaseTimes.hpp"
#include "memory/iterator.inline.hpp"

G1FullGCReferenceProcessingExecutor::G1FullGCReferenceProcessingExecutor(G1FullCollector* collector) :
    _collector(collector),
    _reference_processor(collector->reference_processor()),
    _old_mt_degree(_reference_processor->num_queues()) {
  if (_reference_processor->processing_is_mt()) {
    _reference_processor->set_active_mt_degree(_collector->workers());
  }
}

G1FullGCReferenceProcessingExecutor::~G1FullGCReferenceProcessingExecutor() {
  if (_reference_processor->processing_is_mt()) {
    _reference_processor->set_active_mt_degree(_old_mt_degree);
  }
}

G1FullGCReferenceProcessingExecutor::G1RefProcTaskProxy::G1RefProcTaskProxy(ProcessTask& proc_task, G1FullCollector* collector) :
     AbstractGangTask("G1 reference processing task"),
     _proc_task(proc_task),
     _collector(collector),
     _terminator(_collector->workers(), _collector->oop_queue_set()) { }

void G1FullGCReferenceProcessingExecutor::G1RefProcTaskProxy::work(uint worker_id) {
  G1FullGCMarker* marker = _collector->marker(worker_id);
  G1IsAliveClosure is_alive(_collector->mark_bitmap());
  G1FullKeepAliveClosure keep_alive(marker);
  _proc_task.work(worker_id, is_alive, keep_alive, *marker->stack_closure());
}

void G1FullGCReferenceProcessingExecutor::run_task(AbstractGangTask* task) {
  G1CollectedHeap::heap()->workers()->run_task(task, _collector->workers());
}

void G1FullGCReferenceProcessingExecutor::run_task(AbstractGangTask* task, uint workers) {
  G1CollectedHeap::heap()->workers()->run_task(task, workers);
}

void G1FullGCReferenceProcessingExecutor::execute(ProcessTask& proc_task, uint ergo_workers) {
  G1RefProcTaskProxy proc_task_proxy(proc_task, _collector);
  run_task(&proc_task_proxy, ergo_workers);
}

void G1FullGCReferenceProcessingExecutor::execute(STWGCTimer* timer) {
  // Process reference objects found during marking.
  G1FullGCMarker* marker = _collector->marker(0);
  G1IsAliveClosure is_alive(_collector->mark_bitmap());
  G1FullKeepAliveClosure keep_alive(marker);
  ReferenceProcessorPhaseTimes pt(timer, _reference_processor->max_num_queues());
  AbstractRefProcTaskExecutor* executor = _reference_processor->processing_is_mt() ? this : NULL;

  // Process discovered references, use this executor if multi-threaded
  // processing is enabled.
  const ReferenceProcessorStats& stats = _reference_processor->process_discovered_references(&is_alive, &keep_alive, marker->stack_closure(), executor, &pt);

  pt.print_all_references();
}

#include "precompiled.hpp"
#include "gc/g1/g1FullGCScope.hpp"

G1FullGCScope::G1FullGCScope(GCMemoryManager* memory_manager, bool explicit_gc, bool clear_soft) :
    _rm(),
    _explicit_gc(explicit_gc),
    _g1h(G1CollectedHeap::heap()),
    _gc_id(),
    _svc_marker(SvcGCMarker::FULL),
    _timer(),
    _tracer(),
    _active(),
    _cpu_time(),
    _soft_refs(clear_soft, _g1h->soft_ref_policy()),
    _memory_stats(memory_manager, _g1h->gc_cause()),
    _collector_stats(_g1h->g1mm()->full_collection_counters()),
    _heap_transition(_g1h) {
  _timer.register_gc_start();
  _tracer.report_gc_start(_g1h->gc_cause(), _timer.gc_start());
  _g1h->pre_full_gc_dump(&_timer);
  _g1h->trace_heap_before_gc(&_tracer);
}

G1FullGCScope::~G1FullGCScope() {
  // We must call G1MonitoringSupport::update_sizes() in the same scoping level
  // as an active TraceMemoryManagerStats object (i.e. before the destructor for the
  // TraceMemoryManagerStats is called) so that the G1 memory pools are updated
  // before any GC notifications are raised.
  _g1h->g1mm()->update_sizes();
  _g1h->trace_heap_after_gc(&_tracer);
  _g1h->post_full_gc_dump(&_timer);
  _timer.register_gc_end();
  _tracer.report_gc_end(_timer.gc_end(), _timer.time_partitions());
}

bool G1FullGCScope::is_explicit_gc() {
  return _explicit_gc;
}

bool G1FullGCScope::should_clear_soft_refs() {
  return _soft_refs.should_clear();
}

STWGCTimer* G1FullGCScope::timer() {
  return &_timer;
}

G1FullGCTracer* G1FullGCScope::tracer() {
  return &_tracer;
}

G1HeapTransition* G1FullGCScope::heap_transition() {
  return &_heap_transition;
}

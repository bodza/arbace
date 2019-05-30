#ifndef SHARE_VM_GC_G1_G1FULLGCSCOPE_HPP
#define SHARE_VM_GC_G1_G1FULLGCSCOPE_HPP

#include "gc/g1/g1CollectedHeap.hpp"
#include "gc/g1/g1HeapTransition.hpp"
#include "gc/shared/collectorCounters.hpp"
#include "gc/shared/gcId.hpp"
#include "gc/shared/gcTrace.hpp"
#include "gc/shared/gcTraceTime.hpp"
#include "gc/shared/gcTimer.hpp"
#include "gc/shared/isGCActiveMark.hpp"
#include "gc/shared/vmGCOperations.hpp"
#include "memory/allocation.hpp"
#include "services/memoryService.hpp"

class GCMemoryManager;

// Class used to group scoped objects used in the Full GC together.
class G1FullGCScope : public StackObj {
  ResourceMark            _rm;
  bool                    _explicit_gc;
  G1CollectedHeap*        _g1h;
  GCIdMark                _gc_id;
  SvcGCMarker             _svc_marker;
  STWGCTimer              _timer;
  G1FullGCTracer          _tracer;
  IsGCActiveMark          _active;
  GCTraceCPUTime          _cpu_time;
  ClearedAllSoftRefs      _soft_refs;
  TraceCollectorStats     _collector_stats;
  TraceMemoryManagerStats _memory_stats;
  G1HeapTransition        _heap_transition;

public:
  G1FullGCScope(GCMemoryManager* memory_manager, bool explicit_gc, bool clear_soft);
  ~G1FullGCScope();

  bool is_explicit_gc();
  bool should_clear_soft_refs();

  STWGCTimer* timer();
  G1FullGCTracer* tracer();
  G1HeapTransition* heap_transition();
};

#endif

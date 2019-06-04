#ifndef SHARE_VM_GC_G1_VM_OPERATIONS_G1_HPP
#define SHARE_VM_GC_G1_VM_OPERATIONS_G1_HPP

#include "gc/shared/gcId.hpp"
#include "gc/shared/vmGCOperations.hpp"

// VM_operations for the G1 collector.
// VM_GC_Operation:
//   - VM_CGC_Operation
//   - VM_G1CollectForAllocation
//   - VM_G1CollectFull

class VM_G1CollectFull: public VM_GC_Operation {
public:
  VM_G1CollectFull(uint gc_count_before, uint full_gc_count_before, GCCause::Cause cause)
    : VM_GC_Operation(gc_count_before, cause, full_gc_count_before, true) { }
  virtual VMOp_Type type() const { return VMOp_G1CollectFull; }
  virtual void doit();
  virtual const char* name() const {
    return "G1 Full collection";
  }
};

class VM_G1CollectForAllocation: public VM_CollectForAllocation {
private:
  bool      _pause_succeeded;

  bool         _should_initiate_conc_mark;
  bool         _should_retry_gc;
  double       _target_pause_time_ms;
  uint         _old_marking_cycles_completed_before;
public:
  VM_G1CollectForAllocation(size_t         word_size,
                            uint           gc_count_before,
                            GCCause::Cause gc_cause,
                            bool           should_initiate_conc_mark,
                            double         target_pause_time_ms);
  virtual VMOp_Type type() const { return VMOp_G1CollectForAllocation; }
  virtual bool doit_prologue();
  virtual void doit();
  virtual void doit_epilogue();
  virtual const char* name() const {
    return "G1 collect for allocation";
  }
  bool should_retry_gc() const { return _should_retry_gc; }
  bool pause_succeeded() { return _pause_succeeded; }
};

// Concurrent GC stop-the-world operations such as remark and cleanup;
// consider sharing these with CMS's counterparts.
class VM_CGC_Operation: public VM_Operation {
  VoidClosure* _cl;
  const char*  _printGCMessage;
  uint         _gc_id;

public:
  VM_CGC_Operation(VoidClosure* cl, const char *printGCMsg)
    : _cl(cl), _printGCMessage(printGCMsg), _gc_id(GCId::current()) { }
  virtual VMOp_Type type() const { return VMOp_CGC_Operation; }
  virtual void doit();
  virtual bool doit_prologue();
  virtual void doit_epilogue();
  virtual const char* name() const {
    return "concurrent gc";
  }
};

#endif

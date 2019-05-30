#ifndef SHARE_VM_GC_SHARED_SOFTREFPOLICY_HPP
#define SHARE_VM_GC_SHARED_SOFTREFPOLICY_HPP

#include "memory/allocation.hpp"

class SoftRefPolicy {
 private:
  // Set to true when policy wants soft refs cleared.
  // Reset to false by gc after it clears all soft refs.
  bool _should_clear_all_soft_refs;

  // Set to true by the GC if the just-completed gc cleared all
  // softrefs.  This is set to true whenever a gc clears all softrefs, and
  // set to false each time gc returns to the mutator.  For example, in the
  // ParallelScavengeHeap case the latter would be done toward the end of
  // mem_allocate() where it returns op.result()
  bool _all_soft_refs_clear;

 public:
  SoftRefPolicy();

  bool should_clear_all_soft_refs() { return _should_clear_all_soft_refs; }
  void set_should_clear_all_soft_refs(bool v) { _should_clear_all_soft_refs = v; }
  // Returns the current value of _should_clear_all_soft_refs.
  // _should_clear_all_soft_refs is set to false as a side effect.
  bool use_should_clear_all_soft_refs(bool v);
  bool all_soft_refs_clear() { return _all_soft_refs_clear; }
  void set_all_soft_refs_clear(bool v) { _all_soft_refs_clear = v; }

  // Called by the GC after Soft Refs have been cleared to indicate
  // that the request in _should_clear_all_soft_refs has been fulfilled.
  virtual void cleared_all_soft_refs();
};

class ClearedAllSoftRefs : public StackObj {
  bool           _clear_all_soft_refs;
  SoftRefPolicy* _soft_ref_policy;
 public:
  ClearedAllSoftRefs(bool clear_all_soft_refs, SoftRefPolicy* soft_ref_policy) :
    _clear_all_soft_refs(clear_all_soft_refs),
    _soft_ref_policy(soft_ref_policy) {}

  ~ClearedAllSoftRefs() {
    if (_clear_all_soft_refs) {
      _soft_ref_policy->cleared_all_soft_refs();
    }
  }

  bool should_clear() { return _clear_all_soft_refs; }
};

#endif

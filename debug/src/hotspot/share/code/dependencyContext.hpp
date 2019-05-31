#ifndef SHARE_VM_CODE_DEPENDENCYCONTEXT_HPP
#define SHARE_VM_CODE_DEPENDENCYCONTEXT_HPP

#include "memory/allocation.hpp"
#include "oops/oop.hpp"
#include "runtime/handles.hpp"
#include "runtime/perfData.hpp"
#include "runtime/safepoint.hpp"

class nmethod;
class DepChange;

//
// nmethodBucket is used to record dependent nmethods for
// deoptimization.  nmethod dependencies are actually <klass, method>
// pairs but we really only care about the klass part for purposes of
// finding nmethods which might need to be deoptimized.  Instead of
// recording the method, a count of how many times a particular nmethod
// was recorded is kept.  This ensures that any recording errors are
// noticed since an nmethod should be removed as many times are it's
// added.
//
class nmethodBucket: public CHeapObj<mtClass> {
  friend class VMStructs;
 private:
  nmethod*       _nmethod;
  int            _count;
  nmethodBucket* _next;

 public:
  nmethodBucket(nmethod* nmethod, nmethodBucket* next) :
   _nmethod(nmethod), _next(next), _count(1) { }

  int count()                             { return _count; }
  int increment()                         { _count += 1; return _count; }
  int decrement();
  nmethodBucket* next()                   { return _next; }
  void set_next(nmethodBucket* b)         { _next = b; }
  nmethod* get_nmethod()                  { return _nmethod; }
};

//
// Utility class to manipulate nmethod dependency context.
// The context consists of nmethodBucket* (a head of a linked list)
// and a boolean flag (does the list contains stale entries). The structure is
// encoded as an intptr_t: lower bit is used for the flag. It is possible since
// nmethodBucket* is aligned - the structure is malloc'ed in C heap.
// Dependency context can be attached either to an InstanceKlass (_dep_context field)
// or CallSiteContext oop for call_site_target dependencies (see javaClasses.hpp).
// DependencyContext class operates on some location which holds a intptr_t value.
//
class DependencyContext : public StackObj {
  friend class VMStructs;
  friend class TestDependencyContext;
 private:
  enum TagBits { _has_stale_entries_bit = 1, _has_stale_entries_mask = 1 };

  intptr_t* _dependency_context_addr;

  void set_dependencies(nmethodBucket* b) {
    if (has_stale_entries()) {
      *_dependency_context_addr = intptr_t(b) | _has_stale_entries_mask;
    } else {
      *_dependency_context_addr = intptr_t(b);
    }
  }

  void set_has_stale_entries(bool x) {
    if (x) {
      *_dependency_context_addr |= _has_stale_entries_mask;
    } else {
      *_dependency_context_addr &= ~_has_stale_entries_mask;
    }
  }

  nmethodBucket* dependencies() {
    intptr_t value = *_dependency_context_addr;
    return (nmethodBucket*) (value & ~_has_stale_entries_mask);
  }

  bool has_stale_entries() const {
    intptr_t value = *_dependency_context_addr;
    return (value & _has_stale_entries_mask) != 0;
  }

  static PerfCounter* _perf_total_buckets_allocated_count;
  static PerfCounter* _perf_total_buckets_deallocated_count;
  static PerfCounter* _perf_total_buckets_stale_count;
  static PerfCounter* _perf_total_buckets_stale_acc_count;

 public:
  DependencyContext(intptr_t* addr) : _dependency_context_addr(addr) { }

  static const intptr_t EMPTY = 0; // dependencies = NULL, has_stale_entries = false

  static void init();

  int  mark_dependent_nmethods(DepChange& changes);
  void add_dependent_nmethod(nmethod* nm, bool expunge_stale_entries = false);
  void remove_dependent_nmethod(nmethod* nm, bool expunge_stale_entries = false);
  int  remove_all_dependents();

  void expunge_stale_entries();

  // Unsafe deallocation of nmethodBuckets. Used in IK::release_C_heap_structures
  // to clean up the context possibly containing live entries pointing to unloaded nmethods.
  void wipe();
};
#endif

#ifndef SHARE_VM_GC_SHARED_COPYFAILEDINFO_HPP
#define SHARE_VM_GC_SHARED_COPYFAILEDINFO_HPP

#include "runtime/thread.hpp"
#include "utilities/globalDefinitions.hpp"

class CopyFailedInfo : public CHeapObj<mtGC> {
  size_t    _first_size;
  size_t    _smallest_size;
  size_t    _total_size;
  uint      _count;

 public:
  CopyFailedInfo() : _first_size(0), _smallest_size(0), _total_size(0), _count(0) { }

  virtual void register_copy_failure(size_t size) {
    if (_first_size == 0) {
      _first_size = size;
      _smallest_size = size;
    } else if (size < _smallest_size) {
      _smallest_size = size;
    }
    _total_size += size;
    _count++;
  }

  virtual void reset() {
    _first_size = 0;
    _smallest_size = 0;
    _total_size = 0;
    _count = 0;
  }

  bool has_failed() const { return _count != 0; }
  size_t first_size() const { return _first_size; }
  size_t smallest_size() const { return _smallest_size; }
  size_t total_size() const { return _total_size; }
  uint failed_count() const { return _count; }
};

class PromotionFailedInfo : public CopyFailedInfo {
  traceid _thread_trace_id;

 public:
  PromotionFailedInfo() : CopyFailedInfo(), _thread_trace_id(0) { }

  void register_copy_failure(size_t size) {
    CopyFailedInfo::register_copy_failure(size);
    if (_thread_trace_id == 0) {
      _thread_trace_id = JFR_THREAD_ID(Thread::current());
    }
  }

  void reset() {
    CopyFailedInfo::reset();
    _thread_trace_id = 0;
  }

  traceid thread_trace_id() const { return _thread_trace_id; }
};

class EvacuationFailedInfo : public CopyFailedInfo { };

#endif

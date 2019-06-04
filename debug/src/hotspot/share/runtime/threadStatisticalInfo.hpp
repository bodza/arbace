#ifndef SHARE_VM_RUNTIME_THREADSTATISTICS_HPP
#define SHARE_VM_RUNTIME_THREADSTATISTICS_HPP

#include "jni.h"
#include "runtime/os.hpp"
#include "utilities/globalDefinitions.hpp"

class ThreadStatisticalInfo {
  // The time stamp the thread was started.
  const uint64_t _start_time_stamp;
  uint64_t _define_class_count;

public:
  ThreadStatisticalInfo() : _start_time_stamp(os::javaTimeMillis()), _define_class_count(0) { }
  uint64_t getStartTime() const             { return _start_time_stamp; }
  uint64_t getDefineClassCount() const                    { return _define_class_count; }
  void     setDefineClassCount(uint64_t defineClassCount) { _define_class_count = defineClassCount; }
  void     incr_define_class_count()                      { _define_class_count += 1; }
  uint64_t getElapsedTime() const           { return os::javaTimeMillis() - getStartTime(); }
};

#endif

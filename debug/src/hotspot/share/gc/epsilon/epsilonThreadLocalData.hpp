#ifndef SHARE_VM_GC_EPSILON_EPSILONTHREADLOCALDATA_HPP
#define SHARE_VM_GC_EPSILON_EPSILONTHREADLOCALDATA_HPP

#include "runtime/thread.hpp"
#include "utilities/debug.hpp"

class EpsilonThreadLocalData {
private:
  size_t _ergo_tlab_size;
  int64_t _last_tlab_time;

  EpsilonThreadLocalData() :
          _ergo_tlab_size(0),
          _last_tlab_time(0) { }

  static EpsilonThreadLocalData* data(Thread* thread) {
    return thread->gc_data<EpsilonThreadLocalData>();
  }

public:
  static void create(Thread* thread) {
    new (data(thread)) EpsilonThreadLocalData();
  }

  static void destroy(Thread* thread) {
    data(thread)->~EpsilonThreadLocalData();
  }

  static size_t ergo_tlab_size(Thread *thread) {
    return data(thread)->_ergo_tlab_size;
  }

  static int64_t last_tlab_time(Thread *thread) {
    return data(thread)->_last_tlab_time;
  }

  static void set_ergo_tlab_size(Thread *thread, size_t val) {
    data(thread)->_ergo_tlab_size = val;
  }

  static void set_last_tlab_time(Thread *thread, int64_t time) {
    data(thread)->_last_tlab_time = time;
  }
};

#endif

#ifndef SHARE_VM_SERVICES_MEM_TRACKER_HPP
#define SHARE_VM_SERVICES_MEM_TRACKER_HPP

#include "services/nmtCommon.hpp"
#include "utilities/nativeCallStack.hpp"

#define CURRENT_PC   NativeCallStack::empty_stack()
#define CALLER_PC    NativeCallStack::empty_stack()

class Tracker : public StackObj {
 public:
  enum TrackerType {
     uncommit,
     release
  };
  Tracker(enum TrackerType type) : _type(type) { }
  void record(address addr, size_t size) { }
 private:
  enum TrackerType  _type;
};

class MemTracker : AllStatic {
 public:
  static inline NMT_TrackingLevel tracking_level() { return NMT_off; }
  static inline void shutdown() { }
  static inline void init() { }
  static bool check_launcher_nmt_support(const char* value) { return true; }
  static bool verify_nmt_option() { return true; }

  static inline void* record_malloc(void* mem_base, size_t size, MEMFLAGS flag, const NativeCallStack& stack, NMT_TrackingLevel level) { return mem_base; }
  static inline size_t malloc_header_size(NMT_TrackingLevel level) { return 0; }
  static inline size_t malloc_header_size(void* memblock) { return 0; }
  static inline void* malloc_base(void* memblock) { return memblock; }
  static inline void* record_free(void* memblock) { return memblock; }

  static inline void record_new_arena(MEMFLAGS flag) { }
  static inline void record_arena_free(MEMFLAGS flag) { }
  static inline void record_arena_size_change(int diff, MEMFLAGS flag) { }
  static inline void record_virtual_memory_reserve(void* addr, size_t size, const NativeCallStack& stack, MEMFLAGS flag = mtNone) { }
  static inline void record_virtual_memory_reserve_and_commit(void* addr, size_t size, const NativeCallStack& stack, MEMFLAGS flag = mtNone) { }
  static inline void record_virtual_memory_commit(void* addr, size_t size, const NativeCallStack& stack) { }
  static inline void record_virtual_memory_type(void* addr, MEMFLAGS flag) { }
  static inline void record_thread_stack(void* addr, size_t size) { }
  static inline void release_thread_stack(void* addr, size_t size) { }

  static void final_report(outputStream*) { }
  static void error_report(outputStream*) { }
};

#endif

#ifndef SHARE_VM_RUNTIME_MEMPROFILER_HPP
#define SHARE_VM_RUNTIME_MEMPROFILER_HPP

// Prints periodic memory usage trace of HotSpot VM

class MemProfilerTask;

class MemProfiler : AllStatic {
 friend class MemProfilerTask;
 private:
  static MemProfilerTask* _task;
  static FILE* _log_fp;
  // Do trace (callback from MemProfilerTask and from disengage)
  static void do_trace()      { };
 public:
  // Start/stop the profiler
  static void engage()        { };
  static void disengage()     { };
  // Tester
  static bool is_active()     { return 0; };
};

#endif

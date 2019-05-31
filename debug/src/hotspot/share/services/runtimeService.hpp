#ifndef SHARE_VM_SERVICES_RUNTIMESERVICE_HPP
#define SHARE_VM_SERVICES_RUNTIMESERVICE_HPP

#include "runtime/perfData.hpp"
#include "runtime/timer.hpp"

class RuntimeService : public AllStatic {
private:
  static PerfCounter* _sync_time_ticks;        // Accumulated time spent getting to safepoints
  static PerfCounter* _total_safepoints;
  static PerfCounter* _safepoint_time_ticks;   // Accumulated time at safepoints
  static PerfCounter* _application_time_ticks; // Accumulated time not at safepoints

  static TimeStamp _safepoint_timer;
  static TimeStamp _app_timer;
  static double _last_safepoint_sync_time_sec;

public:
  static void init();

  static jlong safepoint_sync_time_ms();
  static jlong safepoint_count();
  static jlong safepoint_time_ms();
  static jlong application_time_ms();

  static double last_safepoint_time_sec()      { return _safepoint_timer.seconds(); }
  static double last_application_time_sec()    { return _app_timer.seconds(); }

  // callbacks
  static void record_safepoint_begin() { };
  static void record_safepoint_synchronized() { };
  static void record_safepoint_end() { };
  static void record_application_start() { };
};

#endif

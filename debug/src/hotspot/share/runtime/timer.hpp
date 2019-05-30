#ifndef SHARE_VM_RUNTIME_TIMER_HPP
#define SHARE_VM_RUNTIME_TIMER_HPP

#include "utilities/globalDefinitions.hpp"

// Timers for simple measurement.

class elapsedTimer {
  friend class VMStructs;
 private:
  jlong _counter;
  jlong _start_counter;
  bool  _active;
 public:
  elapsedTimer()             { _active = false; reset(); }
  elapsedTimer(jlong time, jlong timeUnitsPerSecond);
  void add(elapsedTimer t);
  void start();
  void stop();
  void reset()               { _counter = 0; }
  double seconds() const;
  jlong milliseconds() const;
  jlong ticks() const        { return _counter; }
  jlong active_ticks() const;
  bool  is_active() const { return _active; }
};

// TimeStamp is used for recording when an event took place.
class TimeStamp {
 private:
  jlong _counter;
 public:
  TimeStamp()  { _counter = 0; }
  void clear() { _counter = 0; }
  // has the timestamp been updated since being created or cleared?
  bool is_updated() const { return _counter != 0; }
  // update to current elapsed time
  void update();
  // update to given elapsed time
  void update_to(jlong ticks);
  // returns seconds since updated
  // (must not be in a cleared state:  must have been previously updated)
  double seconds() const;
  jlong milliseconds() const;
  // ticks elapsed between VM start and last update
  jlong ticks() const { return _counter; }
  // ticks elapsed since last update
  jlong ticks_since_update() const;
};

class TraceCPUTime: public StackObj {
 private:
  bool _active;                 // true if times will be measured and printed
  bool _print_cr;               // if true print carriage return at end
  double _starting_user_time;   // user time at start of measurement
  double _starting_system_time; // system time at start of measurement
  double _starting_real_time;   // real time at start of measurement
  outputStream* _logfile;       // output is printed to this stream
  bool _error;                  // true if an error occurred, turns off output

 public:
  TraceCPUTime(bool doit = true,
               bool print_cr = true,
               outputStream *logfile = NULL);
  ~TraceCPUTime();
};

class TimeHelper {
 public:
  static double counter_to_seconds(jlong counter);
  static double counter_to_millis(jlong counter);
  static jlong millis_to_counter(jlong millis);
};

#endif

#ifndef SHARE_VM_RUNTIME_TIMERTRACE_HPP
#define SHARE_VM_RUNTIME_TIMERTRACE_HPP

#include "logging/log.hpp"
#include "utilities/globalDefinitions.hpp"

// TraceTime is used for tracing the execution time of a block
// Usage:
//  {
//    TraceTime t("some timer", TIMERTRACE_LOG(Info, startuptime, tagX...));
//    some_code();
//  }
//

typedef void (*TraceTimerLogPrintFunc)(const char*, ...);

class TraceTime: public StackObj {
 private:
  bool          _active;    // do timing
  bool          _verbose;   // report every timing
  elapsedTimer  _t;         // timer
  elapsedTimer* _accum;     // accumulator
  const char*   _title;     // name of timer
  TraceTimerLogPrintFunc _print;

 public:
  // Constructors
  TraceTime(const char* title,
            bool doit = true);

  TraceTime(const char* title,
            elapsedTimer* accumulator,
            bool doit = true,
            bool verbose = false);

  TraceTime(const char* title,
            TraceTimerLogPrintFunc ttlpf);

  ~TraceTime();

  // Accessors
  bool verbose() const            { return _verbose; }

  // Activation
  void suspend()  { if (_active) _t.stop(); }
  void resume()   { if (_active) _t.start(); }
};

#endif

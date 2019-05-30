#ifndef SHARE_VM_GC_SHARED_COLLECTORCOUNTERS_HPP
#define SHARE_VM_GC_SHARED_COLLECTORCOUNTERS_HPP

#include "runtime/perfData.hpp"

// CollectorCounters is a holder class for performance counters
// that track a collector

class CollectorCounters: public CHeapObj<mtGC> {
  friend class VMStructs;

  private:
    PerfCounter*      _invocations;
    PerfCounter*      _time;
    PerfVariable*     _last_entry_time;
    PerfVariable*     _last_exit_time;

    // Constant PerfData types don't need to retain a reference.
    // However, it's a good idea to document them here.
    // PerfStringConstant*     _name;

    char*             _name_space;

  public:

    CollectorCounters(const char* name, int ordinal);

    ~CollectorCounters();

    inline PerfCounter* invocation_counter() const  { return _invocations; }

    inline PerfCounter* time_counter() const        { return _time; }

    inline PerfVariable* last_entry_counter() const { return _last_entry_time; }

    inline PerfVariable* last_exit_counter() const  { return _last_exit_time; }

    const char* name_space() const                  { return _name_space; }
};

class TraceCollectorStats: public PerfTraceTimedEvent {

  protected:
    CollectorCounters* _c;

  public:
    TraceCollectorStats(CollectorCounters* c);

    ~TraceCollectorStats();
};

#endif

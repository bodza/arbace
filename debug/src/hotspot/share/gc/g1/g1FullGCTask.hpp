#ifndef SHARE_GC_G1_G1FULLGCTASK_HPP
#define SHARE_GC_G1_G1FULLGCTASK_HPP

#include "gc/shared/workgroup.hpp"
#include "utilities/ticks.hpp"

class G1FullCollector;

class G1FullGCTask : public AbstractGangTask {
  G1FullCollector* _collector;

protected:
  G1FullGCTask(const char* name, G1FullCollector* collector) :
    AbstractGangTask(name),
    _collector(collector) { }

  G1FullCollector* collector() { return _collector; }
  void log_task(const char* name, uint worker_id, const Ticks& start, const Ticks& stop = Ticks::now());
};

#endif

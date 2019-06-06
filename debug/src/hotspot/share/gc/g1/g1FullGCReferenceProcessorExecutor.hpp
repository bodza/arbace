#ifndef SHARE_GC_G1_G1FULLGCREFERENCEPROCESSOREXECUTOR_HPP
#define SHARE_GC_G1_G1FULLGCREFERENCEPROCESSOREXECUTOR_HPP

#include "gc/g1/g1FullGCCompactionPoint.hpp"
#include "gc/g1/g1FullGCScope.hpp"
#include "gc/g1/g1FullGCTask.hpp"
#include "gc/g1/g1RootProcessor.hpp"
#include "gc/g1/g1StringDedup.hpp"
#include "gc/g1/heapRegionManager.hpp"
#include "gc/shared/referenceProcessor.hpp"
#include "utilities/ticks.hpp"

class STWGCTimer;

class G1FullGCReferenceProcessingExecutor: public AbstractRefProcTaskExecutor {
  G1FullCollector*    _collector;
  ReferenceProcessor* _reference_processor;
  uint                _old_mt_degree;

public:
  G1FullGCReferenceProcessingExecutor(G1FullCollector* collector);
  ~G1FullGCReferenceProcessingExecutor();

  // Do reference processing.
  void execute(STWGCTimer* timer);

  // Executes the given task using concurrent marking worker threads.
  virtual void execute(ProcessTask& task, uint ergo_workers);

private:
  void run_task(AbstractGangTask* task);
  void run_task(AbstractGangTask* task, uint workers);

  class G1RefProcTaskProxy : public AbstractGangTask {
    typedef AbstractRefProcTaskExecutor::ProcessTask ProcessTask;
    ProcessTask&           _proc_task;
    G1FullCollector*       _collector;
    ParallelTaskTerminator _terminator;

  public:
    G1RefProcTaskProxy(ProcessTask& proc_task, G1FullCollector* scope);

    virtual void work(uint worker_id);
  };
};

#endif

#ifndef SHARE_VM_RUNTIME_STATSAMPLER_HPP
#define SHARE_VM_RUNTIME_STATSAMPLER_HPP

#include "runtime/perfData.hpp"
#include "runtime/task.hpp"

class StatSamplerTask;

/*
 * The StatSampler class is responsible for periodically updating
 * sampled PerfData instances and writing the sampled values to the
 * PerfData memory region.
 *
 * In addition it is also responsible for providing a home for
 * PerfData instances that otherwise have no better home.
 */
class StatSampler : AllStatic {

  friend class StatSamplerTask;

  private:

    static StatSamplerTask* _task;
    static PerfDataList* _sampled;

    static void collect_sample();
    static void create_misc_perfdata();
    static void create_sampled_perfdata();
    static void sample_data(PerfDataList* list);
    static const char* get_system_property(const char* name, TRAPS);
    static void create_system_property_instrumentation(TRAPS);

  public:
    // Start/stop the sampler
    static void engage();
    static void disengage();

    static bool is_active() { return _task != NULL; }

    static void initialize();
    static void destroy();
};

void statSampler_exit();

#endif

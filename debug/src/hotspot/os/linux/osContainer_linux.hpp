#ifndef OS_LINUX_VM_OSCONTAINER_LINUX_HPP
#define OS_LINUX_VM_OSCONTAINER_LINUX_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "memory/allocation.hpp"

#define OSCONTAINER_ERROR (-2)

class OSContainer: AllStatic {
 private:
  static bool   _is_initialized;
  static bool   _is_containerized;

 public:
  static void init();
  static inline bool is_containerized();
  static const char * container_type();

  static jlong memory_limit_in_bytes();
  static jlong memory_and_swap_limit_in_bytes();
  static jlong memory_soft_limit_in_bytes();
  static jlong memory_usage_in_bytes();
  static jlong memory_max_usage_in_bytes();

  static int active_processor_count();

  static char * cpu_cpuset_cpus();
  static char * cpu_cpuset_memory_nodes();

  static int cpu_quota();
  static int cpu_period();

  static int cpu_shares();
};

inline bool OSContainer::is_containerized() {
  return _is_containerized;
}

#endif

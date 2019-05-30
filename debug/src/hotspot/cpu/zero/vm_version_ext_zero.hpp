#ifndef CPU_ZERO_VM_VM_VERSION_EXT_ZERO_HPP
#define CPU_ZERO_VM_VM_VERSION_EXT_ZERO_HPP

#include "utilities/macros.hpp"
#include "vm_version_zero.hpp"

class VM_Version_Ext : public VM_Version {
 private:
  static const size_t      CPU_TYPE_DESC_BUF_SIZE = 256;
  static const size_t      CPU_DETAILED_DESC_BUF_SIZE = 4096;

  static int               _no_of_threads;
  static int               _no_of_cores;
  static int               _no_of_sockets;
  static bool              _initialized;
  static char              _cpu_name[CPU_TYPE_DESC_BUF_SIZE];
  static char              _cpu_desc[CPU_DETAILED_DESC_BUF_SIZE];

 public:
  static int number_of_threads(void);
  static int number_of_cores(void);
  static int number_of_sockets(void);

  static const char* cpu_name(void);
  static const char* cpu_description(void);
  static void initialize_cpu_information(void);
};

#endif

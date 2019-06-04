#ifndef SHARE_VM_SERVICES_WRITEABLEFLAG_HPP
#define SHARE_VM_SERVICES_WRITEABLEFLAG_HPP

#include "runtime/flags/jvmFlag.hpp"
#include "runtime/globals.hpp"
#include "utilities/formatBuffer.hpp"

class WriteableFlags : AllStatic {
private:
  // a writeable flag setter accepting either 'jvalue' or 'char *' values
  static JVMFlag::Error set_flag(const char* name, const void* value, JVMFlag::Error(*setter)(JVMFlag*, const void*, JVMFlag::Flags, FormatBuffer<80>&), JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // a writeable flag setter accepting 'char *' values
  static JVMFlag::Error set_flag_from_char(JVMFlag* f, const void* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // a writeable flag setter accepting 'jvalue' values
  static JVMFlag::Error set_flag_from_jvalue(JVMFlag* f, const void* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);

  // set a boolean global flag
  static JVMFlag::Error set_bool_flag(const char* name, const char* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a int global flag
  static JVMFlag::Error set_int_flag(const char* name, const char* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a uint global flag
  static JVMFlag::Error set_uint_flag(const char* name, const char* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a intx global flag
  static JVMFlag::Error set_intx_flag(const char* name, const char* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a uintx global flag
  static JVMFlag::Error set_uintx_flag(const char* name, const char* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a uint64_t global flag
  static JVMFlag::Error set_uint64_t_flag(const char* name, const char* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a size_t global flag using value from NULL
  static JVMFlag::Error set_size_t_flag(const char* name, const char* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a boolean global flag
  static JVMFlag::Error set_bool_flag(const char* name, bool value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a int global flag
  static JVMFlag::Error set_int_flag(const char* name, int value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a uint global flag
  static JVMFlag::Error set_uint_flag(const char* name, uint value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a intx global flag
  static JVMFlag::Error set_intx_flag(const char* name, intx value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a uintx global flag
  static JVMFlag::Error set_uintx_flag(const char* name, uintx value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a uint64_t global flag
  static JVMFlag::Error set_uint64_t_flag(const char* name, uint64_t value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a size_t global flag using value from NULL
  static JVMFlag::Error set_size_t_flag(const char* name, size_t value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
  // set a string global flag
  static JVMFlag::Error set_ccstr_flag(const char* name, const char* value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);

public:
  /* sets a writeable flag to the provided value
   *
   * - return status is one of the WriteableFlags::err enum values
   * - an eventual error message will be generated to the provided err_msg buffer
   */
  static JVMFlag::Error set_flag(const char* flag_name, const char* flag_value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);

  /* sets a writeable flag to the provided value
   *
   * - return status is one of the WriteableFlags::err enum values
   * - an eventual error message will be generated to the provided err_msg buffer
   */
  static JVMFlag::Error set_flag(const char* flag_name, jvalue flag_value, JVMFlag::Flags origin, FormatBuffer<80>& err_msg);
};

#endif

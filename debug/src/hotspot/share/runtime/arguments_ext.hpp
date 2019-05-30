#ifndef SHARE_VM_RUNTIME_ARGUMENTS_EXT_HPP
#define SHARE_VM_RUNTIME_ARGUMENTS_EXT_HPP

#include "memory/allocation.hpp"
#include "runtime/arguments.hpp"

class ArgumentsExt: AllStatic {
public:
  // The argument processing extension. Returns true if there is
  // no additional parsing needed in Arguments::parse() for the option.
  // Otherwise returns false.
  static inline bool process_options(const JavaVMOption *option) { return false; }
  static inline void report_unsupported_options() { }
};

#endif

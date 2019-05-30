#ifndef SHARE_VM_RUNTIME_OS_EXT_HPP
#define SHARE_VM_RUNTIME_OS_EXT_HPP

#define EMIT_RANGES_FOR_GLOBALS_EXT // NOP
#define EMIT_CONSTRAINTS_FOR_GLOBALS_EXT // NOP
#define EMIT_WRITEABLES_FOR_GLOBALS_EXT // NOP

public:
  static void init_globals_ext() {} // Run from init_globals().
                                    // See os.hpp/cpp and init.cpp.

 private:

#endif

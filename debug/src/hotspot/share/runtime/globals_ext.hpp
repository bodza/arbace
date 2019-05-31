#ifndef SHARE_VM_RUNTIME_GLOBALS_EXT_HPP
#define SHARE_VM_RUNTIME_GLOBALS_EXT_HPP

#include "runtime/flags/jvmFlag.hpp"

// globals_extension.hpp extension

// Additional JVMFlags enum values
#define JVMFLAGS_EXT

// Additional JVMFlagsWithType enum values
#define JVMFLAGSWITHTYPE_EXT

// globals.cpp extension

// Additional flag definitions
#define MATERIALIZE_FLAGS_EXT

// Additional flag descriptors: see flagTable definition
#define FLAGTABLE_EXT

// Default method implementations

inline bool JVMFlag::is_unlocker_ext() const {
  return false;
}

inline bool JVMFlag::is_unlocked_ext() const {
  return true;
}

inline bool JVMFlag::is_writeable_ext() const {
  return false;
}

inline bool JVMFlag::is_external_ext() const {
  return false;
}

inline JVMFlag::MsgType JVMFlag::get_locked_message_ext(char* buf, int buflen) const {
  buf[0] = '\0';
  return JVMFlag::NONE;
}

#endif

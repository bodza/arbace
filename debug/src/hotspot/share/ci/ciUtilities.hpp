#ifndef SHARE_VM_CI_CIUTILITIES_HPP
#define SHARE_VM_CI_CIUTILITIES_HPP

#include "ci/ciEnv.hpp"
#include "utilities/globalDefinitions.hpp"

// The following routines and definitions are used internally in the
// compiler interface.

#define CURRENT_ENV \
  ciEnv::current()

// where current thread is THREAD
#define CURRENT_THREAD_ENV \
  ciEnv::current(thread)

#define IS_IN_VM \
  ciEnv::is_in_vm()

#define ASSERT_IN_VM \

inline const char* bool_to_str(bool b) {
  return ((b) ? "true" : "false");
}

const char* basictype_to_str(BasicType t);
const char  basictype_to_char(BasicType t);

jbyte *ci_card_table_address();
template <typename T> T ci_card_table_address_as() {
  return reinterpret_cast<T>(ci_card_table_address());
}

#endif

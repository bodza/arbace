#ifndef SHARE_VM_RUNTIME_EXTENDEDPC_HPP
#define SHARE_VM_RUNTIME_EXTENDEDPC_HPP

#include "utilities/globalDefinitions.hpp"

// An ExtendedPC contains the _pc from a signal handler in a platform
// independent way.

class ExtendedPC {
 private:
  address _pc;

 public:
  address pc()     const { return _pc; }
  ExtendedPC(address pc) { _pc = pc; }
  ExtendedPC()           { _pc = NULL; }
};

#endif

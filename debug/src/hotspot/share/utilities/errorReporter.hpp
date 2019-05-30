#ifndef SHARE_VM_UTILITIES_ERRORREPORTER_HPP
#define SHARE_VM_UTILITIES_ERRORREPORTER_HPP

#include "utilities/globalDefinitions.hpp"
#include "memory/allocation.hpp"

class ErrorReporter : public StackObj {

public:
  ErrorReporter();
  ~ErrorReporter(){};

  void call(FILE* fd, char *buffer, int length);
};

#endif

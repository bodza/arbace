#ifndef SHARE_VM_SERVICES_ATTACHLISTENER_HPP
#define SHARE_VM_SERVICES_ATTACHLISTENER_HPP

#include "memory/allocation.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "utilities/ostream.hpp"

// The AttachListener thread services a queue of operations that are enqueued
// by client tools. Each operation is identified by a name and has up to 3
// arguments. The operation name is mapped to a function which performs the
// operation. The function is called with an outputStream which is can use to
// write any result data (for examples the properties command serializes
// properties names and values to the output stream). When the function
// complets the result value and any result data is returned to the client
// tool.

class AttachOperation;

typedef jint (*AttachOperationFunction)(AttachOperation* op, outputStream* out);

struct AttachOperationFunctionInfo {
  const char* name;
  AttachOperationFunction func;
};

class AttachListener: AllStatic {
 public:
  static void vm_start() {};
  static void init()  {};
  static void abort() {};

  // invoke to perform clean-up tasks when all clients detach
  static void detachall() {};

  // indicates if the Attach Listener needs to be created at startup
  static bool init_at_startup() { return false; };

  // indicates if we have a trigger to start the Attach Listener
  static bool is_init_trigger() { return false; };

  static bool is_attach_supported()             { return false; }

 private:
  static bool has_init_error(TRAPS);
};

#endif

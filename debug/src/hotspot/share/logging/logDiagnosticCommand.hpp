#ifndef SHARE_VM_LOGGING_LOGDIAGNOSTICCOMMAND_HPP
#define SHARE_VM_LOGGING_LOGDIAGNOSTICCOMMAND_HPP

#include "services/diagnosticCommand.hpp"

// The LogDiagnosticCommand represents the 'VM.log' DCMD
// that allows configuration of the logging at runtime.
// It can be used to view or modify the current log configuration.
// VM.log without additional arguments prints the usage description.
// The 'list' argument will list all available log tags,
// levels, decorators and currently configured log outputs.
// Specifying 'disable' will disable logging completely.
// The remaining arguments are used to set a log output to log everything
// with the specified tags and levels using the given decorators.
class LogDiagnosticCommand : public DCmdWithParser {
 protected:
  DCmdArgument<char *> _output;
  DCmdArgument<char *> _output_options;
  DCmdArgument<char *> _what;
  DCmdArgument<char *> _decorators;
  DCmdArgument<bool> _disable;
  DCmdArgument<bool> _list;
  DCmdArgument<bool> _rotate;

 public:
  LogDiagnosticCommand(outputStream* output, bool heap_allocated);
  void execute(DCmdSource source, TRAPS);
  static void registerCommand();
  static int num_arguments();

  static const char* name() {
    return "VM.log";
  }

  static const char* description() {
    return "Lists current log configuration, enables/disables/configures a log output, or rotates all logs.";
  }

  // Used by SecurityManager. This DCMD requires ManagementPermission = control.
  static const JavaPermission permission() {
    JavaPermission p = {"java.lang.management.ManagementPermission", "control", NULL};
    return p;
  }
};

#endif

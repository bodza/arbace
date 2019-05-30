#ifndef SHARE_MEMORY_METASPACE_METASPACEDCMD_HPP
#define SHARE_MEMORY_METASPACE_METASPACEDCMD_HPP

#include "services/diagnosticCommand.hpp"

class outputStream;

namespace metaspace {

class MetaspaceDCmd : public DCmdWithParser {
  DCmdArgument<bool> _basic;
  DCmdArgument<bool> _show_loaders;
  DCmdArgument<bool> _by_spacetype;
  DCmdArgument<bool> _by_chunktype;
  DCmdArgument<bool> _show_vslist;
  DCmdArgument<bool> _show_vsmap;
  DCmdArgument<char*> _scale;
  DCmdArgument<bool> _show_classes;
public:
  MetaspaceDCmd(outputStream* output, bool heap);
  static const char* name() {
    return "VM.metaspace";
  }
  static const char* description() {
    return "Prints the statistics for the metaspace";
  }
  static const char* impact() {
      return "Medium: Depends on number of classes loaded.";
  }
  static const JavaPermission permission() {
    JavaPermission p = {"java.lang.management.ManagementPermission",
                        "monitor", NULL};
    return p;
  }
  static int num_arguments();
  virtual void execute(DCmdSource source, TRAPS);
};
}

#endif

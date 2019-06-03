#ifndef HOTSPOT_SHARE_CLASSFILE_CLASSLOADERHIERARCHYDCMD_HPP_
#define HOTSPOT_SHARE_CLASSFILE_CLASSLOADERHIERARCHYDCMD_HPP_

#include "services/diagnosticCommand.hpp"

class ClassLoaderHierarchyDCmd: public DCmdWithParser {
  DCmdArgument<bool> _show_classes;
  DCmdArgument<bool> _verbose;
  DCmdArgument<bool> _fold;
public:
  ClassLoaderHierarchyDCmd(outputStream* output, bool heap);

  static const char* name() {
    return "VM.classloaders";
  }

  static const char* description() {
    return "Prints classloader hierarchy.";
  }
  static const char* impact() {
      return "Medium: Depends on number of class loaders and classes loaded.";
  }
  static const JavaPermission permission() {
    JavaPermission p = {"java.lang.management.ManagementPermission", "monitor", NULL};
    return p;
  }
  static int num_arguments();
  virtual void execute(DCmdSource source, TRAPS);
};

#endif

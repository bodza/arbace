#ifndef SHARE_VM_CLASSFILE_CLASSLOADEREXT_HPP
#define SHARE_VM_CLASSFILE_CLASSLOADEREXT_HPP

#include "classfile/classLoader.hpp"
#include "classfile/moduleEntry.hpp"
#include "utilities/macros.hpp"

class ClassListParser;

class ClassLoaderExt: public ClassLoader { // AllStatic
public:
  enum SomeConstants {
    max_classpath_index = 0x7fff
  };

  static void setup_search_paths() { };
  static void setup_module_paths(TRAPS) { };
};

#endif

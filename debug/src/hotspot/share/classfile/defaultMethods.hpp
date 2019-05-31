#ifndef SHARE_VM_CLASSFILE_DEFAULTMETHODS_HPP
#define SHARE_VM_CLASSFILE_DEFAULTMETHODS_HPP

#include "runtime/handles.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/exceptions.hpp"

class InstanceKlass;
class Symbol;
class Method;

class DefaultMethods : AllStatic {
 public:

  // Analyzes class and determines which default methods are inherited
  // from interfaces (and has no other implementation).  For each method
  // (and each different signature the method could have), create an
  // "overpass" method that is an instance method that redirects to the
  // default method.  Overpass methods are added to the methods lists for
  // the class.
  static void generate_default_methods(InstanceKlass* klass, const GrowableArray<Method*>* mirandas, TRAPS);
};
#endif

#ifndef SHARE_VM_PRIMS_PRIVILEGEDSTACK_HPP
#define SHARE_VM_PRIMS_PRIVILEGEDSTACK_HPP

#include "oops/oopsHierarchy.hpp"
#include "runtime/vframe.hpp"
#include "utilities/growableArray.hpp"

class PrivilegedElement {
 private:
  Klass*    _klass;                // klass for method
  oop       _privileged_context;   // context for operation
  intptr_t*     _frame_id;             // location on stack
  PrivilegedElement* _next;        // Link to next one on stack
 public:
  void initialize(vframeStream* vf, oop context, PrivilegedElement* next, TRAPS);
  void oops_do(OopClosure* f);
  intptr_t* frame_id() const           { return _frame_id; }
  oop  privileged_context() const  { return _privileged_context; }
  oop  class_loader() const        { return InstanceKlass::cast(_klass)->class_loader(); }
  oop  protection_domain() const   { return InstanceKlass::cast(_klass)->protection_domain(); }
  PrivilegedElement *next() const  { return _next; }

  // debugging (used for find)
  void print_on(outputStream* st) const   {};
  bool contains(address addr)             { return 0; };
};

#endif

#ifndef SHARE_VM_PRIMS_METHODCOMPARATOR_HPP
#define SHARE_VM_PRIMS_METHODCOMPARATOR_HPP

#include "interpreter/bytecodeStream.hpp"
#include "oops/constantPool.hpp"
#include "oops/method.hpp"

// methodComparator provides an interface for determining if methods of
// different versions of classes are equivalent or switchable

class MethodComparator {
 private:
  static BytecodeStream *_s_old, *_s_new;
  static ConstantPool* _old_cp;
  static ConstantPool* _new_cp;

  static bool args_same(Bytecodes::Code c_old, Bytecodes::Code c_new);
  static bool pool_constants_same(int cpi_old, int cpi_new);
  static int check_stack_and_locals_size(Method* old_method, Method* new_method);

 public:
  // Check if the new method is equivalent to the old one modulo constant pool (EMCP).
  // Intuitive definition: two versions of the same method are EMCP, if they don't differ
  // on the source code level. Practically, we check whether the only difference between
  // method versions is some constantpool indices embedded into the bytecodes, and whether
  // these indices eventually point to the same constants for both method versions.
  static bool methods_EMCP(Method* old_method, Method* new_method);
};

#endif

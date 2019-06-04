#ifndef SHARE_VM_CLASSFILE_VERIFICATIONTYPE_HPP
#define SHARE_VM_CLASSFILE_VERIFICATIONTYPE_HPP

#include "classfile/systemDictionary.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/oop.hpp"
#include "oops/symbol.hpp"
#include "runtime/handles.hpp"
#include "runtime/signature.hpp"

enum {
  // As specifed in the JVM spec
  ITEM_Top = 0,
  ITEM_Integer = 1,
  ITEM_Float = 2,
  ITEM_Double = 3,
  ITEM_Long = 4,
  ITEM_Null = 5,
  ITEM_UninitializedThis = 6,
  ITEM_Object = 7,
  ITEM_Uninitialized = 8,
  ITEM_Bogus = (uint)-1
};

#endif

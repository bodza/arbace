#include "precompiled.hpp"
#include "ci/ciClassList.hpp"
#include "ci/ciMemberName.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "classfile/javaClasses.hpp"

// ------------------------------------------------------------------
// ciMemberName::get_vmtarget
//
// Return: MN.vmtarget
ciMethod* ciMemberName::get_vmtarget() const {
  VM_ENTRY_MARK;
  // FIXME: Share code with ciMethodHandle::get_vmtarget
  Metadata* vmtarget = java_lang_invoke_MemberName::vmtarget(get_oop());
  if (vmtarget->is_method())
    return CURRENT_ENV->get_method((Method*) vmtarget);
  // FIXME: What if the vmtarget is a Klass?
  ShouldNotReachHere();
  return NULL;
}

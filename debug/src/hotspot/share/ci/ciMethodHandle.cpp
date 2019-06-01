#include "precompiled.hpp"

#include "ci/ciClassList.hpp"
#include "ci/ciMethodHandle.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "classfile/javaClasses.hpp"

// ------------------------------------------------------------------
// ciMethodHandle::get_vmtarget
//
// Return: MH.form -> LF.vmentry -> MN.vmtarget
ciMethod* ciMethodHandle::get_vmtarget() const {
  VM_ENTRY_MARK;
  oop form_oop     = java_lang_invoke_MethodHandle::form(get_oop());
  oop vmentry_oop  = java_lang_invoke_LambdaForm::vmentry(form_oop);
  // FIXME: Share code with ciMemberName::get_vmtarget
  Metadata* vmtarget = java_lang_invoke_MemberName::vmtarget(vmentry_oop);
  if (vmtarget->is_method())
    return CURRENT_ENV->get_method((Method*) vmtarget);
  // FIXME: What if the vmtarget is a Klass?
  ShouldNotReachHere();
  return NULL;
}

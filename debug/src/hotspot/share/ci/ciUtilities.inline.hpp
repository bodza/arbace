#ifndef SHARE_VM_CI_CIUTILITIES_INLINE_HPP
#define SHARE_VM_CI_CIUTILITIES_INLINE_HPP

#include "ci/ciUtilities.hpp"
#include "runtime/interfaceSupport.inline.hpp"

// Add a ci native entry wrapper?

// Bring the compilation thread into the VM state.
#define VM_ENTRY_MARK \
  CompilerThread* thread=CompilerThread::current(); \
  ThreadInVMfromNative __tiv(thread); \
  ResetNoHandleMark rnhm; \
  HandleMarkCleaner __hm(thread); \
  Thread* THREAD = thread;

// Bring the compilation thread into the VM state.  No handle mark.
#define VM_QUICK_ENTRY_MARK \
  CompilerThread* thread=CompilerThread::current(); \
  ThreadInVMfromNative __tiv(thread); \
/* \
 * [TODO] The NoHandleMark line does nothing but declare a function prototype \
 * The NoHandkeMark constructor is NOT executed. If the ()'s are \
 * removed, causes the NoHandleMark assert to trigger. \
 */ \
  Thread* THREAD = thread;

#define EXCEPTION_CONTEXT \
  CompilerThread* thread=CompilerThread::current(); \
  Thread* THREAD = thread;

#define GUARDED_VM_ENTRY(action) \
  { if (IS_IN_VM) { action } else { VM_ENTRY_MARK; { action }}}

#define GUARDED_VM_QUICK_ENTRY(action) \
  { if (IS_IN_VM) { action } else { VM_QUICK_ENTRY_MARK; { action }}}

// Redefine this later.
#define KILL_COMPILE_ON_FATAL_(result) \
  THREAD); \
  if (HAS_PENDING_EXCEPTION) { \
    if (PENDING_EXCEPTION->klass() == \
        SystemDictionary::ThreadDeath_klass()) { \
      /* Kill the compilation. */ \
      fatal("unhandled ci exception"); \
      return (result); \
    } \
    CLEAR_PENDING_EXCEPTION; \
    return (result); \
  } \
  (void)(0

#define KILL_COMPILE_ON_ANY \
  THREAD); \
  if (HAS_PENDING_EXCEPTION) { \
    fatal("unhandled ci exception"); \
    CLEAR_PENDING_EXCEPTION; \
  } \
(void)(0

#endif

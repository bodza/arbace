// This file defines a set of macros which are used by the c++-interpreter
// for updating a method's methodData object.

#ifndef SHARE_VM_INTERPRETER_BYTECODEINTERPRETERPROFILING_HPP
#define SHARE_VM_INTERPRETER_BYTECODEINTERPRETERPROFILING_HPP

#ifdef CC_INTERP

// Empty dummy implementations if profiling code is switched off. //////////////

#define SET_MDX(mdx)

#define BI_PROFILE_GET_OR_CREATE_METHOD_DATA(exception_handler) \
  if (ProfileInterpreter) { \
    ShouldNotReachHere(); \
  }

#define BI_PROFILE_ALIGN_TO_CURRENT_BCI()

#define BI_PROFILE_UPDATE_JUMP()
#define BI_PROFILE_UPDATE_BRANCH(is_taken)
#define BI_PROFILE_UPDATE_RET(bci)
#define BI_PROFILE_SUBTYPECHECK_FAILED(receiver)
#define BI_PROFILE_UPDATE_CHECKCAST(null_seen, receiver)
#define BI_PROFILE_UPDATE_INSTANCEOF(null_seen, receiver)
#define BI_PROFILE_UPDATE_CALL()
#define BI_PROFILE_UPDATE_FINALCALL()
#define BI_PROFILE_UPDATE_VIRTUALCALL(receiver)
#define BI_PROFILE_UPDATE_SWITCH(switch_index)

#endif

#endif

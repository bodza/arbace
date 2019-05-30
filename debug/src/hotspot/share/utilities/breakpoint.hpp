#ifndef SHARE_VM_UTILITIES_BREAKPOINT_HPP
#define SHARE_VM_UTILITIES_BREAKPOINT_HPP

// Provide BREAKPOINT macro for requesting stop in the debugger.

// We presently only have one non-default definition, so it's not
// worth going through the COMPILER_HEADER() dispatch, with all
// non-visCPP files being empty.
#ifdef TARGET_COMPILER_visCPP
#define BREAKPOINT __asm { int 3 }
#endif

// If no more specific definition provided, default to calling a
// function that is defined per-platform.  See also os::breakpoint().
#ifndef BREAKPOINT
extern "C" void breakpoint();
#define BREAKPOINT ::breakpoint()
#endif

#endif

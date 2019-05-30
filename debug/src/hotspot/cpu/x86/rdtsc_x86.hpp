#ifndef CPU_X86_VM_RDTSC_X86_HPP
#define CPU_X86_VM_RDTSC_X86_HPP

#include "memory/allocation.hpp"
#include "utilities/macros.hpp"

// Interface to the x86 rdtsc() time counter, if available.
// Not guaranteed to be synchronized across hardware threads and
// therefore software threads, and can be updated asynchronously
// by software. elapsed_counter() can jump backwards
// as well as jump forward when threads query different cores/sockets.
// Very much not recommended for general use.
// INVTSC is a minimal requirement for auto-enablement.

class Rdtsc : AllStatic {
 public:
  static jlong elapsed_counter(); // provides quick time stamps
  static jlong frequency();       // tsc register
  static bool  is_supported();    // InvariantTSC
  static jlong raw();             // direct rdtsc() access
  static bool  is_elapsed_counter_enabled(); // turn off with -XX:-UseFastUnorderedTimeStamps
  static jlong epoch();
  static bool  initialize();
};

#endif

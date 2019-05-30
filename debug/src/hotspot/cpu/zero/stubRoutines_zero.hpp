#ifndef CPU_ZERO_VM_STUBROUTINES_ZERO_HPP
#define CPU_ZERO_VM_STUBROUTINES_ZERO_HPP

  // This file holds the platform specific parts of the StubRoutines
  // definition. See stubRoutines.hpp for a description on how to
  // extend it.

 public:
  static address call_stub_return_pc() {
    return (address) -1;
  }

  static bool returns_to_call_stub(address return_pc) {
    return return_pc == call_stub_return_pc();
  }

  enum platform_dependent_constants {
    code_size1 = 0,      // The assembler will fail with a guarantee
    code_size2 = 0       // if these are too small.  Simply increase
  };                     // them if that happens.

  enum method_handles_platform_dependent_constants {
    method_handles_adapters_code_size = 0
  };

#endif

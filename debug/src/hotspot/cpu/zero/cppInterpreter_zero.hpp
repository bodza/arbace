#ifndef CPU_ZERO_VM_CPPINTERPRETER_ZERO_HPP
#define CPU_ZERO_VM_CPPINTERPRETER_ZERO_HPP

 protected:
  // Size of interpreter code
  const static int InterpreterCodeSize = 6 * K;

 public:
  // Method entries
  static int normal_entry(Method* method, intptr_t UNUSED, TRAPS);
  static int native_entry(Method* method, intptr_t UNUSED, TRAPS);
  static int accessor_entry(Method* method, intptr_t UNUSED, TRAPS);
  static int empty_entry(Method* method, intptr_t UNUSED, TRAPS);

 public:
  // Main loop of normal_entry
  static void main_loop(int recurse, TRAPS);

 private:
  // Helpers for method_handle_entry
  static void insert_vmslots(int insert_before, int num_slots, TRAPS);
  static void remove_vmslots(int first_slot, int num_slots, TRAPS);
  static BasicType result_type_of_handle(oop method_handle);
  static intptr_t* calculate_unwind_sp(ZeroStack* stack, oop method_handle);
  static void throw_exception(JavaThread* thread, Symbol* name,char *msg=NULL);

#endif

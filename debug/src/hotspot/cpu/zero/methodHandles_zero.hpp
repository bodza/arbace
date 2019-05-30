// Adapters
enum /* platform_dependent_constants */ {
  adapter_code_size = sizeof(ZeroEntry) * (Interpreter::method_handle_invoke_LAST - Interpreter::method_handle_invoke_FIRST + 1)
};

private:
  static oop popFromStack(TRAPS);
  static void invoke_target(Method* method, TRAPS);
  static void throw_AME(Klass* rcvr, Method* interface_method, TRAPS);
  static int method_handle_entry_invokeBasic(Method* method, intptr_t UNUSED, TRAPS);
  static int method_handle_entry_linkToStaticOrSpecial(Method* method, intptr_t UNUSED, TRAPS);
  static int method_handle_entry_linkToVirtual(Method* method, intptr_t UNUSED, TRAPS);
  static int method_handle_entry_linkToInterface(Method* method, intptr_t UNUSED, TRAPS);
  static int method_handle_entry_invalid(Method* method, intptr_t UNUSED, TRAPS);

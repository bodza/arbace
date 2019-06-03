#ifndef SHARE_VM_COMPILER_COMPILERORACLE_HPP
#define SHARE_VM_COMPILER_COMPILERORACLE_HPP

#include "memory/allocation.hpp"
#include "oops/oopsHierarchy.hpp"

// CompilerOracle is an interface for turning on and off compilation
// for some methods

class CompilerOracle : AllStatic {
 private:
  static bool _quiet;
  static void print_tip();
  static void print_parse_error(const char*&  error_msg, char* original_line);

 public:
  // True if the command file has been specified or is implicit
  static bool has_command_file();

  // Reads from file and adds to lists
  static void parse_from_file();

  // Tells whether we to exclude compilation of method
  static bool should_exclude(const methodHandle& method);
  static bool should_exclude_quietly() { return _quiet; }

  // Tells whether we want to inline this method
  static bool should_inline(const methodHandle& method);

  // Tells whether we want to disallow inlining of this method
  static bool should_not_inline(const methodHandle& method);

  // Tells whether we should print the assembly for this method
  static bool should_print(const methodHandle& method);

  // Tells whether to break when compiling method
  static bool should_break_at(const methodHandle& method);

  // Check to see if this method has option set for it
  static bool has_option_string(const methodHandle& method, const char * option);

  // Check if method has option and value set. If yes, overwrite value and return true,
  // otherwise leave value unchanged and return false.
  template<typename T>
  static bool has_option_value(const methodHandle& method, const char* option, T& value);

  // Fast check if there is any option available that compile control needs to know about
  static bool has_any_option();

  // Reads from string instead of file
  static void parse_from_string(const char* command_string, void (*parser)(char*));

  static void parse_from_line(char* line);
  static void parse_compile_only(char * line);

  // For updating the oracle file
  static void append_comment_to_file(const char* message);
  static void append_exclude_to_file(const methodHandle& method);

  // Tells whether there are any methods to print for print_method_statistics()
  static bool should_print_methods();
};

#endif

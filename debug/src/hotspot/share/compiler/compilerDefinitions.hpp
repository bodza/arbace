#ifndef SHARE_VM_COMPILER_COMPILERDEFINITIONS_HPP
#define SHARE_VM_COMPILER_COMPILERDEFINITIONS_HPP

#include "memory/allocation.hpp"

// The (closed set) of concrete compiler classes.
enum CompilerType {
  compiler_none,
  compiler_c1,
  compiler_c2,
  compiler_jvmci,
  compiler_number_of_types
};

extern const char* compilertype2name_tab[compiler_number_of_types];     // Map CompilerType to its name
inline const char* compilertype2name(CompilerType t) { return (uint)t < compiler_number_of_types ? compilertype2name_tab[t] : NULL; }

// Handy constants for deciding which compiler mode to use.
enum MethodCompilation {
  InvocationEntryBci   = -1,     // i.e. not an on-stack replacement compilation
  BeforeBci            = InvocationEntryBci,
  AfterBci             = -2,
  UnwindBci            = -3,
  AfterExceptionBci    = -4,
  UnknownBci           = -5,
  InvalidFrameStateBci = -6
};

// Enumeration to distinguish tiers of compilation
enum CompLevel {
  CompLevel_any               = -2,
  CompLevel_all               = -2,
  CompLevel_none              = 0,
  CompLevel_simple            = 1, // C1
  CompLevel_full_optimization = 4  // C2 or JVMCI
};

extern CompLevel CompLevel_highest_tier;
extern CompLevel CompLevel_initial_compile;

enum CompMode {
  CompMode_none = 0,
  CompMode_client = 1,
  CompMode_server = 2
};

extern CompMode Compilation_mode;

inline bool is_server_compilation_mode_vm()         { return Compilation_mode == CompMode_server; }
inline bool is_client_compilation_mode_vm()         { return Compilation_mode == CompMode_client; }
inline bool is_c1_compile(int comp_level)           { return comp_level > CompLevel_none && comp_level < CompLevel_full_optimization; }
inline bool is_c2_compile(int comp_level)           { return comp_level == CompLevel_full_optimization; }
inline bool is_highest_tier_compile(int comp_level) { return comp_level == CompLevel_highest_tier; }
inline bool is_compile(int comp_level)              { return is_c1_compile(comp_level) || is_c2_compile(comp_level); }

class CompilerConfig : public AllStatic {
public:
  // Scale compile thresholds
  // Returns threshold scaled with CompileThresholdScaling
  static intx scaled_compile_threshold(intx threshold, double scale);
  static intx scaled_compile_threshold(intx threshold);

  // Returns freq_log scaled with CompileThresholdScaling
  static intx scaled_freq_log(intx freq_log, double scale);
  static intx scaled_freq_log(intx freq_log);

  static bool check_args_consistency(bool status);

  static void ergo_initialize();

private:
  static void set_tiered_flags();
};

#endif

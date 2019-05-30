#ifndef SHARE_VM_INTERPRETER_BYTECODEHISTOGRAM_HPP
#define SHARE_VM_INTERPRETER_BYTECODEHISTOGRAM_HPP

#include "interpreter/bytecodes.hpp"
#include "memory/allocation.hpp"

// BytecodeCounter counts the number of bytecodes executed

class BytecodeCounter: AllStatic {
 private:
  friend class TemplateInterpreterGenerator;
  friend class         BytecodeInterpreter;

 public:
  // Initialization
  static void reset()                      {};

  // Counter info (all info since last reset)
  static int    counter_value()            { return 0; };
  static double elapsed_time()             { return 0; }; // in seconds
  static double frequency()                { return 0; }; // bytecodes/seconds

  // Counter printing
  static void   print()                    {};
};

// BytecodeHistogram collects number of executions of bytecodes

class BytecodeHistogram: AllStatic {
 private:
  friend class TemplateInterpreterGenerator;
  friend class         BytecodeInterpreter;

 public:
  // Initialization
  static void reset()                       {}; // reset counters

  // Profile printing
  static void print(float cutoff = 0.01F)   {}; // cutoff in percent
};

// BytecodePairHistogram collects number of executions of bytecode pairs.
// A bytecode pair is any sequence of two consequtive bytecodes.

class BytecodePairHistogram: AllStatic {
 public: // for SparcWorks
  enum Constants {
    log2_number_of_codes = 8,                         // use a power of 2 for faster addressing
    number_of_codes      = 1 << log2_number_of_codes, // must be no less than Bytecodes::number_of_codes
    number_of_pairs      = number_of_codes * number_of_codes
  };

 private:
  friend class TemplateInterpreterGenerator;

 public:
  // Initialization
  static void reset()                       {};   // reset counters

  // Profile printing
  static void print(float cutoff = 0.01F)   {};   // cutoff in percent
};

#endif

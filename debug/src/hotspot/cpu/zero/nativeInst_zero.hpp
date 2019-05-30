#ifndef CPU_ZERO_VM_NATIVEINST_ZERO_HPP
#define CPU_ZERO_VM_NATIVEINST_ZERO_HPP

#include "asm/assembler.hpp"
#include "runtime/icache.hpp"
#include "runtime/os.hpp"

// We have interfaces for the following instructions:
// - NativeInstruction
// - - NativeCall
// - - NativeMovConstReg
// - - NativeMovConstRegPatching
// - - NativeJump
// - - NativeIllegalOpCode
// - - NativeReturn
// - - NativeReturnX (return with argument)
// - - NativePushConst
// - - NativeTstRegMem

// The base class for different kinds of native instruction abstractions.
// Provides the primitive operations to manipulate code relative to this.

class NativeInstruction {
 public:
  bool is_jump() {
    ShouldNotCallThis();
    return false;
  }

  bool is_safepoint_poll() {
    ShouldNotCallThis();
    return false;
  }
};

inline NativeInstruction* nativeInstruction_at(address address) {
  ShouldNotCallThis();
  return NULL;
}

class NativeCall : public NativeInstruction {
 public:
  enum zero_specific_constants {
    instruction_size = 0 // not used within the interpreter
  };

  address instruction_address() const {
    ShouldNotCallThis();
    return NULL;
  }

  address next_instruction_address() const {
    ShouldNotCallThis();
    return NULL;
  }

  address return_address() const {
    ShouldNotCallThis();
    return NULL;
  }

  address destination() const {
    ShouldNotCallThis();
    return NULL;
  }

  void set_destination_mt_safe(address dest) {
    ShouldNotCallThis();
  }

  void verify_alignment() {
    ShouldNotCallThis();
  }

  void verify() {
    ShouldNotCallThis();
  }

  static bool is_call_before(address return_address) {
    ShouldNotCallThis();
    return false;
  }
};

inline NativeCall* nativeCall_before(address return_address) {
  ShouldNotCallThis();
  return NULL;
}

inline NativeCall* nativeCall_at(address address) {
  ShouldNotCallThis();
  return NULL;
}

class NativeMovConstReg : public NativeInstruction {
 public:
  address next_instruction_address() const {
    ShouldNotCallThis();
    return NULL;
  }

  intptr_t data() const {
    ShouldNotCallThis();
    return 0;
  }

  void set_data(intptr_t x) {
    ShouldNotCallThis();
  }
};

inline NativeMovConstReg* nativeMovConstReg_at(address address) {
  ShouldNotCallThis();
  return NULL;
}

class NativeMovRegMem : public NativeInstruction {
 public:
  int offset() const {
    ShouldNotCallThis();
    return 0;
  }

  void set_offset(intptr_t x) {
    ShouldNotCallThis();
  }

  void add_offset_in_bytes(int add_offset) {
    ShouldNotCallThis();
  }
};

inline NativeMovRegMem* nativeMovRegMem_at(address address) {
  ShouldNotCallThis();
  return NULL;
}

class NativeJump : public NativeInstruction {
 public:
  enum zero_specific_constants {
    instruction_size = 0 // not used within the interpreter
  };

  address jump_destination() const {
    ShouldNotCallThis();
    return NULL;
  }

  void set_jump_destination(address dest) {
    ShouldNotCallThis();
  }

  static void check_verified_entry_alignment(address entry, address verified_entry) {
  }

  static void patch_verified_entry(address entry, address verified_entry, address dest);
};

inline NativeJump* nativeJump_at(address address) {
  ShouldNotCallThis();
  return NULL;
}

class NativeGeneralJump : public NativeInstruction {
 public:
  address jump_destination() const {
    ShouldNotCallThis();
    return NULL;
  }

  static void insert_unconditional(address code_pos, address entry) {
    ShouldNotCallThis();
  }

  static void replace_mt_safe(address instr_addr, address code_buffer) {
    ShouldNotCallThis();
  }
};

inline NativeGeneralJump* nativeGeneralJump_at(address address) {
  ShouldNotCallThis();
  return NULL;
}

#endif

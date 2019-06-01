#ifndef SHARE_VM_CODE_VMREG_HPP
#define SHARE_VM_CODE_VMREG_HPP

#include "asm/register.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "utilities/ostream.hpp"

//------------------------------VMReg------------------------------------------
// The VM uses 'unwarped' stack slots; the compiler uses 'warped' stack slots.
// Register numbers below VMRegImpl::stack0 are the same for both.  Register
// numbers above stack0 are either warped (in the compiler) or unwarped
// (in the VM).  Unwarped numbers represent stack indices, offsets from
// the current stack pointer.  Warped numbers are required during compilation
// when we do not yet know how big the frame will be.

class VMRegImpl;
typedef VMRegImpl* VMReg;

class VMRegImpl {
// friend class OopMap;
friend class VMStructs;
friend class OptoReg;
// friend class Location;
private:
  enum {
    BAD_REG = -1
  };

  static VMReg stack0;
  // Names for registers
  static const char *regName[];
  static const int register_count;

public:

  static VMReg  as_VMReg(int val, bool bad_ok = false) { return (VMReg) (intptr_t) val; }

  const char*  name() {
    if (is_reg()) {
      return regName[value()];
    } else if (!is_valid()) {
      return "BAD";
    } else {
      // shouldn't really be called with stack
      return "STACKED REG";
    }
  }
  static VMReg Bad() { return (VMReg) (intptr_t) BAD_REG; }
  bool is_valid() const { return ((intptr_t) this) != BAD_REG; }
  bool is_stack() const { return (intptr_t) this >= (intptr_t) stack0; }
  bool is_reg()   const { return is_valid() && !is_stack(); }

  // A concrete register is a value that returns true for is_reg() and is
  // also a register you could use in the assembler. On machines with
  // 64bit registers only one half of the VMReg (and OptoReg) is considered
  // concrete.
  //  bool is_concrete();

  // VMRegs are 4 bytes wide on all platforms
  static const int stack_slot_size;
  static const int slots_per_word;

  // This really ought to check that the register is "real" in the sense that
  // we don't try and get the VMReg number of a physical register that doesn't
  // have an expressible part. That would be pd specific code
  VMReg next() {
    return (VMReg)(intptr_t)(value() + 1);
  }
  VMReg next(int i) {
    return (VMReg)(intptr_t)(value() + i);
  }
  VMReg prev() {
    return (VMReg)(intptr_t)(value() - 1);
  }

  intptr_t value() const         { return (intptr_t) this; }

  void print_on(outputStream* st) const;
  void print() const { print_on(tty); }

  // bias a stack slot.
  // Typically used to adjust a virtual frame slots by amounts that are offset by
  // amounts that are part of the native abi. The VMReg must be a stack slot
  // and the result must be also.

  VMReg bias(int offset) {
    // VMReg res = VMRegImpl::as_VMReg(value() + offset);
    VMReg res = stack2reg(reg2stack() + offset);
    return res;
  }

  // Convert register numbers to stack slots and vice versa
  static VMReg stack2reg( int idx ) {
    return (VMReg) (intptr_t) (stack0->value() + idx);
  }

  uintptr_t reg2stack() {
    return value() - stack0->value();
  }

  static void set_regName();

#include CPU_HEADER(vmreg)
};

//---------------------------VMRegPair-------------------------------------------
// Pairs of 32-bit registers for arguments.
// SharedRuntime::java_calling_convention will overwrite the structs with
// the calling convention's registers.  VMRegImpl::Bad is returned for any
// unused 32-bit register.  This happens for the unused high half of Int
// arguments, or for 32-bit pointers or for longs in the 32-bit sparc build
// (which are passed to natives in low 32-bits of e.g. O0/O1 and the high
// 32-bits of O0/O1 are set to VMRegImpl::Bad).  Longs in one register & doubles
// always return a high and a low register, as do 64-bit pointers.
//
class VMRegPair {
private:
  VMReg _second;
  VMReg _first;
public:
  void set_bad (                   ) { _second=VMRegImpl::Bad(); _first=VMRegImpl::Bad(); }
  void set1    (         VMReg v  ) { _second=VMRegImpl::Bad(); _first=v; }
  void set2    (         VMReg v  ) { _second=v->next();  _first=v; }
  void set_pair( VMReg second, VMReg first    ) { _second= second;    _first= first; }
  void set_ptr ( VMReg ptr ) {
    _second = ptr->next();
    _first = ptr;
  }
  // Return true if single register, even if the pair is really just adjacent stack slots
  bool is_single_reg() const {
    return (_first->is_valid()) && (_first->value() + 1 == _second->value());
  }

  // Return true if single stack based "register" where the slot alignment matches input alignment
  bool is_adjacent_on_stack(int alignment) const {
    return (_first->is_stack() && (_first->value() + 1 == _second->value()) && ((_first->value() & (alignment-1)) == 0));
  }

  // Return true if single stack based "register" where the slot alignment matches input alignment
  bool is_adjacent_aligned_on_stack(int alignment) const {
    return (_first->is_stack() && (_first->value() + 1 == _second->value()) && ((_first->value() & (alignment-1)) == 0));
  }

  // Return true if single register but adjacent stack slots do not count
  bool is_single_phys_reg() const {
    return (_first->is_reg() && (_first->value() + 1 == _second->value()));
  }

  VMReg second() const { return _second; }
  VMReg first()  const { return _first; }
  VMRegPair(VMReg s, VMReg f) {  _second = s; _first = f; }
  VMRegPair(VMReg f) { _second = VMRegImpl::Bad(); _first = f; }
  VMRegPair() { _second = VMRegImpl::Bad(); _first = VMRegImpl::Bad(); }
};

#endif

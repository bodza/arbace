#ifndef CPU_ZERO_VM_REGISTER_ZERO_HPP
#define CPU_ZERO_VM_REGISTER_ZERO_HPP

#include "asm/register.hpp"
#include "vm_version_zero.hpp"

class VMRegImpl;
typedef VMRegImpl* VMReg;

// Use Register as shortcut
class RegisterImpl;
typedef RegisterImpl* Register;

inline Register as_Register(int encoding) {
  return (Register)(intptr_t) encoding;
}

// The implementation of integer registers for the zero architecture
class RegisterImpl : public AbstractRegisterImpl {
 public:
  enum {
    number_of_registers = 0
  };

  // construction
  inline friend Register as_Register(int encoding);
  VMReg as_VMReg();

  // derived registers, offsets, and addresses
  Register successor() const {
    return as_Register(encoding() + 1);
  }

  // accessors
  int encoding() const {
    return (intptr_t)this;
  }
  bool is_valid() const {
    return 0 <= (intptr_t) this && (intptr_t)this < number_of_registers;
  }
  const char* name() const;
};

// Use FloatRegister as shortcut
class FloatRegisterImpl;
typedef FloatRegisterImpl* FloatRegister;

inline FloatRegister as_FloatRegister(int encoding) {
  return (FloatRegister)(intptr_t) encoding;
}

// The implementation of floating point registers for the zero architecture
class FloatRegisterImpl : public AbstractRegisterImpl {
 public:
  enum {
    number_of_registers = 0
  };

  // construction
  inline friend FloatRegister as_FloatRegister(int encoding);
  VMReg as_VMReg();

  // derived registers, offsets, and addresses
  FloatRegister successor() const {
    return as_FloatRegister(encoding() + 1);
  }

  // accessors
  int encoding() const {
    return (intptr_t)this;
  }
  bool is_valid() const {
    return 0 <= (intptr_t) this && (intptr_t)this < number_of_registers;
  }
  const char* name() const;
};

class ConcreteRegisterImpl : public AbstractRegisterImpl {
 public:
  enum {
    number_of_registers = RegisterImpl::number_of_registers + FloatRegisterImpl::number_of_registers
  };

  static const int max_gpr;
  static const int max_fpr;
};

CONSTANT_REGISTER_DECLARATION(Register, noreg, (-1));
#ifndef DONT_USE_REGISTER_DEFINES
#define noreg ((Register)(noreg_RegisterEnumValue))
#endif

#endif

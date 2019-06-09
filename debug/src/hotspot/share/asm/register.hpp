#ifndef SHARE_VM_ASM_REGISTER_HPP
#define SHARE_VM_ASM_REGISTER_HPP

#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

// Use AbstractRegister as shortcut
class AbstractRegisterImpl;
typedef AbstractRegisterImpl* AbstractRegister;

// The super class for platform specific registers. Instead of using value objects,
// registers are implemented as pointers. Subclassing is used so all registers can
// use the debugging suport below. No virtual functions are used for efficiency.
// They are canonicalized; i.e., registers are equal if their pointers are equal,
// and vice versa. A concrete implementation may just map the register onto 'this'.

class AbstractRegisterImpl {
 protected:
  int value()                              const { return (int)(intx)this; }
};

//
// Macros for use in defining Register instances.  We'd like to be
// able to simply define const instances of the RegisterImpl* for each
// of the registers needed on a system in a header file.  However many
// compilers don't handle this very well and end up producing a
// private definition in every file which includes the header file.
// Along with the static constructors necessary for initialization it
// can consume a significant amount of space in the result library.
//
// The following macros allow us to declare the instance in a .hpp and
// produce an enumeration value which has the same number.  Then in a
// .cpp the the register instance can be defined using the enumeration
// value.  This avoids the use of static constructors and multiple
// definitions per .cpp.  In addition #defines for the register can be
// produced so that the constant registers can be inlined.  These
// macros should not be used inside other macros, because you may get
// multiple evaluations of the macros which can give bad results.
//
// Here are some example uses and expansions.  Note that the macro
// invocation is terminated with a ;.
//
// CONSTANT_REGISTER_DECLARATION(Register, G0, 0);
//
// extern const Register G0;
// enum { G0_RegisterEnumValue = 0 };
//
// REGISTER_DECLARATION(Register, Gmethod, G5);
//
// extern const Register Gmethod;
// enum { Gmethod_RegisterEnumValue = G5_RegisterEnumValue };
//
// REGISTER_DEFINITION(Register, G0);
//
// const Register G0 = ( ( Register ) G0_RegisterEnumValue );
//

#define AS_REGISTER(type,name)         ((type)name##_##type##EnumValue)

#define CONSTANT_REGISTER_DECLARATION(type, name, value) \
extern const type name; \
enum { name##_##type##EnumValue = (value) }

#define REGISTER_DECLARATION(type, name, value) \
extern const type name; \
enum { name##_##type##EnumValue = value##_##type##EnumValue }

#define REGISTER_DEFINITION(type, name) \
const type name = ((type)name##_##type##EnumValue)

#include CPU_HEADER(register)

#endif

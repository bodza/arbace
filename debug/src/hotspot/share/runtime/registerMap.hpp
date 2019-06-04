#ifndef SHARE_VM_RUNTIME_REGISTERMAP_HPP
#define SHARE_VM_RUNTIME_REGISTERMAP_HPP

#include "code/vmreg.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

class JavaThread;

//
// RegisterMap
//
// A companion structure used for stack traversal. The RegisterMap contains
// misc. information needed in order to do correct stack traversal of stack
// frames.  Hence, it must always be passed in as an argument to
// frame::sender(RegisterMap*).
//
// In particular,
//   1) It provides access to the thread for which the stack belongs.  The
//      thread object is needed in order to get sender of a deoptimized frame.
//
//   2) It is used to pass information from a callee frame to its caller
//      frame about how the frame should be traversed.  This is used to let
//      the caller frame take care of calling oops-do of out-going
//      arguments, when the callee frame is not instantiated yet.  This
//      happens, e.g., when a compiled frame calls into
//      resolve_virtual_call.  (Hence, it is critical that the same
//      RegisterMap object is used for the entire stack walk.  Normally,
//      this is hidden by using the StackFrameStream.)  This is used when
//      doing follow_oops and oops_do.
//
//   3) The RegisterMap keeps track of the values of callee-saved registers
//      from frame to frame (hence, the name).  For some stack traversal the
//      values of the callee-saved registers does not matter, e.g., if you
//      only need the static properties such as frame type, pc, and such.
//      Updating of the RegisterMap can be turned off by instantiating the
//      register map as: RegisterMap map(thread, false);

class RegisterMap : public StackObj {
 public:
    typedef julong LocationValidType;
  enum {
    reg_count = ConcreteRegisterImpl::number_of_registers,
    location_valid_type_size = sizeof(LocationValidType) * 8,
    location_valid_size = (reg_count + location_valid_type_size - 1) / location_valid_type_size
  };
 private:
  intptr_t*    _location[reg_count];    // Location of registers (intptr_t* looks better than address in the debugger)
  LocationValidType _location_valid[location_valid_size];
  bool        _include_argument_oops;   // Should include argument_oop marked locations for compiler
  JavaThread* _thread;                  // Reference to current thread
  bool        _update_map;              // Tells if the register map need to be
                                        // updated when traversing the stack

  void check_location_valid() { }

 public:
  RegisterMap(JavaThread *thread, bool update_map = true);
  RegisterMap(const RegisterMap* map);

  address location(VMReg reg) const {
    int index = reg->value() / location_valid_type_size;
    if (_location_valid[index] & ((LocationValidType)1 << (reg->value() % location_valid_type_size))) {
      return (address) _location[reg->value()];
    } else {
      return pd_location(reg);
    }
  }

  void set_location(VMReg reg, address loc) {
    int index = reg->value() / location_valid_type_size;
    _location[reg->value()] = (intptr_t*) loc;
    _location_valid[index] |= ((LocationValidType)1 << (reg->value() % location_valid_type_size));
    check_location_valid();
  }

  // Called by an entry frame.
  void clear();

  bool include_argument_oops() const      { return _include_argument_oops; }
  void set_include_argument_oops(bool f)  { _include_argument_oops = f; }

  JavaThread *thread() const { return _thread; }
  bool update_map()    const { return _update_map; }

  void print_on(outputStream* st) const;
  void print() const;

  // the following contains the definition of pd_xxx methods
#include CPU_HEADER(registerMap)
};

#endif

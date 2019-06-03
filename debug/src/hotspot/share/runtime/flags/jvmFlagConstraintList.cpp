#include "precompiled.hpp"

#include "classfile/stringTable.hpp"
#include "classfile/symbolTable.hpp"
#include "gc/shared/jvmFlagConstraintsGC.hpp"
#include "runtime/arguments.hpp"
#include "runtime/flags/jvmFlag.hpp"
#include "runtime/flags/jvmFlagConstraintList.hpp"
#include "runtime/flags/jvmFlagConstraintsCompiler.hpp"
#include "runtime/flags/jvmFlagConstraintsRuntime.hpp"
#include "runtime/os.hpp"
#include "utilities/macros.hpp"
#include "c1/c1_globals.hpp"

class JVMFlagConstraint_bool : public JVMFlagConstraint {
  JVMFlagConstraintFunc_bool _constraint;
  const bool* _ptr;

public:
  // the "name" argument must be a string literal
  JVMFlagConstraint_bool(const char* name, const bool* ptr,
                                 JVMFlagConstraintFunc_bool func,
                                 ConstraintType type) : JVMFlagConstraint(name, type), _constraint(func), _ptr(ptr) { }

  JVMFlag::Error apply(bool verbose) {
    bool value = *_ptr;
    return _constraint(value, verbose);
  }

  JVMFlag::Error apply_bool(bool value, bool verbose) {
    return _constraint(value, verbose);
  }
};

class JVMFlagConstraint_int : public JVMFlagConstraint {
  JVMFlagConstraintFunc_int _constraint;
  const int* _ptr;

public:
  // the "name" argument must be a string literal
  JVMFlagConstraint_int(const char* name, const int* ptr,
                                JVMFlagConstraintFunc_int func,
                                ConstraintType type) : JVMFlagConstraint(name, type), _constraint(func), _ptr(ptr) { }

  JVMFlag::Error apply(bool verbose) {
    int value = *_ptr;
    return _constraint(value, verbose);
  }

  JVMFlag::Error apply_int(int value, bool verbose) {
    return _constraint(value, verbose);
  }
};

class JVMFlagConstraint_intx : public JVMFlagConstraint {
  JVMFlagConstraintFunc_intx _constraint;
  const intx* _ptr;

public:
  // the "name" argument must be a string literal
  JVMFlagConstraint_intx(const char* name, const intx* ptr,
                                 JVMFlagConstraintFunc_intx func,
                                 ConstraintType type) : JVMFlagConstraint(name, type), _constraint(func), _ptr(ptr) { }

  JVMFlag::Error apply(bool verbose) {
    intx value = *_ptr;
    return _constraint(value, verbose);
  }

  JVMFlag::Error apply_intx(intx value, bool verbose) {
    return _constraint(value, verbose);
  }
};

class JVMFlagConstraint_uint : public JVMFlagConstraint {
  JVMFlagConstraintFunc_uint _constraint;
  const uint* _ptr;

public:
  // the "name" argument must be a string literal
  JVMFlagConstraint_uint(const char* name, const uint* ptr,
                                 JVMFlagConstraintFunc_uint func,
                                 ConstraintType type) : JVMFlagConstraint(name, type), _constraint(func), _ptr(ptr) { }

  JVMFlag::Error apply(bool verbose) {
    uint value = *_ptr;
    return _constraint(value, verbose);
  }

  JVMFlag::Error apply_uint(uint value, bool verbose) {
    return _constraint(value, verbose);
  }
};

class JVMFlagConstraint_uintx : public JVMFlagConstraint {
  JVMFlagConstraintFunc_uintx _constraint;
  const uintx* _ptr;

public:
  // the "name" argument must be a string literal
  JVMFlagConstraint_uintx(const char* name, const uintx* ptr,
                                  JVMFlagConstraintFunc_uintx func,
                                 ConstraintType type) : JVMFlagConstraint(name, type), _constraint(func), _ptr(ptr) { }

  JVMFlag::Error apply(bool verbose) {
    uintx value = *_ptr;
    return _constraint(value, verbose);
  }

  JVMFlag::Error apply_uintx(uintx value, bool verbose) {
    return _constraint(value, verbose);
  }
};

class JVMFlagConstraint_uint64_t : public JVMFlagConstraint {
  JVMFlagConstraintFunc_uint64_t _constraint;
  const uint64_t* _ptr;

public:
  // the "name" argument must be a string literal
  JVMFlagConstraint_uint64_t(const char* name, const uint64_t* ptr,
                                     JVMFlagConstraintFunc_uint64_t func,
                                 ConstraintType type) : JVMFlagConstraint(name, type), _constraint(func), _ptr(ptr) { }

  JVMFlag::Error apply(bool verbose) {
    uint64_t value = *_ptr;
    return _constraint(value, verbose);
  }

  JVMFlag::Error apply_uint64_t(uint64_t value, bool verbose) {
    return _constraint(value, verbose);
  }
};

class JVMFlagConstraint_size_t : public JVMFlagConstraint {
  JVMFlagConstraintFunc_size_t _constraint;
  const size_t* _ptr;
public:
  // the "name" argument must be a string literal
  JVMFlagConstraint_size_t(const char* name, const size_t* ptr,
                                   JVMFlagConstraintFunc_size_t func,
                                 ConstraintType type) : JVMFlagConstraint(name, type), _constraint(func), _ptr(ptr) { }

  JVMFlag::Error apply(bool verbose) {
    size_t value = *_ptr;
    return _constraint(value, verbose);
  }

  JVMFlag::Error apply_size_t(size_t value, bool verbose) {
    return _constraint(value, verbose);
  }
};

class JVMFlagConstraint_double : public JVMFlagConstraint {
  JVMFlagConstraintFunc_double _constraint;
  const double* _ptr;

public:
  // the "name" argument must be a string literal
  JVMFlagConstraint_double(const char* name, const double* ptr,
                                   JVMFlagConstraintFunc_double func,
                                 ConstraintType type) : JVMFlagConstraint(name, type), _constraint(func), _ptr(ptr) { }

  JVMFlag::Error apply(bool verbose) {
    double value = *_ptr;
    return _constraint(value, verbose);
  }

  JVMFlag::Error apply_double(double value, bool verbose) {
    return _constraint(value, verbose);
  }
};

// No constraint emitting
void emit_constraint_no(...) { /* NOP */ }

// No constraint emitting if function argument is NOT provided
void emit_constraint_bool(const char* /*name*/, const bool* /*value*/) { /* NOP */ }
void emit_constraint_ccstr(const char* /*name*/, const ccstr* /*value*/) { /* NOP */ }
void emit_constraint_ccstrlist(const char* /*name*/, const ccstrlist* /*value*/) { /* NOP */ }
void emit_constraint_int(const char* /*name*/, const int* /*value*/) { /* NOP */ }
void emit_constraint_intx(const char* /*name*/, const intx* /*value*/) { /* NOP */ }
void emit_constraint_uint(const char* /*name*/, const uint* /*value*/) { /* NOP */ }
void emit_constraint_uintx(const char* /*name*/, const uintx* /*value*/) { /* NOP */ }
void emit_constraint_uint64_t(const char* /*name*/, const uint64_t* /*value*/) { /* NOP */ }
void emit_constraint_size_t(const char* /*name*/, const size_t* /*value*/) { /* NOP */ }
void emit_constraint_double(const char* /*name*/, const double* /*value*/) { /* NOP */ }

// JVMFlagConstraint emitting code functions if function argument is provided
void emit_constraint_bool(const char* name, const bool* ptr, JVMFlagConstraintFunc_bool func, JVMFlagConstraint::ConstraintType type) {
  JVMFlagConstraintList::add(new JVMFlagConstraint_bool(name, ptr, func, type));
}
void emit_constraint_int(const char* name, const int* ptr, JVMFlagConstraintFunc_int func, JVMFlagConstraint::ConstraintType type) {
  JVMFlagConstraintList::add(new JVMFlagConstraint_int(name, ptr, func, type));
}
void emit_constraint_intx(const char* name, const intx* ptr, JVMFlagConstraintFunc_intx func, JVMFlagConstraint::ConstraintType type) {
  JVMFlagConstraintList::add(new JVMFlagConstraint_intx(name, ptr, func, type));
}
void emit_constraint_uint(const char* name, const uint* ptr, JVMFlagConstraintFunc_uint func, JVMFlagConstraint::ConstraintType type) {
  JVMFlagConstraintList::add(new JVMFlagConstraint_uint(name, ptr, func, type));
}
void emit_constraint_uintx(const char* name, const uintx* ptr, JVMFlagConstraintFunc_uintx func, JVMFlagConstraint::ConstraintType type) {
  JVMFlagConstraintList::add(new JVMFlagConstraint_uintx(name, ptr, func, type));
}
void emit_constraint_uint64_t(const char* name, const uint64_t* ptr, JVMFlagConstraintFunc_uint64_t func, JVMFlagConstraint::ConstraintType type) {
  JVMFlagConstraintList::add(new JVMFlagConstraint_uint64_t(name, ptr, func, type));
}
void emit_constraint_size_t(const char* name, const size_t* ptr, JVMFlagConstraintFunc_size_t func, JVMFlagConstraint::ConstraintType type) {
  JVMFlagConstraintList::add(new JVMFlagConstraint_size_t(name, ptr, func, type));
}
void emit_constraint_double(const char* name, const double* ptr, JVMFlagConstraintFunc_double func, JVMFlagConstraint::ConstraintType type) {
  JVMFlagConstraintList::add(new JVMFlagConstraint_double(name, ptr, func, type));
}

// Generate code to call emit_constraint_xxx function
#define EMIT_CONSTRAINT_PRODUCT_FLAG(type, name, value, doc)      ); emit_constraint_##type(#name,&name
#define EMIT_CONSTRAINT_DIAGNOSTIC_FLAG(type, name, value, doc)   ); emit_constraint_##type(#name,&name
#define EMIT_CONSTRAINT_EXPERIMENTAL_FLAG(type, name, value, doc) ); emit_constraint_##type(#name,&name
#define EMIT_CONSTRAINT_MANAGEABLE_FLAG(type, name, value, doc)   ); emit_constraint_##type(#name,&name
#define EMIT_CONSTRAINT_PRODUCT_RW_FLAG(type, name, value, doc)   ); emit_constraint_##type(#name,&name
#define EMIT_CONSTRAINT_PD_PRODUCT_FLAG(type, name, doc)          ); emit_constraint_##type(#name,&name
#define EMIT_CONSTRAINT_PD_DIAGNOSTIC_FLAG(type, name, doc)       ); emit_constraint_##type(#name,&name
#define EMIT_CONSTRAINT_DEVELOPER_FLAG(type, name, value, doc)    ); emit_constraint_no(#name,&name
#define EMIT_CONSTRAINT_PD_DEVELOPER_FLAG(type, name, doc)        ); emit_constraint_no(#name,&name
#define EMIT_CONSTRAINT_NOTPRODUCT_FLAG(type, name, value, doc)   ); emit_constraint_no(#name,&name
#define EMIT_CONSTRAINT_LP64_PRODUCT_FLAG(type, name, value, doc) ); emit_constraint_##type(#name,&name

// Generate func argument to pass into emit_constraint_xxx functions
#define EMIT_CONSTRAINT_CHECK(func, type)                         , func, JVMFlagConstraint::type

// the "name" argument must be a string literal
#define INITIAL_CONSTRAINTS_SIZE 72
GrowableArray<JVMFlagConstraint*>* JVMFlagConstraintList::_constraints = NULL;
JVMFlagConstraint::ConstraintType JVMFlagConstraintList::_validating_type = JVMFlagConstraint::AtParse;

// Check the ranges of all flags that have them or print them out and exit if requested
void JVMFlagConstraintList::init(void) {
  _constraints = new (ResourceObj::C_HEAP, mtArguments) GrowableArray<JVMFlagConstraint*>(INITIAL_CONSTRAINTS_SIZE, true);

  emit_constraint_no(NULL VM_FLAGS(EMIT_CONSTRAINT_DEVELOPER_FLAG,
                                   EMIT_CONSTRAINT_PD_DEVELOPER_FLAG,
                                   EMIT_CONSTRAINT_PRODUCT_FLAG,
                                   EMIT_CONSTRAINT_PD_PRODUCT_FLAG,
                                   EMIT_CONSTRAINT_DIAGNOSTIC_FLAG,
                                   EMIT_CONSTRAINT_PD_DIAGNOSTIC_FLAG,
                                   EMIT_CONSTRAINT_EXPERIMENTAL_FLAG,
                                   EMIT_CONSTRAINT_NOTPRODUCT_FLAG,
                                   EMIT_CONSTRAINT_MANAGEABLE_FLAG,
                                   EMIT_CONSTRAINT_PRODUCT_RW_FLAG,
                                   EMIT_CONSTRAINT_LP64_PRODUCT_FLAG,
                                   IGNORE_RANGE,
                                   EMIT_CONSTRAINT_CHECK,
                                   IGNORE_WRITEABLE));

  EMIT_CONSTRAINTS_FOR_GLOBALS_EXT

  emit_constraint_no(NULL ARCH_FLAGS(EMIT_CONSTRAINT_DEVELOPER_FLAG,
                                     EMIT_CONSTRAINT_PRODUCT_FLAG,
                                     EMIT_CONSTRAINT_DIAGNOSTIC_FLAG,
                                     EMIT_CONSTRAINT_EXPERIMENTAL_FLAG,
                                     EMIT_CONSTRAINT_NOTPRODUCT_FLAG,
                                     IGNORE_RANGE,
                                     EMIT_CONSTRAINT_CHECK,
                                     IGNORE_WRITEABLE));

  emit_constraint_no(NULL C1_FLAGS(EMIT_CONSTRAINT_DEVELOPER_FLAG,
                                   EMIT_CONSTRAINT_PD_DEVELOPER_FLAG,
                                   EMIT_CONSTRAINT_PRODUCT_FLAG,
                                   EMIT_CONSTRAINT_PD_PRODUCT_FLAG,
                                   EMIT_CONSTRAINT_DIAGNOSTIC_FLAG,
                                   EMIT_CONSTRAINT_PD_DIAGNOSTIC_FLAG,
                                   EMIT_CONSTRAINT_NOTPRODUCT_FLAG,
                                   IGNORE_RANGE,
                                   EMIT_CONSTRAINT_CHECK,
                                   IGNORE_WRITEABLE));
}

JVMFlagConstraint* JVMFlagConstraintList::find(const char* name) {
  JVMFlagConstraint* found = NULL;
  for (int i = 0; i<length(); i++) {
    JVMFlagConstraint* constraint = at(i);
    if (strcmp(constraint->name(), name) == 0) {
      found = constraint;
      break;
    }
  }
  return found;
}

// Find constraints by name and return only if found constraint's type is equal or lower than current validating type.
JVMFlagConstraint* JVMFlagConstraintList::find_if_needs_check(const char* name) {
  JVMFlagConstraint* found = NULL;
  JVMFlagConstraint* constraint = find(name);
  if (constraint && (constraint->type() <= _validating_type)) {
    found = constraint;
  }
  return found;
}

// Check constraints for specific constraint type.
bool JVMFlagConstraintList::check_constraints(JVMFlagConstraint::ConstraintType type) {
  guarantee(type > _validating_type, "Constraint check is out of order.");
  _validating_type = type;

  bool status = true;
  for (int i = 0; i<length(); i++) {
    JVMFlagConstraint* constraint = at(i);
    if (type != constraint->type()) continue;
    if (constraint->apply(true) != JVMFlag::SUCCESS) status = false;
  }
  return status;
}

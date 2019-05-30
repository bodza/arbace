#include "precompiled.hpp"
#include "runtime/flags/jvmFlagWriteableList.hpp"
#include "runtime/os.hpp"
#include "c1/c1_globals.hpp"
#include "jvmci/jvmci_globals.hpp"

bool JVMFlagWriteable::is_writeable(void) {
  return _writeable;
}

void JVMFlagWriteable::mark_once(void) {
  if (_type == Once) {
    _writeable = false;
  }
}

void JVMFlagWriteable::mark_startup(void) {
  if (_type == JVMFlagWriteable::CommandLineOnly) {
    _writeable = false;
  }
}

// No control emitting
void emit_writeable_no(...)                         { /* NOP */ }

// No control emitting if type argument is NOT provided
void emit_writeable_bool(const char* /*name*/)      { /* NOP */ }
void emit_writeable_ccstr(const char* /*name*/)     { /* NOP */ }
void emit_writeable_ccstrlist(const char* /*name*/) { /* NOP */ }
void emit_writeable_int(const char* /*name*/)       { /* NOP */ }
void emit_writeable_intx(const char* /*name*/)      { /* NOP */ }
void emit_writeable_uint(const char* /*name*/)      { /* NOP */ }
void emit_writeable_uintx(const char* /*name*/)     { /* NOP */ }
void emit_writeable_uint64_t(const char* /*name*/)  { /* NOP */ }
void emit_writeable_size_t(const char* /*name*/)    { /* NOP */ }
void emit_writeable_double(const char* /*name*/)    { /* NOP */ }

// JVMFlagWriteable emitting code functions if range arguments are provided
void emit_writeable_bool(const char* name, JVMFlagWriteable::WriteableType type) {
  JVMFlagWriteableList::add(new JVMFlagWriteable(name, type));
}
void emit_writeable_int(const char* name, JVMFlagWriteable::WriteableType type) {
  JVMFlagWriteableList::add(new JVMFlagWriteable(name, type));
}
void emit_writeable_intx(const char* name, JVMFlagWriteable::WriteableType type) {
  JVMFlagWriteableList::add(new JVMFlagWriteable(name, type));
}
void emit_writeable_uint(const char* name, JVMFlagWriteable::WriteableType type) {
  JVMFlagWriteableList::add(new JVMFlagWriteable(name, type));
}
void emit_writeable_uintx(const char* name, JVMFlagWriteable::WriteableType type) {
  JVMFlagWriteableList::add(new JVMFlagWriteable(name, type));
}
void emit_writeable_uint64_t(const char* name, JVMFlagWriteable::WriteableType type) {
  JVMFlagWriteableList::add(new JVMFlagWriteable(name, type));
}
void emit_writeable_size_t(const char* name, JVMFlagWriteable::WriteableType type) {
  JVMFlagWriteableList::add(new JVMFlagWriteable(name, type));
}
void emit_writeable_double(const char* name, JVMFlagWriteable::WriteableType type) {
  JVMFlagWriteableList::add(new JVMFlagWriteable(name, type));
}

// Generate code to call emit_writeable_xxx function
#define EMIT_WRITEABLE_PRODUCT_FLAG(type, name, value, doc)      ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_DIAGNOSTIC_FLAG(type, name, value, doc)   ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_EXPERIMENTAL_FLAG(type, name, value, doc) ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_MANAGEABLE_FLAG(type, name, value, doc)   ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_PRODUCT_RW_FLAG(type, name, value, doc)   ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_PD_PRODUCT_FLAG(type, name, doc)          ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_DEVELOPER_FLAG(type, name, value, doc)    ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_PD_DEVELOPER_FLAG(type, name, doc)        ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_PD_DIAGNOSTIC_FLAG(type, name, doc)       ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_NOTPRODUCT_FLAG(type, name, value, doc)   ); emit_writeable_##type(#name
#define EMIT_WRITEABLE_LP64_PRODUCT_FLAG(type, name, value, doc) ); emit_writeable_##type(#name

// Generate type argument to pass into emit_writeable_xxx functions
#define EMIT_WRITEABLE(a)                                      , JVMFlagWriteable::a

#define INITIAL_WRITEABLES_SIZE 2
GrowableArray<JVMFlagWriteable*>* JVMFlagWriteableList::_controls = NULL;

void JVMFlagWriteableList::init(void) {

  _controls = new (ResourceObj::C_HEAP, mtArguments) GrowableArray<JVMFlagWriteable*>(INITIAL_WRITEABLES_SIZE, true);

  emit_writeable_no(NULL VM_FLAGS(EMIT_WRITEABLE_DEVELOPER_FLAG,
                                  EMIT_WRITEABLE_PD_DEVELOPER_FLAG,
                                  EMIT_WRITEABLE_PRODUCT_FLAG,
                                  EMIT_WRITEABLE_PD_PRODUCT_FLAG,
                                  EMIT_WRITEABLE_DIAGNOSTIC_FLAG,
                                  EMIT_WRITEABLE_PD_DIAGNOSTIC_FLAG,
                                  EMIT_WRITEABLE_EXPERIMENTAL_FLAG,
                                  EMIT_WRITEABLE_NOTPRODUCT_FLAG,
                                  EMIT_WRITEABLE_MANAGEABLE_FLAG,
                                  EMIT_WRITEABLE_PRODUCT_RW_FLAG,
                                  EMIT_WRITEABLE_LP64_PRODUCT_FLAG,
                                  IGNORE_RANGE,
                                  IGNORE_CONSTRAINT,
                                  EMIT_WRITEABLE));

  EMIT_WRITEABLES_FOR_GLOBALS_EXT

  emit_writeable_no(NULL ARCH_FLAGS(EMIT_WRITEABLE_DEVELOPER_FLAG,
                                EMIT_WRITEABLE_PRODUCT_FLAG,
                                EMIT_WRITEABLE_DIAGNOSTIC_FLAG,
                                EMIT_WRITEABLE_EXPERIMENTAL_FLAG,
                                EMIT_WRITEABLE_NOTPRODUCT_FLAG,
                                IGNORE_RANGE,
                                IGNORE_CONSTRAINT,
                                EMIT_WRITEABLE));

  emit_writeable_no(NULL JVMCI_FLAGS(EMIT_WRITEABLE_DEVELOPER_FLAG,
                                 EMIT_WRITEABLE_PD_DEVELOPER_FLAG,
                                 EMIT_WRITEABLE_PRODUCT_FLAG,
                                 EMIT_WRITEABLE_PD_PRODUCT_FLAG,
                                 EMIT_WRITEABLE_DIAGNOSTIC_FLAG,
                                 EMIT_WRITEABLE_PD_DIAGNOSTIC_FLAG,
                                 EMIT_WRITEABLE_EXPERIMENTAL_FLAG,
                                 EMIT_WRITEABLE_NOTPRODUCT_FLAG,
                                 IGNORE_RANGE,
                                 IGNORE_CONSTRAINT,
                                 EMIT_WRITEABLE));

  emit_writeable_no(NULL C1_FLAGS(EMIT_WRITEABLE_DEVELOPER_FLAG,
                              EMIT_WRITEABLE_PD_DEVELOPER_FLAG,
                              EMIT_WRITEABLE_PRODUCT_FLAG,
                              EMIT_WRITEABLE_PD_PRODUCT_FLAG,
                              EMIT_WRITEABLE_DIAGNOSTIC_FLAG,
                              EMIT_WRITEABLE_PD_DIAGNOSTIC_FLAG,
                              EMIT_WRITEABLE_NOTPRODUCT_FLAG,
                              IGNORE_RANGE,
                              IGNORE_CONSTRAINT,
                              EMIT_WRITEABLE));
}

JVMFlagWriteable* JVMFlagWriteableList::find(const char* name) {
  JVMFlagWriteable* found = NULL;
  for (int i=0; i<length(); i++) {
    JVMFlagWriteable* writeable = at(i);
    if (strcmp(writeable->name(), name) == 0) {
      found = writeable;
      break;
    }
  }
  return found;
}

void JVMFlagWriteableList::mark_startup(void) {
  for (int i=0; i<length(); i++) {
    JVMFlagWriteable* writeable = at(i);
    writeable->mark_startup();
  }
}

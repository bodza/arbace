#ifndef SHARE_VM_RUNTIME_JVMFLAGWRITEABLE_HPP
#define SHARE_VM_RUNTIME_JVMFLAGWRITEABLE_HPP

#include "utilities/growableArray.hpp"

class JVMFlagWriteable : public CHeapObj<mtArguments> {
public:
  enum WriteableType {
    // can be set without any limits
    Always           = 0,
    // can only be set once, either via command lines or during runtime
    Once             = 1,
    // can only be set on command line (multiple times allowed)
    CommandLineOnly  = 2
  };
private:
  const char* _name;
  WriteableType _type;
  bool _writeable;
  bool _startup_done;
public:
  // the "name" argument must be a string literal
  JVMFlagWriteable(const char* name, WriteableType type) { _name=name; _type=type; _writeable=true; _startup_done=false; }
  ~JVMFlagWriteable() { }
  const char* name() { return _name; }
  const WriteableType type() { return _type; }
  bool is_writeable(void);
  void mark_once(void);
  void mark_startup(void);
};

class JVMFlagWriteableList : public AllStatic {
  static GrowableArray<JVMFlagWriteable*>* _controls;
public:
  static void init();
  static int length() { return (_controls != NULL) ? _controls->length() : 0; }
  static JVMFlagWriteable* at(int i) { return (_controls != NULL) ? _controls->at(i) : NULL; }
  static JVMFlagWriteable* find(const char* name);
  static void add(JVMFlagWriteable* range) { _controls->append(range); }
  static void mark_startup(void);
};

#endif

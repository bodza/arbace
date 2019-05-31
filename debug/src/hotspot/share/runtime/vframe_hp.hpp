#ifndef SHARE_VM_RUNTIME_VFRAME_HP_HPP
#define SHARE_VM_RUNTIME_VFRAME_HP_HPP

#include "runtime/vframe.hpp"

class compiledVFrame: public javaVFrame {
 public:
  // JVM state
  Method*                      method()             const;
  int                          bci()                const;
  bool                         should_reexecute()   const;
  StackValueCollection*        locals()             const;
  StackValueCollection*        expressions()        const;
  GrowableArray<MonitorInfo*>* monitors()           const;
  int                          vframe_id()          const { return _vframe_id; }

  void set_locals(StackValueCollection* values) const;

  // Virtuals defined in vframe
  bool is_compiled_frame() const { return true; }
  vframe* sender() const;
  bool is_top() const;

  // Casting
  static compiledVFrame* cast(vframe* vf) {
    return (compiledVFrame*) vf;
  }

 public:
  // Constructors
  compiledVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread, CompiledMethod* nm);

  // Update a local in a compiled frame. Update happens when deopt occurs
  void update_local(BasicType type, int index, jvalue value);

  // Update an expression stack value in a compiled frame. Update happens when deopt occurs
  void update_stack(BasicType type, int index, jvalue value);

  // Update a lock value in a compiled frame. Update happens when deopt occurs
  void update_monitor(int index, MonitorInfo* value);

  // Returns the active nmethod
  CompiledMethod*  code() const;

  // Returns the scopeDesc
  ScopeDesc* scope() const { return _scope; }

  // Returns SynchronizationEntryBCI or bci() (used for synchronization)
  int raw_bci() const;

 protected:
  ScopeDesc* _scope;
  int _vframe_id;

  //StackValue resolve(ScopeValue* sv) const;
  BasicLock* resolve_monitor_lock(Location location) const;
  StackValue *create_stack_value(ScopeValue *sv) const;

 private:
  compiledVFrame(const frame* fr, const RegisterMap* reg_map, JavaThread* thread, ScopeDesc* scope, int vframe_id);
};

#endif

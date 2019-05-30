#ifndef SHARE_VM_RUNTIME_OBJECTMONITOR_INLINE_HPP
#define SHARE_VM_RUNTIME_OBJECTMONITOR_INLINE_HPP

inline intptr_t ObjectMonitor::is_entered(TRAPS) const {
  if (THREAD == _owner || THREAD->is_lock_owned((address) _owner)) {
    return 1;
  }
  return 0;
}

inline markOop ObjectMonitor::header() const {
  return _header;
}

inline volatile markOop* ObjectMonitor::header_addr() {
  assert((intptr_t)this == (intptr_t)&_header, "sync code expects this");
  return &_header;
}

inline void ObjectMonitor::set_header(markOop hdr) {
  _header = hdr;
}

inline jint ObjectMonitor::count() const {
  return _count;
}

inline jint ObjectMonitor::waiters() const {
  return _waiters;
}

inline void* ObjectMonitor::owner() const {
  return _owner;
}

inline void ObjectMonitor::clear() {
  assert(_header, "Fatal logic error in ObjectMonitor header!");
  assert(_count == 0, "Fatal logic error in ObjectMonitor count!");
  assert(_waiters == 0, "Fatal logic error in ObjectMonitor waiters!");
  assert(_recursions == 0, "Fatal logic error in ObjectMonitor recursions!");
  assert(_object != NULL, "Fatal logic error in ObjectMonitor object!");
  assert(_owner == 0, "Fatal logic error in ObjectMonitor owner!");

  _header = NULL;
  _object = NULL;
}

inline void* ObjectMonitor::object() const {
  return _object;
}

inline void* ObjectMonitor::object_addr() {
  return (void *)(&_object);
}

inline void ObjectMonitor::set_object(void* obj) {
  _object = obj;
}

inline bool ObjectMonitor::check(TRAPS) {
  if (THREAD != _owner) {
    if (THREAD->is_lock_owned((address) _owner)) {
      _owner = THREAD;  // regain ownership of inflated monitor
      assert(_recursions == 0, "invariant");
    } else {
      check_slow(THREAD);
      return false;
    }
  }
  return true;
}

// return number of threads contending for this monitor
inline jint ObjectMonitor::contentions() const {
  return _count;
}

// Do NOT set _count = 0. There is a race such that _count could
// be set while inflating prior to setting _owner
// Just use Atomic::inc/dec and assert 0 when monitor put on free list
inline void ObjectMonitor::set_owner(void* owner) {
  _owner = owner;
  _recursions = 0;
}

#endif

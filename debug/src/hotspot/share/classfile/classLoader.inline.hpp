#ifndef SHARE_VM_CLASSFILE_CLASSLOADER_INLINE_HPP
#define SHARE_VM_CLASSFILE_CLASSLOADER_INLINE_HPP

#include "classfile/classLoader.hpp"
#include "runtime/orderAccess.hpp"

// Next entry in class path
inline ClassPathEntry* ClassPathEntry::next() const { return OrderAccess::load_acquire(&_next); }

inline void ClassPathEntry::set_next(ClassPathEntry* next) {
  // may have unlocked readers, so ensure visibility.
  OrderAccess::release_store(&_next, next);
}

inline ClassPathEntry* ClassLoader::classpath_entry(int n) {
  if (n == 0) {
    return ClassLoader::_jrt_entry;
  } else {
    ClassPathEntry* e = ClassLoader::_first_append_entry;
    while (--n >= 1) {
      e = e->next();
    }
    return e;
  }
}

#endif

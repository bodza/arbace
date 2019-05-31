#include "precompiled.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"
#include "utilities/accessFlags.hpp"

void AccessFlags::atomic_set_bits(jint bits) {
  // Atomically update the flags with the bits given
  jint old_flags, new_flags, f;
  do {
    old_flags = _flags;
    new_flags = old_flags | bits;
    f = Atomic::cmpxchg(new_flags, &_flags, old_flags);
  } while(f != old_flags);
}

void AccessFlags::atomic_clear_bits(jint bits) {
  // Atomically update the flags with the bits given
  jint old_flags, new_flags, f;
  do {
    old_flags = _flags;
    new_flags = old_flags & ~bits;
    f = Atomic::cmpxchg(new_flags, &_flags, old_flags);
  } while(f != old_flags);
}

void accessFlags_init() {
}

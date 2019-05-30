#ifndef SHARE_VM_GC_SHARED_GCID_HPP
#define SHARE_VM_GC_SHARED_GCID_HPP

#include "memory/allocation.hpp"

class GCId : public AllStatic {
private:
  friend class GCIdMark;

  static uint _next_id;
  static const uint UNDEFINED = (uint)-1;
  static uint create();

public:
  // Returns the currently active GC id. Asserts that there is an active GC id.
  static uint current();
  // Same as current() but can return undefined() if no GC id is currently active
  static uint current_or_undefined();
  // Returns the next expected GCId.
  static uint peek();
  static uint undefined() { return UNDEFINED; }
  static size_t print_prefix(char* buf, size_t len);
};

class GCIdMark : public StackObj {
private:
  const uint _previous_gc_id;

public:
  GCIdMark();
  GCIdMark(uint gc_id);
  ~GCIdMark();
};

#endif

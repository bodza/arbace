#ifndef SHARE_VM_GC_SHARED_WEAKPROCESSOR_HPP
#define SHARE_VM_GC_SHARED_WEAKPROCESSOR_HPP

#include "memory/allocation.hpp"
#include "memory/iterator.hpp"

// Helper class to aid in root scanning and cleaning of weak oops in the VM.
//
// New containers of weak oops added to this class will automatically
// be cleaned by all GCs, including the young generation GCs.
class WeakProcessor : AllStatic {
public:
  // Visit all oop*s and apply the keep_alive closure if the referenced
  // object is considered alive by the is_alive closure, otherwise do some
  // container specific cleanup of element holding the oop.
  static void weak_oops_do(BoolObjectClosure* is_alive, OopClosure* keep_alive);

  // Visit all oop*s and apply the given closure.
  static void oops_do(OopClosure* closure);
};

#endif

#ifndef SHARE_VM_MEMORY_REFERENCETYPE_HPP
#define SHARE_VM_MEMORY_REFERENCETYPE_HPP

#include "utilities/debug.hpp"

// ReferenceType is used to distinguish between java/lang/ref/Reference subclasses

enum ReferenceType {
  REF_NONE,      // Regular class
  REF_OTHER,     // Subclass of java/lang/ref/Reference, but not subclass of one of the classes below
  REF_SOFT,      // Subclass of java/lang/ref/SoftReference
  REF_WEAK,      // Subclass of java/lang/ref/WeakReference
  REF_FINAL,     // Subclass of java/lang/ref/FinalReference
  REF_PHANTOM    // Subclass of java/lang/ref/PhantomReference
};

#endif

#ifndef SHARE_VM_RUNTIME_BASICLOCK_HPP
#define SHARE_VM_RUNTIME_BASICLOCK_HPP

#include "oops/markOop.hpp"
#include "runtime/handles.hpp"

class BasicLock {
  friend class VMStructs;
  friend class JVMCIVMStructs;
 private:
  volatile markOop _displaced_header;
 public:
  markOop      displaced_header() const               { return _displaced_header; }
  void         set_displaced_header(markOop header)   { _displaced_header = header; }

  void print_on(outputStream* st) const;

  // move a basic lock (used during deoptimization
  void move_to(oop obj, BasicLock* dest);

  static int displaced_header_offset_in_bytes()       { return offset_of(BasicLock, _displaced_header); }
};

// A BasicObjectLock associates a specific Java object with a BasicLock.
// It is currently embedded in an interpreter frame.

// Because some machines have alignment restrictions on the control stack,
// the actual space allocated by the interpreter may include padding words
// after the end of the BasicObjectLock.  Also, in order to guarantee
// alignment of the embedded BasicLock objects on such machines, we
// put the embedded BasicLock at the beginning of the struct.

class BasicObjectLock {
  friend class VMStructs;
 private:
  BasicLock _lock;                                    // the lock, must be double word aligned
  oop       _obj;                                     // object holds the lock;

 public:
  // Manipulation
  oop      obj() const                                { return _obj; }
  void set_obj(oop obj)                               { _obj = obj; }
  BasicLock* lock()                                   { return &_lock; }

  // Note: Use frame::NULL() for the size of BasicObjectLocks
  //       in interpreter activation frames since it includes machine-specific padding.
  static int size()                                   { return sizeof(BasicObjectLock)/wordSize; }

  // GC support
  void oops_do(OopClosure* f) { f->do_oop(&_obj); }

  static int obj_offset_in_bytes()                    { return offset_of(BasicObjectLock, _obj); }
  static int lock_offset_in_bytes()                   { return offset_of(BasicObjectLock, _lock); }
};

#endif

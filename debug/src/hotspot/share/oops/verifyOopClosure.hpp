#ifndef SHARE_VM_OOPS_VERIFYOOPCLOSURE_HPP
#define SHARE_VM_OOPS_VERIFYOOPCLOSURE_HPP

#include "memory/iterator.hpp"

class VerifyOopClosure: public OopClosure {
 protected:
  template <class T> void do_oop_work(T* p);
 public:
  virtual void do_oop(oop* p);
  virtual void do_oop(narrowOop* p);
};

#endif

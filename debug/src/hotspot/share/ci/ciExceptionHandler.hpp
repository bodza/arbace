#ifndef SHARE_VM_CI_CIEXCEPTIONHANDLER_HPP
#define SHARE_VM_CI_CIEXCEPTIONHANDLER_HPP

#include "ci/ciClassList.hpp"
#include "ci/ciInstanceKlass.hpp"

// ciExceptionHandler
//
// This class represents an exception handler for a method.
class ciExceptionHandler : public ResourceObj {
private:
  friend class ciMethod;

  // The loader to be used for resolving the exception
  // klass.
  ciInstanceKlass* _loading_klass;

  // Handler data.
  int _start;
  int _limit;
  int _handler_bci;
  int _catch_klass_index;

  // The exception klass that this handler catches.
  ciInstanceKlass* _catch_klass;

public:
  ciExceptionHandler(ciInstanceKlass* loading_klass, int start, int limit, int handler_bci, int klass_index) {
    _loading_klass = loading_klass;
    _start  = start;
    _limit  = limit;
    _handler_bci = handler_bci;
    _catch_klass_index = klass_index;
    _catch_klass = NULL;
  }

  int       start()             { return _start; }
  int       limit()             { return _limit; }
  int       handler_bci()       { return _handler_bci; }
  int       catch_klass_index() { return _catch_klass_index; }

  // Get the exception klass that this handler catches.
  ciInstanceKlass* catch_klass();

  bool      is_catch_all()                { return catch_klass_index() == 0; }
  bool      is_in_range(int bci)          { return start() <= bci && bci < limit(); }
  bool      catches(ciInstanceKlass *exc) { return is_catch_all() || exc->is_subtype_of(catch_klass()); }
  bool      is_rethrow()                  { return handler_bci() == -1; }

  void      print();
};

#endif

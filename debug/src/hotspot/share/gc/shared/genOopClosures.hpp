#ifndef SHARE_VM_GC_SHARED_GENOOPCLOSURES_HPP
#define SHARE_VM_GC_SHARED_GENOOPCLOSURES_HPP

#include "memory/iterator.hpp"
#include "oops/oop.hpp"

class Generation;
class HeapWord;
class CardTableRS;
class CardTableBarrierSet;
class DefNewGeneration;
class KlassRemSet;

// Closure for iterating roots from a particular generation
// Note: all classes deriving from this MUST call this do_barrier
// method at the end of their own do_oop method!
// Note: no do_oop defined, this is an abstract class.

class OopsInGenClosure : public OopIterateClosure {
 private:
  Generation*  _orig_gen;     // generation originally set in ctor
  Generation*  _gen;          // generation being scanned

 protected:
  // Some subtypes need access.
  HeapWord*    _gen_boundary; // start of generation
  CardTableRS* _rs;           // remembered set

  // For assertions
  Generation* generation() { return _gen; }
  CardTableRS* rs() { return _rs; }

  // Derived classes that modify oops so that they might be old-to-young
  // pointers must call the method below.
  template <class T> void do_barrier(T* p);

  // Version for use by closures that may be called in parallel code.
  template <class T> void par_do_barrier(T* p);

 public:
  OopsInGenClosure() : OopIterateClosure(NULL),
    _orig_gen(NULL), _gen(NULL), _gen_boundary(NULL), _rs(NULL) { };

  OopsInGenClosure(Generation* gen);
  void set_generation(Generation* gen);

  void reset_generation() { _gen = _orig_gen; }

  // Problem with static closures: must have _gen_boundary set at some point,
  // but cannot do this until after the heap is initialized.
  void set_orig_generation(Generation* gen) {
    _orig_gen = gen;
    set_generation(gen);
  }

  HeapWord* gen_boundary() { return _gen_boundary; }
};

class BasicOopsInGenClosure: public OopsInGenClosure {
 public:
  BasicOopsInGenClosure() : OopsInGenClosure() { }
  BasicOopsInGenClosure(Generation* gen);

  virtual bool do_metadata() { return false; }
  virtual void do_klass(Klass* k) { ShouldNotReachHere(); }
  virtual void do_cld(ClassLoaderData* cld) { ShouldNotReachHere(); }
};

// Super class for scan closures. It contains code to dirty scanned class loader data.
class OopsInClassLoaderDataOrGenClosure: public BasicOopsInGenClosure {
  ClassLoaderData* _scanned_cld;
 public:
  OopsInClassLoaderDataOrGenClosure(Generation* g) : BasicOopsInGenClosure(g), _scanned_cld(NULL) { }
  void set_scanned_cld(ClassLoaderData* cld) {
    _scanned_cld = cld;
  }
  bool is_scanning_a_cld() { return _scanned_cld != NULL; }
  void do_cld_barrier();
};

class CLDScanClosure: public CLDClosure {
  OopsInClassLoaderDataOrGenClosure*   _scavenge_closure;
  // true if the the modified oops state should be saved.
  bool                                 _accumulate_modified_oops;
 public:
  CLDScanClosure(OopsInClassLoaderDataOrGenClosure* scavenge_closure,
                 bool accumulate_modified_oops) :
       _scavenge_closure(scavenge_closure), _accumulate_modified_oops(accumulate_modified_oops) { }
  void do_cld(ClassLoaderData* cld);
};

class FilteringClosure: public OopIterateClosure {
 private:
  HeapWord*   _boundary;
  OopIterateClosure* _cl;
 protected:
  template <class T> inline void do_oop_work(T* p);
 public:
  FilteringClosure(HeapWord* boundary, OopIterateClosure* cl) :
    OopIterateClosure(cl->ref_discoverer()), _boundary(boundary),
    _cl(cl) { }
  virtual void do_oop(oop* p);
  virtual void do_oop(narrowOop* p);
  virtual bool do_metadata()            {
    return false; }
  virtual void do_klass(Klass*)         { ShouldNotReachHere(); }
  virtual void do_cld(ClassLoaderData*) { ShouldNotReachHere(); }
};

#endif

#ifndef SHARE_GC_G1_G1FULLGCOOPCLOSURES_HPP
#define SHARE_GC_G1_G1FULLGCOOPCLOSURES_HPP

#include "memory/iterator.hpp"
#include "memory/universe.hpp"

class G1CollectedHeap;
class G1FullCollector;
class G1CMBitMap;
class G1FullGCMarker;

// Below are closures used by the G1 Full GC.
class G1IsAliveClosure : public BoolObjectClosure {
  G1CMBitMap* _bitmap;

public:
  G1IsAliveClosure(G1CMBitMap* bitmap) : _bitmap(bitmap) { }

  virtual bool do_object_b(oop p);
};

class G1FullKeepAliveClosure: public OopClosure {
  G1FullGCMarker* _marker;
  template <class T>
  inline void do_oop_work(T* p);

public:
  G1FullKeepAliveClosure(G1FullGCMarker* pm) : _marker(pm) { }

  virtual void do_oop(oop* p);
  virtual void do_oop(narrowOop* p);
};

class G1MarkAndPushClosure : public OopIterateClosure {
  G1FullGCMarker* _marker;
  uint _worker_id;

public:
  G1MarkAndPushClosure(uint worker, G1FullGCMarker* marker, ReferenceDiscoverer* ref) :
    _marker(marker),
    _worker_id(worker),
    OopIterateClosure(ref) { }

  template <class T> inline void do_oop_work(T* p);
  virtual void do_oop(oop* p);
  virtual void do_oop(narrowOop* p);

  virtual bool do_metadata();
  virtual void do_klass(Klass* k);
  virtual void do_cld(ClassLoaderData* cld);
};

class G1AdjustClosure : public BasicOopIterateClosure {
  template <class T> static inline void adjust_pointer(T* p);
public:
  template <class T> void do_oop_work(T* p) { adjust_pointer(p); }
  virtual void do_oop(oop* p);
  virtual void do_oop(narrowOop* p);

  virtual ReferenceIterationMode reference_iteration_mode() { return DO_FIELDS; }
};

class G1VerifyOopClosure: public BasicOopIterateClosure {
private:
  G1CollectedHeap* _g1h;
  bool             _failures;
  oop              _containing_obj;
  VerifyOption     _verify_option;

public:
  int _cc;
  G1VerifyOopClosure(VerifyOption option);

  void set_containing_obj(oop obj) {
    _containing_obj = obj;
  }

  bool failures() { return _failures; }
  void print_object(outputStream* out, oop obj);

  template <class T> void do_oop_work(T* p);

  void do_oop(oop* p)       { do_oop_work(p); }
  void do_oop(narrowOop* p) { do_oop_work(p); }
};

class G1FollowStackClosure: public VoidClosure {
  G1FullGCMarker* _marker;

public:
  G1FollowStackClosure(G1FullGCMarker* marker) : _marker(marker) {}
  virtual void do_void();
};

#endif

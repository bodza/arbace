#ifndef SHARE_VM_GC_G1_G1CODEBLOBCLOSURE_HPP
#define SHARE_VM_GC_G1_G1CODEBLOBCLOSURE_HPP

#include "gc/g1/g1CollectedHeap.hpp"
#include "memory/iterator.hpp"

class nmethod;

class G1CodeBlobClosure : public CodeBlobClosure {
  class HeapRegionGatheringOopClosure : public OopClosure {
    G1CollectedHeap* _g1h;
    OopClosure* _work;
    nmethod* _nm;

    template <typename T>
    void do_oop_work(T* p);

  public:
    HeapRegionGatheringOopClosure(OopClosure* oc) : _g1h(G1CollectedHeap::heap()), _work(oc), _nm(NULL) {}

    void do_oop(oop* o);
    void do_oop(narrowOop* o);

    void set_nm(nmethod* nm) {
      _nm = nm;
    }
  };

  HeapRegionGatheringOopClosure _oc;
public:
  G1CodeBlobClosure(OopClosure* oc) : _oc(oc) {}

  void do_code_blob(CodeBlob* cb);
};

#endif

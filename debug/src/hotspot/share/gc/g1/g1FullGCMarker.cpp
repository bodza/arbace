#include "precompiled.hpp"

#include "gc/g1/g1FullGCMarker.inline.hpp"
#include "gc/shared/referenceProcessor.hpp"
#include "memory/iterator.inline.hpp"

G1FullGCMarker::G1FullGCMarker(uint worker_id, PreservedMarks* preserved_stack, G1CMBitMap* bitmap) :
    _worker_id(worker_id),
    _mark_closure(worker_id, this, G1CollectedHeap::heap()->ref_processor_stw()),
    _verify_closure(VerifyOption_G1UseFullMarking),
    _cld_closure(mark_closure()),
    _stack_closure(this),
    _preserved_stack(preserved_stack),
    _bitmap(bitmap) {
  _oop_stack.initialize();
  _objarray_stack.initialize();
}

G1FullGCMarker::~G1FullGCMarker() { }

void G1FullGCMarker::complete_marking(OopQueueSet* oop_stacks, ObjArrayTaskQueueSet* array_stacks, ParallelTaskTerminator* terminator) {
  int hash_seed = 17;
  do {
    drain_stack();
    ObjArrayTask steal_array;
    if (array_stacks->steal(_worker_id, &hash_seed, steal_array)) {
      follow_array_chunk(objArrayOop(steal_array.obj()), steal_array.index());
    } else {
      oop steal_oop;
      if (oop_stacks->steal(_worker_id, &hash_seed, steal_oop)) {
        follow_object(steal_oop);
      }
    }
  } while (!is_empty() || !terminator->offer_termination());
}

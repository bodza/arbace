#ifndef SHARE_GC_G1_G1FULLGCMARKER_HPP
#define SHARE_GC_G1_G1FULLGCMARKER_HPP

#include "gc/g1/g1FullGCOopClosures.hpp"
#include "gc/shared/preservedMarks.hpp"
#include "gc/shared/taskqueue.hpp"
#include "memory/iterator.hpp"
#include "oops/markOop.hpp"
#include "oops/oop.hpp"
#include "runtime/timer.hpp"
#include "utilities/chunkedList.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/stack.hpp"

typedef OverflowTaskQueue<oop, mtGC>                 OopQueue;
typedef OverflowTaskQueue<ObjArrayTask, mtGC>        ObjArrayTaskQueue;

typedef GenericTaskQueueSet<OopQueue, mtGC>          OopQueueSet;
typedef GenericTaskQueueSet<ObjArrayTaskQueue, mtGC> ObjArrayTaskQueueSet;

class G1CMBitMap;

class G1FullGCMarker : public CHeapObj<mtGC> {
private:
  uint               _worker_id;
  // Backing mark bitmap
  G1CMBitMap*        _bitmap;

  // Mark stack
  OopQueue           _oop_stack;
  ObjArrayTaskQueue  _objarray_stack;
  PreservedMarks*    _preserved_stack;

  // Marking closures
  G1MarkAndPushClosure _mark_closure;
  G1VerifyOopClosure   _verify_closure;
  G1FollowStackClosure _stack_closure;
  CLDToOopClosure      _cld_closure;

  inline bool is_empty();
  inline bool pop_object(oop& obj);
  inline bool pop_objarray(ObjArrayTask& array);
  inline void push_objarray(oop obj, size_t index);
  inline bool mark_object(oop obj);

  // Marking helpers
  inline void follow_object(oop obj);
  inline void follow_array(objArrayOop array);
  inline void follow_array_chunk(objArrayOop array, int index);
public:
  G1FullGCMarker(uint worker_id, PreservedMarks* preserved_stack, G1CMBitMap* bitmap);
  ~G1FullGCMarker();

  // Stack getters
  OopQueue*          oop_stack()       { return &_oop_stack; }
  ObjArrayTaskQueue* objarray_stack()  { return &_objarray_stack; }
  PreservedMarks*    preserved_stack() { return _preserved_stack; }

  // Marking entry points
  template <class T> inline void mark_and_push(T* p);
  inline void follow_klass(Klass* k);
  inline void follow_cld(ClassLoaderData* cld);

  inline void drain_stack();
  void complete_marking(OopQueueSet* oop_stacks,
                        ObjArrayTaskQueueSet* array_stacks,
                        ParallelTaskTerminator* terminator);

  // Closure getters
  CLDToOopClosure*      cld_closure()   { return &_cld_closure; }
  G1MarkAndPushClosure* mark_closure()  { return &_mark_closure; }
  G1FollowStackClosure* stack_closure() { return &_stack_closure; }
};

#endif

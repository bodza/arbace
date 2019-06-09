#include "precompiled.hpp"

#include "classfile/vmSymbols.hpp"
#include "code/vmreg.inline.hpp"
#include "interpreter/bytecode.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/methodData.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/monitorChunk.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/vframe.hpp"
#include "runtime/vframeArray.hpp"
#include "runtime/vframe_hp.hpp"
#include "utilities/copy.hpp"

int vframeArrayElement::bci(void) const { return (_bci == SynchronizationEntryBCI ? 0 : _bci); }

void vframeArrayElement::free_monitors(JavaThread* jt) {
  if (_monitors != NULL) {
     MonitorChunk* chunk = _monitors;
     _monitors = NULL;
     jt->remove_monitor_chunk(chunk);
     delete chunk;
  }
}

void vframeArrayElement::fill_in(compiledVFrame* vf, bool realloc_failures) {
// Copy the information from the compiled vframe to the
// interpreter frame we will be creating to replace vf

  _method = vf->method();
  _bci    = vf->raw_bci();
  _reexecute = vf->should_reexecute();

  int index;

  // Get the monitors off-stack

  GrowableArray<MonitorInfo*>* list = vf->monitors();
  if (list->is_empty()) {
    _monitors = NULL;
  } else {
    // Allocate monitor chunk
    _monitors = new MonitorChunk(list->length());
    vf->thread()->add_monitor_chunk(_monitors);

    // Migrate the BasicLocks from the stack to the monitor chunk
    for (index = 0; index < list->length(); index++) {
      MonitorInfo* monitor = list->at(index);
      BasicObjectLock* dest = _monitors->at(index);
      if (monitor->owner_is_scalar_replaced()) {
        dest->set_obj(NULL);
      } else {
        dest->set_obj(monitor->owner());
        monitor->lock()->move_to(monitor->owner(), dest->lock());
      }
    }
  }

  // Convert the vframe locals and expressions to off stack
  // values. Because we will not gc all oops can be converted to
  // intptr_t (i.e. a stack slot) and we are fine. This is
  // good since we are inside a HandleMark and the oops in our
  // collection would go away between packing them here and
  // unpacking them in ...

  // First the locals go off-stack

  // FIXME this seems silly it creates a StackValueCollection
  // in order to get the size to then copy them and
  // convert the types to intptr_t size slots. Seems like it
  // could do it in place... Still uses less memory than the
  // old way though

  StackValueCollection *locs = vf->locals();
  _locals = new StackValueCollection(locs->size());
  for (index = 0; index < locs->size(); index++) {
    StackValue* value = locs->at(index);
    switch (value->type()) {
      case T_OBJECT:
        // preserve object type
        _locals->add( new StackValue(cast_from_oop<intptr_t>((value->get_obj()())), T_OBJECT ));
        break;
      case T_CONFLICT:
        // A dead local.  Will be initialized to null/zero.
        _locals->add( new StackValue());
        break;
      case T_INT:
        _locals->add( new StackValue(value->get_int()));
        break;
      default:
        ShouldNotReachHere();
    }
  }

  // Now the expressions off-stack
  // Same silliness as above

  StackValueCollection *exprs = vf->expressions();
  _expressions = new StackValueCollection(exprs->size());
  for (index = 0; index < exprs->size(); index++) {
    StackValue* value = exprs->at(index);
    switch (value->type()) {
      case T_OBJECT:
        // preserve object type
        _expressions->add( new StackValue(cast_from_oop<intptr_t>((value->get_obj()())), T_OBJECT ));
        break;
      case T_CONFLICT:
        // A dead stack element.  Will be initialized to null/zero.
        // This can occur when the compiler emits a state in which stack
        // elements are known to be dead (because of an imminent exception).
        _expressions->add( new StackValue());
        break;
      case T_INT:
        _expressions->add( new StackValue(value->get_int()));
        break;
      default:
        ShouldNotReachHere();
    }
  }
}

intptr_t* vframeArray::unextended_sp() const {
  return _original.unextended_sp();
}

vframeArray* vframeArray::allocate(JavaThread* thread, int frame_size, GrowableArray<compiledVFrame*>* chunk,
                                   RegisterMap *reg_map, frame sender, frame caller, frame self,
                                   bool realloc_failures) {
  // Allocate the vframeArray
  vframeArray * result = (vframeArray*) AllocateHeap(sizeof(vframeArray) + // fixed part
                                                     sizeof(vframeArrayElement) * (chunk->length() - 1), // variable part
                                                     mtCompiler);
  result->_frames = chunk->length();
  result->_owner_thread = thread;
  result->_sender = sender;
  result->_caller = caller;
  result->_original = self;
  result->fill_in(thread, frame_size, chunk, reg_map, realloc_failures);
  return result;
}

void vframeArray::fill_in(JavaThread* thread, int frame_size, GrowableArray<compiledVFrame*>* chunk, const RegisterMap *reg_map, bool realloc_failures) {
  // Set owner first, it is used when adding monitor chunks

  _frame_size = frame_size;
  for (int i = 0; i < chunk->length(); i++) {
    element(i)->fill_in(chunk->at(i), realloc_failures);
  }

  // Copy registers for callee-saved registers
  if (reg_map != NULL) {
    for (int i = 0; i < RegisterMap::reg_count; i++) {
#ifdef AMD64
      // The register map has one entry for every int (32-bit value), so
      // 64-bit physical registers have two entries in the map, one for
      // each half.  Ignore the high halves of 64-bit registers, just like
      // frame::oopmapreg_to_location does.
      //
      // [phh] FIXME: this is a temporary hack!  This code *should* work
      // correctly w/o this hack, possibly by changing RegisterMap::pd_location
      // in frame_amd64.cpp and the values of the phantom high half registers
      // in amd64.ad.
      //      if (VMReg::Name(i) < SharedInfo::stack0 && is_even(i)) {
        intptr_t* src = (intptr_t*) reg_map->location(VMRegImpl::as_VMReg(i));
        _callee_registers[i] = src != NULL ? *src : NULL_WORD;
#else
      jint* src = (jint*) reg_map->location(VMRegImpl::as_VMReg(i));
      _callee_registers[i] = src != NULL ? *src : NULL_WORD;
#endif
      if (src == NULL) {
        set_location_valid(i, false);
      } else {
        set_location_valid(i, true);
        jint* dst = (jint*) register_location(i);
        *dst = *src;
      }
    }
  }
}

void vframeArray::deallocate_monitor_chunks() {
  JavaThread* jt = JavaThread::current();
  for (int index = 0; index < frames(); index++) {
     element(index)->free_monitors(jt);
  }
}

address vframeArray::register_location(int i) const {
  return (address) & _callee_registers[i];
}

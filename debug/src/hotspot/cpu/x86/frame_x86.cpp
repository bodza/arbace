#include "precompiled.hpp"

#include "memory/resourceArea.hpp"
#include "oops/markOop.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/monitorChunk.hpp"
#include "runtime/os.inline.hpp"
#include "runtime/signature.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/stubRoutines.hpp"
#include "vmreg_x86.inline.hpp"
#include "c1/c1_Runtime1.hpp"
#include "runtime/vframeArray.hpp"

// Profiling/safepoint support

bool frame::safe_for_sender(JavaThread *thread) {
  address   sp = (address)_sp;
  address   fp = (address)_fp;
  address   unextended_sp = (address)_unextended_sp;

  // consider stack guards when trying to determine "safe" stack pointers
  static size_t stack_guard_size = os::uses_stack_guard_pages() ? JavaThread::stack_red_zone_size() + JavaThread::stack_yellow_zone_size() : 0;
  size_t usable_stack_size = thread->stack_size() - stack_guard_size;

  // sp must be within the usable part of the stack (not in guards)
  bool sp_safe = (sp < thread->stack_base()) && (sp >= thread->stack_base() - usable_stack_size);

  if (!sp_safe) {
    return false;
  }

  // unextended sp must be within the stack and above or equal sp
  bool unextended_sp_safe = (unextended_sp < thread->stack_base()) && (unextended_sp >= sp);

  if (!unextended_sp_safe) {
    return false;
  }

  // an fp must be within the stack and above (but not equal) sp
  // second evaluation on fp+ is added to handle situation where fp is -1
  bool fp_safe = (fp < thread->stack_base() && (fp > sp) && (((fp + (return_addr_offset * sizeof(void*))) < thread->stack_base())));

  // We know sp/unextended_sp are safe only fp is questionable here

  // If the current frame is known to the code cache then we can attempt to
  // to construct the sender and do some validation of it. This goes a long way
  // toward eliminating issues when we get in frame construction code

  if (_cb != NULL) {
    // First check if frame is complete and tester is reliable
    // Unfortunately we can only check frame complete for runtime stubs and nmethod
    // other generic buffer blobs are more problematic so we just assume they are
    // ok. adapter blobs never have a frame complete and are never ok.

    if (!_cb->is_frame_complete_at(_pc)) {
      if (_cb->is_compiled() || _cb->is_adapter_blob() || _cb->is_runtime_stub()) {
        return false;
      }
    }

    // Could just be some random pointer within the codeBlob
    if (!_cb->code_contains(_pc)) {
      return false;
    }

    // Entry frame checks
    if (is_entry_frame()) {
      // an entry frame must have a valid fp.
      return fp_safe && is_entry_frame_valid(thread);
    }

    intptr_t* sender_sp = NULL;
    intptr_t* sender_unextended_sp = NULL;
    address   sender_pc = NULL;
    intptr_t* saved_fp =  NULL;

    {
      // must be some sort of compiled/runtime frame
      // fp does not have to be safe (although it could be check for c1?)

      // check for a valid frame_size, otherwise we are unlikely to get a valid sender_pc
      if (_cb->frame_size() <= 0) {
        return false;
      }

      sender_sp = _unextended_sp + _cb->frame_size();
      // Is sender_sp safe?
      if ((address)sender_sp >= thread->stack_base()) {
        return false;
      }
      sender_unextended_sp = sender_sp;
      // On Intel the return_address is always the word on the stack
      sender_pc = (address) *(sender_sp - 1);
      // Note: frame::sender_sp_offset is only valid for compiled frame
      saved_fp = (intptr_t*) *(sender_sp - frame::sender_sp_offset);
    }

    // We must always be able to find a recognizable pc
    CodeBlob* sender_blob = CodeCache::find_blob_unsafe(sender_pc);
    if (sender_pc == NULL || sender_blob == NULL) {
      return false;
    }

    // Could be a zombie method
    if (sender_blob->is_zombie() || sender_blob->is_unloaded()) {
      return false;
    }

    // Could just be some random pointer within the codeBlob
    if (!sender_blob->code_contains(sender_pc)) {
      return false;
    }

    // We should never be able to see an adapter if the current frame is something from code cache
    if (sender_blob->is_adapter_blob()) {
      return false;
    }

    // Could be the call_stub
    if (StubRoutines::returns_to_call_stub(sender_pc)) {
      bool saved_fp_safe = ((address)saved_fp < thread->stack_base()) && (saved_fp > sender_sp);

      if (!saved_fp_safe) {
        return false;
      }

      // construct the potential sender

      frame sender(sender_sp, sender_unextended_sp, saved_fp, sender_pc);

      // Validate the JavaCallWrapper an entry frame must have
      address jcw = (address)sender.entry_frame_call_wrapper();

      bool jcw_safe = (jcw < thread->stack_base()) && (jcw > (address)sender.fp());

      return jcw_safe;
    }

    CompiledMethod* nm = sender_blob->as_compiled_method_or_null();
    if (nm != NULL) {
        if (nm->is_deopt_mh_entry(sender_pc) || nm->is_deopt_entry(sender_pc) || nm->method()->is_method_handle_intrinsic()) {
            return false;
        }
    }

    // If the frame size is 0 something (or less) is bad because every nmethod has a non-zero frame size
    // because the return address counts against the callee's frame.

    if (sender_blob->frame_size() <= 0) {
      return false;
    }

    // We should never be able to see anything here except an nmethod. If something in the
    // code cache (current frame) is called by an entity within the code cache that entity
    // should not be anything but the call stub (already covered), the interpreter (already covered)
    // or an nmethod.

    if (!sender_blob->is_compiled()) {
        return false;
    }

    // Could put some more validation for the potential non-interpreted sender
    // frame we'd create by calling sender if I could think of any. Wait for next crash in forte...

    // One idea is seeing if the sender_pc we have is one that we'd expect to call to current cb

    // We've validated the potential sender that would be created
    return true;
  }

  // Must be native-compiled frame. Since sender will try and use fp to find
  // linkages it must be safe

  if (!fp_safe) {
    return false;
  }

  // Will the pc we fetch be non-zero (which we'll find at the oldest frame)

  if ((address) this->fp()[return_addr_offset] == NULL) return false;

  // could try and do some more potential verification of native frame if we could think of some...

  return true;
}

void frame::patch_pc(Thread* thread, address pc) {
  address* pc_addr = &(((address*) sp())[-1]);
  // Either the return address is the original one or we are going to
  // patch in the same address that's already there.
  *pc_addr = pc;
  _cb = CodeCache::find_blob(pc);
  address original_pc = CompiledMethod::get_deopt_original_pc(this);
  if (original_pc != NULL) {
    _deopt_state = is_deoptimized;
    // leave _pc as is
  } else {
    _deopt_state = not_deoptimized;
    _pc = pc;
  }
}

int frame::frame_size(RegisterMap* map) const {
  frame sender = this->sender(map);
  return sender.sp() - sp();
}

intptr_t* frame::entry_frame_argument_at(int offset) const {
  // convert offset to index to deal with tsi
  int index = (NULL::expr_offset_in_bytes(offset)/wordSize);
  // Entry frame's arguments are always in relation to unextended_sp()
  return &unextended_sp()[index];
}

frame frame::sender_for_entry_frame(RegisterMap* map) const {
  // Java frame called from C; skip all C frames and return top C
  // frame of that chunk as the sender
  JavaFrameAnchor* jfa = entry_frame_call_wrapper()->anchor();
  // Since we are walking the stack now this nested anchor is obviously walkable
  // even if it wasn't when it was stacked.
  if (!jfa->walkable()) {
    // Capture _last_Java_pc (if needed) and mark anchor walkable.
    jfa->capture_last_Java_pc();
  }
  map->clear();
  vmassert(jfa->last_Java_pc() != NULL, "not walkable");
  frame fr(jfa->last_Java_sp(), jfa->last_Java_fp(), jfa->last_Java_pc());
  return fr;
}

//------------------------------------------------------------------------------
// frame::verify_deopt_original_pc
//
// Verifies the calculated original PC of a deoptimization PC for the
// given unextended SP.

//------------------------------------------------------------------------------
// frame::adjust_unextended_sp

//------------------------------------------------------------------------------
// frame::update_map_with_saved_link
void frame::update_map_with_saved_link(RegisterMap* map, intptr_t** link_addr) {
  // The interpreter and compiler(s) always save EBP/RBP in a known
  // location on entry. We must record where that location is
  // so this if EBP/RBP was live on callout from c2 we can find
  // the saved copy no matter what it called.

  // Since the interpreter always saves EBP/RBP if we record where it is then
  // we don't have to always save EBP/RBP on entry and exit to c2 compiled
  // code, on entry will be enough.
  map->set_location(rbp->as_VMReg(), (address) link_addr);
#ifdef AMD64
  // this is weird "H" ought to be at a higher address however the
  // oopMaps seems to have the "H" regs at the same address and the
  // vanilla register.
  // XXXX make this go away
  if (true) {
    map->set_location(rbp->as_VMReg()->next(), (address) link_addr);
  }
#endif
}

//------------------------------------------------------------------------------
// frame::sender_for_compiled_frame
frame frame::sender_for_compiled_frame(RegisterMap* map) const {
  intptr_t* sender_sp = unextended_sp() + _cb->frame_size();
  intptr_t* unextended_sp = sender_sp;

  // On Intel the return_address is always the word on the stack
  address sender_pc = (address) *(sender_sp - 1);

  // This is the saved value of EBP which may or may not really be an FP.
  // It is only an FP if the sender is an interpreter frame (or C1?).
  intptr_t** saved_fp_addr = (intptr_t**) (sender_sp - frame::sender_sp_offset);

  if (map->update_map()) {
    // Tell GC to use argument oopmaps for some runtime stubs that need it.
    // For C1, the runtime stub might not have oop maps, so set this flag
    // outside of update_register_map.
    map->set_include_argument_oops(_cb->caller_must_gc_arguments(map->thread()));
    if (_cb->oop_maps() != NULL) {
      OopMapSet::update_register_map(this, map);
    }

    // Since the prolog does the save and restore of EBP there is no oopmap
    // for it so we must fill in its location as if there was an oopmap entry
    // since if our caller was compiled code there could be live jvm state in it.
    update_map_with_saved_link(map, saved_fp_addr);
  }

  return frame(sender_sp, unextended_sp, *saved_fp_addr, sender_pc);
}

//------------------------------------------------------------------------------
// frame::sender
frame frame::sender(RegisterMap* map) const {
  // Default is we done have to follow them. The sender_for_xxx will
  // update it accordingly
  map->set_include_argument_oops(false);

  if (is_entry_frame()) {
    return sender_for_entry_frame(map);
  }

  if (_cb != NULL) {
    return sender_for_compiled_frame(map);
  }
  // Must be native-compiled frame, i.e. the marshaling code for native
  // methods that exists in the core system.
  return frame(sender_sp(), link(), sender_pc());
}

intptr_t *frame::initial_deoptimization_info() {
  // used to reset the saved FP
  return fp();
}

intptr_t* frame::real_fp() const {
  if (_cb != NULL) {
    // use the frame size if valid
    int size = _cb->frame_size();
    if (size > 0) {
      return unextended_sp() + size;
    }
  }
  return fp();
}

void JavaFrameAnchor::make_walkable(JavaThread* thread) {
  // last frame set?
  if (last_Java_sp() == NULL) return;
  // already walkable?
  if (walkable()) return;
  vmassert(Thread::current() == (Thread*)thread, "not current thread");
  vmassert(last_Java_sp() != NULL, "not called from Java code?");
  vmassert(last_Java_pc() == NULL, "already walkable");
  capture_last_Java_pc();
  vmassert(walkable(), "something went wrong");
}

void JavaFrameAnchor::capture_last_Java_pc() {
  vmassert(_last_Java_sp != NULL, "no last frame set");
  vmassert(_last_Java_pc == NULL, "already walkable");
  _last_Java_pc = (address)_last_Java_sp[-1];
}

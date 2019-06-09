#include "precompiled.hpp"

#include "gc/shared/barrierSetAssembler.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "interpreter/interp_masm.hpp"
#include "runtime/jniHandles.hpp"
#include "runtime/thread.hpp"

#define __ masm->

void BarrierSetAssembler::load_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Register dst, Address src, Register tmp1, Register tmp_thread) {
  bool in_heap = (decorators & IN_HEAP) != 0;
  bool in_native = (decorators & IN_NATIVE) != 0;
  bool is_not_null = (decorators & IS_NOT_NULL) != 0;
  bool atomic = (decorators & MO_RELAXED) != 0;

  switch (type) {
  case T_OBJECT:
  case T_ARRAY: {
    if (in_heap) {
      if (UseCompressedOops) {
        __ movl(dst, src);
        if (is_not_null) {
          __ decode_heap_oop_not_null(dst);
        } else {
          __ decode_heap_oop(dst);
        }
      } else {
        __ movptr(dst, src);
      }
    } else {
      __ movptr(dst, src);
    }
    break;
  }
  case T_BOOLEAN: __ load_unsigned_byte(dst, src);  break;
  case T_BYTE:    __ load_signed_byte(dst, src);    break;
  case T_CHAR:    __ load_unsigned_short(dst, src); break;
  case T_SHORT:   __ load_signed_short(dst, src);   break;
  case T_INT:     __ movl  (dst, src);              break;
  case T_ADDRESS: __ movptr(dst, src);              break;
  case T_FLOAT:
    __ load_float(src);
    break;
  case T_DOUBLE:
    __ load_double(src);
    break;
  case T_LONG:
    __ movq(rax, src);
    break;
  default: Unimplemented();
  }
}

void BarrierSetAssembler::store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type, Address dst, Register val, Register tmp1, Register tmp2) {
  bool in_heap = (decorators & IN_HEAP) != 0;
  bool in_native = (decorators & IN_NATIVE) != 0;
  bool is_not_null = (decorators & IS_NOT_NULL) != 0;
  bool atomic = (decorators & MO_RELAXED) != 0;

  switch (type) {
  case T_OBJECT:
  case T_ARRAY: {
    if (in_heap) {
      if (val == noreg) {
        if (UseCompressedOops) {
          __ movl(dst, (int32_t)NULL_WORD);
        } else {
          __ movslq(dst, (int32_t)NULL_WORD);
        }
      } else {
        if (UseCompressedOops) {
          if (is_not_null) {
            __ encode_heap_oop_not_null(val);
          } else {
            __ encode_heap_oop(val);
          }
          __ movl(dst, val);
        } else {
          __ movptr(dst, val);
        }
      }
    } else {
      __ movptr(dst, val);
    }
    break;
  }
  case T_BOOLEAN:
    __ andl(val, 0x1);  // boolean is true if LSB is 1
    __ movb(dst, val);
    break;
  case T_BYTE:
    __ movb(dst, val);
    break;
  case T_SHORT:
    __ movw(dst, val);
    break;
  case T_CHAR:
    __ movw(dst, val);
    break;
  case T_INT:
    __ movl(dst, val);
    break;
  case T_LONG:
    __ movq(dst, rax);
    break;
  case T_FLOAT:
    __ store_float(dst);
    break;
  case T_DOUBLE:
    __ store_double(dst);
    break;
  case T_ADDRESS:
    __ movptr(dst, val);
    break;
  default: Unimplemented();
  }
}

void BarrierSetAssembler::obj_equals(MacroAssembler* masm, Register obj1, Address obj2) {
  __ cmpptr(obj1, obj2);
}

void BarrierSetAssembler::obj_equals(MacroAssembler* masm, Register obj1, Register obj2) {
  __ cmpptr(obj1, obj2);
}

void BarrierSetAssembler::try_resolve_jobject_in_native(MacroAssembler* masm, Register jni_env, Register obj, Register tmp, Label& slowpath) {
  __ clear_jweak_tag(obj);
  __ movptr(obj, Address(obj, 0));
}

void BarrierSetAssembler::tlab_allocate(MacroAssembler* masm, Register thread, Register obj, Register var_size_in_bytes, int con_size_in_bytes, Register t1, Register t2, Label& slow_case) {
  Register end = t2;
  if (!thread->is_valid()) {
    thread = r15_thread;
  }

  __ movptr(obj, Address(thread, JavaThread::tlab_top_offset()));
  if (var_size_in_bytes == noreg) {
    __ lea(end, Address(obj, con_size_in_bytes));
  } else {
    __ lea(end, Address(obj, var_size_in_bytes, Address::times_1));
  }
  __ cmpptr(end, Address(thread, JavaThread::tlab_end_offset()));
  __ jcc(Assembler::above, slow_case);

  // update the tlab top pointer
  __ movptr(Address(thread, JavaThread::tlab_top_offset()), end);

  // recover var_size_in_bytes if necessary
  if (var_size_in_bytes == end) {
    __ subptr(var_size_in_bytes, obj);
  }
}

// Defines obj, preserves var_size_in_bytes
void BarrierSetAssembler::eden_allocate(MacroAssembler* masm, Register thread, Register obj, Register var_size_in_bytes, int con_size_in_bytes, Register t1, Label& slow_case) {
  if (!Universe::heap()->supports_inline_contig_alloc()) {
    __ jmp(slow_case);
  } else {
    Register end = t1;
    Label retry;
    __ bind(retry);
    ExternalAddress heap_top((address) Universe::heap()->top_addr());
    __ movptr(obj, heap_top);
    if (var_size_in_bytes == noreg) {
      __ lea(end, Address(obj, con_size_in_bytes));
    } else {
      __ lea(end, Address(obj, var_size_in_bytes, Address::times_1));
    }
    // if end < obj then we wrapped around => object too long => slow case
    __ cmpptr(end, obj);
    __ jcc(Assembler::below, slow_case);
    __ cmpptr(end, ExternalAddress((address) Universe::heap()->end_addr()));
    __ jcc(Assembler::above, slow_case);
    // Compare obj with the top addr, and if still equal, store the new top addr in
    // end at the address of the top addr pointer. Sets ZF if was equal, and clears
    // it otherwise. Use lock prefix for atomicity on MPs.
    __ locked_cmpxchgptr(end, heap_top);
    __ jcc(Assembler::notEqual, retry);
    incr_allocated_bytes(masm, thread, var_size_in_bytes, con_size_in_bytes, thread->is_valid() ? noreg : t1);
  }
}

void BarrierSetAssembler::incr_allocated_bytes(MacroAssembler* masm, Register thread, Register var_size_in_bytes, int con_size_in_bytes, Register t1) {
  if (!thread->is_valid()) {
    thread = r15_thread;
  }

  if (var_size_in_bytes->is_valid()) {
    __ addq(Address(thread, in_bytes(JavaThread::allocated_bytes_offset())), var_size_in_bytes);
  } else {
    __ addq(Address(thread, in_bytes(JavaThread::allocated_bytes_offset())), con_size_in_bytes);
  }
}

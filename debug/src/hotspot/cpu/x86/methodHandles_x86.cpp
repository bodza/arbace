#include "precompiled.hpp"

#include "jvm.h"
#include "asm/macroAssembler.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/flags/flagSetting.hpp"
#include "runtime/frame.inline.hpp"
#include "utilities/preserveException.hpp"

#define __ _masm->

#define BLOCK_COMMENT(str) /* nothing */
#define STOP(error) stop(error)

#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")

void MethodHandles::load_klass_from_Class(MacroAssembler* _masm, Register klass_reg) {
  if (VerifyMethodHandles)
    verify_klass(_masm, klass_reg, SystemDictionary::WK_KLASS_ENUM_NAME(java_lang_Class), "MH argument is a Class");
  __ movptr(klass_reg, Address(klass_reg, java_lang_Class::klass_offset_in_bytes()));
}

#define NONZERO(x) (x)

void MethodHandles::jump_from_method_handle(MacroAssembler* _masm, Register method, Register temp, bool for_compiler_entry) {
   Label L_no_such_method;
   __ testptr(rbx, rbx);
   __ jcc(Assembler::zero, L_no_such_method);

  __ verify_method_ptr(method);

  const ByteSize entry_offset = for_compiler_entry ? Method::from_compiled_offset() : Method::from_interpreted_offset();
  __ jmp(Address(method, entry_offset));

  __ bind(L_no_such_method);
  __ jump(RuntimeAddress(StubRoutines::throw_AbstractMethodError_entry()));
}

void MethodHandles::jump_to_lambda_form(MacroAssembler* _masm, Register recv, Register method_temp, Register temp2, bool for_compiler_entry) {
  BLOCK_COMMENT("jump_to_lambda_form {");
  // Load the invoker, as MH -> MH.form -> LF.vmentry
  __ verify_oop(recv);
  __ load_heap_oop(method_temp, Address(recv, NONZERO(java_lang_invoke_MethodHandle::form_offset_in_bytes())), temp2);
  __ verify_oop(method_temp);
  __ load_heap_oop(method_temp, Address(method_temp, NONZERO(java_lang_invoke_LambdaForm::vmentry_offset_in_bytes())), temp2);
  __ verify_oop(method_temp);
  __ load_heap_oop(method_temp, Address(method_temp, NONZERO(java_lang_invoke_MemberName::method_offset_in_bytes())), temp2);
  __ verify_oop(method_temp);
  __ access_load_at(T_ADDRESS, IN_HEAP, method_temp, Address(method_temp, NONZERO(java_lang_invoke_ResolvedMethodName::vmtarget_offset_in_bytes())), noreg, noreg);

  if (VerifyMethodHandles && !for_compiler_entry) {
    // make sure recv is already on stack
    __ movptr(temp2, Address(method_temp, Method::const_offset()));
    __ load_sized_value(temp2, Address(temp2, ConstMethod::size_of_parameters_offset()), sizeof(u2), /*is_signed*/ false);
    Label L;
    __ cmpoop(recv, __ argument_address(temp2, -1));
    __ jcc(Assembler::equal, L);
    __ movptr(rax, __ argument_address(temp2, -1));
    __ STOP("receiver not on stack");
    __ BIND(L);
  }

  jump_from_method_handle(_masm, method_temp, temp2, for_compiler_entry);
  BLOCK_COMMENT("} jump_to_lambda_form");
}

// Code generation
address MethodHandles::generate_method_handle_interpreter_entry(MacroAssembler* _masm, vmIntrinsics::ID iid) {
  const bool not_for_compiler_entry = false;  // this is the interpreter entry
  if (iid == vmIntrinsics::_invokeGeneric || iid == vmIntrinsics::_compiledLambdaForm) {
    // Perhaps surprisingly, the symbolic references visible to Java are not directly used.
    // They are linked to Java-generated adapters via MethodHandleNatives.linkMethod.
    // They all allow an appendix argument.
    __ hlt();           // empty stubs make SG sick
    return NULL;
  }

  // rsi/r13: sender SP (must preserve; see prepare_to_jump_from_interpreted)
  // rbx: Method*
  // rdx: argument locator (parameter slot count, added to rsp)
  // rcx: used as temp to hold mh or receiver
  // rax, rdi: garbage temps, blown away
  Register rdx_argp   = rdx;   // argument list ptr, live on error paths
  Register rax_temp   = rax;
  Register rcx_mh     = rcx;   // MH receiver; dies quickly and is recycled
  Register rbx_method = rbx;   // eventual target of this invocation

  // here's where control starts out:
  __ align(CodeEntryAlignment);
  address entry_point = __ pc();

  if (VerifyMethodHandles) {
    Label L;
    BLOCK_COMMENT("verify_intrinsic_id {");
    __ cmpw(Address(rbx_method, Method::intrinsic_id_offset_in_bytes()), (int) iid);
    __ jcc(Assembler::equal, L);
    if (iid == vmIntrinsics::_linkToVirtual || iid == vmIntrinsics::_linkToSpecial) {
      // could do this for all kinds, but would explode assembly code size
      trace_method_handle(_masm, "bad Method*::intrinsic_id");
    }
    __ STOP("bad Method*::intrinsic_id");
    __ bind(L);
    BLOCK_COMMENT("} verify_intrinsic_id");
  }

  // First task:  Find out how big the argument list is.
  Address rdx_first_arg_addr;
  int ref_kind = signature_polymorphic_intrinsic_ref_kind(iid);
  if (ref_kind == 0 || MethodHandles::ref_kind_has_receiver(ref_kind)) {
    __ movptr(rdx_argp, Address(rbx_method, Method::const_offset()));
    __ load_sized_value(rdx_argp, Address(rdx_argp, ConstMethod::size_of_parameters_offset()), sizeof(u2), /*is_signed*/ false);
    rdx_first_arg_addr = __ argument_address(rdx_argp, -1);
  }

  if (!is_signature_polymorphic_static(iid)) {
    __ movptr(rcx_mh, rdx_first_arg_addr);
  }

  // rdx_first_arg_addr is live!

  trace_method_handle_interpreter_entry(_masm, iid);

  if (iid == vmIntrinsics::_invokeBasic) {
    generate_method_handle_dispatch(_masm, iid, rcx_mh, noreg, not_for_compiler_entry);
  } else {
    // Adjust argument list by popping the trailing MemberName argument.
    Register rcx_recv = noreg;
    if (MethodHandles::ref_kind_has_receiver(ref_kind)) {
      // Load the receiver (not the MH; the actual MemberName's receiver) up from the interpreter stack.
      __ movptr(rcx_recv = rcx, rdx_first_arg_addr);
    }
    Register rbx_member = rbx_method;  // MemberName ptr; incoming method ptr is dead now
    __ pop(rax_temp);           // return address
    __ pop(rbx_member);         // extract last argument
    __ push(rax_temp);          // re-push return address
    generate_method_handle_dispatch(_masm, iid, rcx_recv, rbx_member, not_for_compiler_entry);
  }

  return entry_point;
}

void MethodHandles::generate_method_handle_dispatch(MacroAssembler* _masm, vmIntrinsics::ID iid, Register receiver_reg, Register member_reg, bool for_compiler_entry) {
  Register rbx_method = rbx;   // eventual target of this invocation
  // temps used in this code are not used in *either* compiled or interpreted calling sequences
  Register temp1 = rscratch1;
  Register temp2 = rscratch2;
  Register temp3 = rax;

  if (iid == vmIntrinsics::_invokeBasic) {
    // indirect through MH.form.vmentry.vmtarget
    jump_to_lambda_form(_masm, receiver_reg, rbx_method, temp1, for_compiler_entry);
  } else {
    // The method is a member invoker used by direct method handles.
    if (VerifyMethodHandles) {
      // make sure the trailing argument really is a MemberName (caller responsibility)
      verify_klass(_masm, member_reg, SystemDictionary::WK_KLASS_ENUM_NAME(java_lang_invoke_MemberName), "MemberName required for invokeVirtual etc.");
    }

    Address member_clazz(    member_reg, NONZERO(java_lang_invoke_MemberName::clazz_offset_in_bytes()));
    Address member_vmindex(  member_reg, NONZERO(java_lang_invoke_MemberName::vmindex_offset_in_bytes()));
    Address member_vmtarget( member_reg, NONZERO(java_lang_invoke_MemberName::method_offset_in_bytes()));
    Address vmtarget_method( rbx_method, NONZERO(java_lang_invoke_ResolvedMethodName::vmtarget_offset_in_bytes()));

    Register temp1_recv_klass = temp1;
    if (iid != vmIntrinsics::_linkToStatic) {
      __ verify_oop(receiver_reg);
      if (iid == vmIntrinsics::_linkToSpecial) {
        // Don't actually load the klass; just null-check the receiver.
        __ null_check(receiver_reg);
      } else {
        // load receiver klass itself
        __ null_check(receiver_reg, oopDesc::klass_offset_in_bytes());
        __ load_klass(temp1_recv_klass, receiver_reg);
        __ verify_klass_ptr(temp1_recv_klass);
      }
      BLOCK_COMMENT("check_receiver {");
      // The receiver for the MemberName must be in receiver_reg.
      // Check the receiver against the MemberName.clazz
      if (VerifyMethodHandles && iid == vmIntrinsics::_linkToSpecial) {
        // Did not load it above...
        __ load_klass(temp1_recv_klass, receiver_reg);
        __ verify_klass_ptr(temp1_recv_klass);
      }
      if (VerifyMethodHandles && iid != vmIntrinsics::_linkToInterface) {
        Label L_ok;
        Register temp2_defc = temp2;
        __ load_heap_oop(temp2_defc, member_clazz, temp3);
        load_klass_from_Class(_masm, temp2_defc);
        __ verify_klass_ptr(temp2_defc);
        __ check_klass_subtype(temp1_recv_klass, temp2_defc, temp3, L_ok);
        // If we get here, the type check failed!
        __ STOP("receiver class disagrees with MemberName.clazz");
        __ bind(L_ok);
      }
      BLOCK_COMMENT("} check_receiver");
    }

    // Live registers at this point:
    //  member_reg - MemberName that was the trailing argument
    //  temp1_recv_klass - klass of stacked receiver, if needed
    //  rsi/r13 - interpreter linkage (if interpreted)
    //  rcx, rdx, rsi, rdi, r8 - compiler arguments (if compiled)

    Label L_incompatible_class_change_error;
    switch (iid) {
    case vmIntrinsics::_linkToSpecial:
      if (VerifyMethodHandles) {
        verify_ref_kind(_masm, JVM_REF_invokeSpecial, member_reg, temp3);
      }
      __ load_heap_oop(rbx_method, member_vmtarget);
      __ access_load_at(T_ADDRESS, IN_HEAP, rbx_method, vmtarget_method, noreg, noreg);
      break;

    case vmIntrinsics::_linkToStatic:
      if (VerifyMethodHandles) {
        verify_ref_kind(_masm, JVM_REF_invokeStatic, member_reg, temp3);
      }
      __ load_heap_oop(rbx_method, member_vmtarget);
      __ access_load_at(T_ADDRESS, IN_HEAP, rbx_method, vmtarget_method, noreg, noreg);
      break;

    case vmIntrinsics::_linkToVirtual:
    {
      // same as TemplateTable::invokevirtual,
      // minus the CP setup and profiling:

      if (VerifyMethodHandles) {
        verify_ref_kind(_masm, JVM_REF_invokeVirtual, member_reg, temp3);
      }

      // pick out the vtable index from the MemberName, and then we can discard it:
      Register temp2_index = temp2;
      __ access_load_at(T_ADDRESS, IN_HEAP, temp2_index, member_vmindex, noreg, noreg);

      if (VerifyMethodHandles) {
        Label L_index_ok;
        __ cmpl(temp2_index, 0);
        __ jcc(Assembler::greaterEqual, L_index_ok);
        __ STOP("no virtual index");
        __ BIND(L_index_ok);
      }

      // Note:  The verifier invariants allow us to ignore MemberName.clazz and vmtarget
      // at this point.  And VerifyMethodHandles has already checked clazz, if needed.

      // get target Method* & entry point
      __ lookup_virtual_method(temp1_recv_klass, temp2_index, rbx_method);
      break;
    }

    case vmIntrinsics::_linkToInterface:
    {
      // same as TemplateTable::invokeinterface
      // (minus the CP setup and profiling, with different argument motion)
      if (VerifyMethodHandles) {
        verify_ref_kind(_masm, JVM_REF_invokeInterface, member_reg, temp3);
      }

      BarrierSetAssembler* bs = BarrierSet::barrier_set()->barrier_set_assembler();

      Register temp3_intf = temp3;
      __ load_heap_oop(temp3_intf, member_clazz);
      load_klass_from_Class(_masm, temp3_intf);
      __ verify_klass_ptr(temp3_intf);

      Register rbx_index = rbx_method;
      __ access_load_at(T_ADDRESS, IN_HEAP, rbx_index, member_vmindex, noreg, noreg);
      if (VerifyMethodHandles) {
        Label L;
        __ cmpl(rbx_index, 0);
        __ jcc(Assembler::greaterEqual, L);
        __ STOP("invalid vtable index for MH.invokeInterface");
        __ bind(L);
      }

      // given intf, index, and recv klass, dispatch to the implementation method
      __ lookup_interface_method(temp1_recv_klass, temp3_intf,
                                 // note: next two args must be the same:
                                 rbx_index, rbx_method,
                                 temp2,
                                 L_incompatible_class_change_error);
      break;
    }

    default:
      fatal("unexpected intrinsic %d: %s", iid, vmIntrinsics::name_at(iid));
      break;
    }

    // Live at this point:
    //   rbx_method
    //   rsi/r13 (if interpreted)

    // After figuring out which concrete method to call, jump into it.
    // Note that this works in the interpreter with no data motion.
    // But the compiled version will require that rcx_recv be shifted out.
    __ verify_method_ptr(rbx_method);
    jump_from_method_handle(_masm, rbx_method, temp1, for_compiler_entry);

    if (iid == vmIntrinsics::_linkToInterface) {
      __ bind(L_incompatible_class_change_error);
      __ jump(RuntimeAddress(StubRoutines::throw_IncompatibleClassChangeError_entry()));
    }
  }
}

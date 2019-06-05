#include "precompiled.hpp"

#include "asm/macroAssembler.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "memory/allocation.inline.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/flags/flagSetting.hpp"
#include "runtime/frame.inline.hpp"

#define __ _masm->

#define BLOCK_COMMENT(str) /* nothing */

#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")

void MethodHandles::load_klass_from_Class(MacroAssembler* _masm, Register klass_reg) {
  __ ldr(klass_reg, Address(klass_reg, java_lang_Class::klass_offset_in_bytes()));
}

#define NONZERO(x) (x)

void MethodHandles::jump_from_method_handle(MacroAssembler* _masm, Register method, Register temp, bool for_compiler_entry) {
  Label L_no_such_method;
  __ cbz(rmethod, L_no_such_method);
  __ verify_method_ptr(method);

  const ByteSize entry_offset = for_compiler_entry ? Method::from_compiled_offset() : Method::from_interpreted_offset();
  __ ldr(rscratch1,Address(method, entry_offset));
  __ br(rscratch1);
  __ bind(L_no_such_method);
  __ far_jump(RuntimeAddress(StubRoutines::throw_AbstractMethodError_entry()));
}

void MethodHandles::jump_to_lambda_form(MacroAssembler* _masm, Register recv, Register method_temp, Register temp2, bool for_compiler_entry) {
  BLOCK_COMMENT("jump_to_lambda_form {");
  // This is the initial entry point of a lazy method handle.
  // After type checking, it picks up the invoker from the LambdaForm.

  // Load the invoker, as MH -> MH.form -> LF.vmentry
  __ verify_oop(recv);
  __ load_heap_oop(method_temp, Address(recv, NONZERO(java_lang_invoke_MethodHandle::form_offset_in_bytes())), temp2);
  __ verify_oop(method_temp);
  __ load_heap_oop(method_temp, Address(method_temp, NONZERO(java_lang_invoke_LambdaForm::vmentry_offset_in_bytes())), temp2);
  __ verify_oop(method_temp);
  __ load_heap_oop(method_temp, Address(method_temp, NONZERO(java_lang_invoke_MemberName::method_offset_in_bytes())), temp2);
  __ verify_oop(method_temp);
  __ access_load_at(T_ADDRESS, IN_HEAP, method_temp, Address(method_temp, NONZERO(java_lang_invoke_ResolvedMethodName::vmtarget_offset_in_bytes())), noreg, noreg);

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
    __ hlt(0);           // empty stubs make SG sick
    return NULL;
  }

  // r13: sender SP (must preserve; see prepare_to_jump_from_interpreted)
  // rmethod: Method*
  // r3: argument locator (parameter slot count, added to rsp)
  // r1: used as temp to hold mh or receiver
  // r0, r11: garbage temps, blown away
  Register argp   = r3;   // argument list ptr, live on error paths
  Register temp   = r0;
  Register mh     = r1;   // MH receiver; dies quickly and is recycled

  // here's where control starts out:
  __ align(CodeEntryAlignment);
  address entry_point = __ pc();

  // First task:  Find out how big the argument list is.
  Address r3_first_arg_addr;
  int ref_kind = signature_polymorphic_intrinsic_ref_kind(iid);
  if (ref_kind == 0 || MethodHandles::ref_kind_has_receiver(ref_kind)) {
    __ ldr(argp, Address(rmethod, Method::const_offset()));
    __ load_sized_value(argp, Address(argp, ConstMethod::size_of_parameters_offset()), sizeof(u2), /*is_signed*/ false);
    r3_first_arg_addr = __ argument_address(argp, -1);
  }

  if (!is_signature_polymorphic_static(iid)) {
    __ ldr(mh, r3_first_arg_addr);
  }

  // r3_first_arg_addr is live!

  if (iid == vmIntrinsics::_invokeBasic) {
    generate_method_handle_dispatch(_masm, iid, mh, noreg, not_for_compiler_entry);
  } else {
    // Adjust argument list by popping the trailing MemberName argument.
    Register recv = noreg;
    if (MethodHandles::ref_kind_has_receiver(ref_kind)) {
      // Load the receiver (not the MH; the actual MemberName's receiver) up from the interpreter stack.
      __ ldr(recv = r2, r3_first_arg_addr);
    }
    Register rmember = rmethod;  // MemberName ptr; incoming method ptr is dead now
    __ pop(rmember);             // extract last argument
    generate_method_handle_dispatch(_masm, iid, recv, rmember, not_for_compiler_entry);
  }

  return entry_point;
}

void MethodHandles::generate_method_handle_dispatch(MacroAssembler* _masm, vmIntrinsics::ID iid, Register receiver_reg, Register member_reg, bool for_compiler_entry) {
  // temps used in this code are not used in *either* compiled or interpreted calling sequences
  Register temp1 = r10;
  Register temp2 = r11;
  Register temp3 = r14;  // r13 is live by this point: it contains the sender SP

  if (iid == vmIntrinsics::_invokeBasic) {
    // indirect through MH.form.vmentry.vmtarget
    jump_to_lambda_form(_masm, receiver_reg, rmethod, temp1, for_compiler_entry);
  } else {
    // The method is a member invoker used by direct method handles.

    Address member_clazz(    member_reg, NONZERO(java_lang_invoke_MemberName::clazz_offset_in_bytes()));
    Address member_vmindex(  member_reg, NONZERO(java_lang_invoke_MemberName::vmindex_offset_in_bytes()));
    Address member_vmtarget( member_reg, NONZERO(java_lang_invoke_MemberName::method_offset_in_bytes()));
    Address vmtarget_method( rmethod, NONZERO(java_lang_invoke_ResolvedMethodName::vmtarget_offset_in_bytes()));

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
    }
    if (iid == vmIntrinsics::_linkToSpecial || iid == vmIntrinsics::_linkToStatic) {
    }

    // Live registers at this point:
    //  member_reg - MemberName that was the trailing argument
    //  temp1_recv_klass - klass of stacked receiver, if needed
    //  r13 - interpreter linkage (if interpreted)  ??? FIXME
    //  r1 ... r0 - compiler arguments (if compiled)

    Label L_incompatible_class_change_error;
    switch (iid) {
    case vmIntrinsics::_linkToSpecial:
      __ load_heap_oop(rmethod, member_vmtarget);
      __ access_load_at(T_ADDRESS, IN_HEAP, rmethod, vmtarget_method, noreg, noreg);
      break;

    case vmIntrinsics::_linkToStatic:
      __ load_heap_oop(rmethod, member_vmtarget);
      __ access_load_at(T_ADDRESS, IN_HEAP, rmethod, vmtarget_method, noreg, noreg);
      break;

    case vmIntrinsics::_linkToVirtual:
    {
      // pick out the vtable index from the MemberName, and then we can discard it:
      Register temp2_index = temp2;
      __ access_load_at(T_ADDRESS, IN_HEAP, temp2_index, member_vmindex, noreg, noreg);

      // get target Method* & entry point
      __ lookup_virtual_method(temp1_recv_klass, temp2_index, rmethod);
      break;
    }

    case vmIntrinsics::_linkToInterface:
    {
      Register temp3_intf = temp3;
      __ load_heap_oop(temp3_intf, member_clazz);
      load_klass_from_Class(_masm, temp3_intf);
      __ verify_klass_ptr(temp3_intf);

      Register rindex = rmethod;
      __ access_load_at(T_ADDRESS, IN_HEAP, rindex, member_vmindex, noreg, noreg);

      // given intf, index, and recv klass, dispatch to the implementation method
      __ lookup_interface_method(temp1_recv_klass, temp3_intf,
                                 // note: next two args must be the same:
                                 rindex, rmethod,
                                 temp2,
                                 L_incompatible_class_change_error);
      break;
    }

    default:
      fatal("unexpected intrinsic %d: %s", iid, vmIntrinsics::name_at(iid));
      break;
    }

    // live at this point:  rmethod, r13 (if interpreted)

    // After figuring out which concrete method to call, jump into it.
    // Note that this works in the interpreter with no data motion.
    // But the compiled version will require that r2_recv be shifted out.
    __ verify_method_ptr(rmethod);
    jump_from_method_handle(_masm, rmethod, temp1, for_compiler_entry);
    if (iid == vmIntrinsics::_linkToInterface) {
      __ bind(L_incompatible_class_change_error);
      __ far_jump(RuntimeAddress(StubRoutines::throw_IncompatibleClassChangeError_entry()));
    }
  }
}

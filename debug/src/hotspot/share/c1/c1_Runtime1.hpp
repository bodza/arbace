#ifndef SHARE_VM_C1_C1_RUNTIME1_HPP
#define SHARE_VM_C1_C1_RUNTIME1_HPP

#include "c1/c1_FrameMap.hpp"
#include "code/stubs.hpp"
#include "memory/allocation.hpp"

class StubAssembler;

// The Runtime1 holds all assembly stubs and VM
// runtime routines needed by code code generated
// by the Compiler1.

#define RUNTIME1_STUBS(stub, last_entry) \
  stub(unwind_exception) \
  stub(forward_exception) \
  stub(throw_range_check_failed)       /* throws ArrayIndexOutOfBoundsException */ \
  stub(throw_index_exception)          /* throws IndexOutOfBoundsException */ \
  stub(throw_div0_exception) \
  stub(throw_null_pointer_exception) \
  stub(register_finalizer) \
  stub(new_instance) \
  stub(fast_new_instance) \
  stub(fast_new_instance_init_check) \
  stub(new_type_array) \
  stub(new_object_array) \
  stub(new_multi_array) \
  stub(handle_exception_nofpu)         /* optimized version that does not preserve fpu registers */ \
  stub(handle_exception) \
  stub(handle_exception_from_callee) \
  stub(throw_array_store_exception) \
  stub(throw_class_cast_exception) \
  stub(throw_incompatible_class_change_error) \
  stub(slow_subtype_check) \
  stub(monitorenter) \
  stub(monitorenter_nofpu)             /* optimized version that does not preserve fpu registers */ \
  stub(monitorexit) \
  stub(monitorexit_nofpu)              /* optimized version that does not preserve fpu registers */ \
  stub(deoptimize) \
  stub(access_field_patching) \
  stub(load_klass_patching) \
  stub(load_mirror_patching) \
  stub(load_appendix_patching) \
  stub(fpu2long_stub) \
  stub(counter_overflow) \
  stub(predicate_failed_trap) \
  last_entry(number_of_ids)

#define DECLARE_STUB_ID(x)       x ## _id ,
#define DECLARE_LAST_STUB_ID(x)  x
#define STUB_NAME(x)             #x " Runtime1 stub",
#define LAST_STUB_NAME(x)        #x " Runtime1 stub"

class StubAssemblerCodeGenClosure: public Closure {
 public:
  virtual OopMapSet* generate_code(StubAssembler* sasm) = 0;
};

class Runtime1: public AllStatic {
  friend class VMStructs;
  friend class ArrayCopyStub;

 public:
  enum StubID {
    RUNTIME1_STUBS(DECLARE_STUB_ID, DECLARE_LAST_STUB_ID)
  };

  // statistics

 private:
  static CodeBlob* _blobs[number_of_ids];
  static const char* _blob_names[];

  // stub generation
 public:
  static CodeBlob*  generate_blob(BufferBlob* buffer_blob, int stub_id, const char* name, bool expect_oop_map, StubAssemblerCodeGenClosure *cl);
  static void       generate_blob_for(BufferBlob* blob, StubID id);
  static OopMapSet* generate_code_for(StubID id, StubAssembler* sasm);
 private:
  static OopMapSet* generate_exception_throw(StubAssembler* sasm, address target, bool has_argument);
  static OopMapSet* generate_handle_exception(StubID id, StubAssembler* sasm);
  static void       generate_unwind_exception(StubAssembler *sasm);
  static OopMapSet* generate_patching(StubAssembler* sasm, address target);

  static OopMapSet* generate_stub_call(StubAssembler* sasm, Register result, address entry, Register arg1 = noreg, Register arg2 = noreg, Register arg3 = noreg);

  // runtime entry points
  static void new_instance    (JavaThread* thread, Klass* klass);
  static void new_type_array  (JavaThread* thread, Klass* klass, jint length);
  static void new_object_array(JavaThread* thread, Klass* klass, jint length);
  static void new_multi_array (JavaThread* thread, Klass* klass, int rank, jint* dims);

  static address counter_overflow(JavaThread* thread, int bci, Method* method);

  static void unimplemented_entry   (JavaThread* thread, StubID id);

  static address exception_handler_for_pc(JavaThread* thread);

  static void throw_range_check_exception(JavaThread* thread, int index, arrayOopDesc* a);
  static void throw_index_exception(JavaThread* thread, int index);
  static void throw_div0_exception(JavaThread* thread);
  static void throw_null_pointer_exception(JavaThread* thread);
  static void throw_class_cast_exception(JavaThread* thread, oopDesc* object);
  static void throw_incompatible_class_change_error(JavaThread* thread);
  static void throw_array_store_exception(JavaThread* thread, oopDesc* object);

  static void monitorenter(JavaThread* thread, oopDesc* obj, BasicObjectLock* lock);
  static void monitorexit (JavaThread* thread, BasicObjectLock* lock);

  static void deoptimize(JavaThread* thread, jint trap_request);

  static int access_field_patching(JavaThread* thread);
  static int move_klass_patching(JavaThread* thread);
  static int move_mirror_patching(JavaThread* thread);
  static int move_appendix_patching(JavaThread* thread);

  static void patch_code(JavaThread* thread, StubID stub_id);

 public:
  // initialization
  static void initialize(BufferBlob* blob);
  static void initialize_pd();

  // stubs
  static CodeBlob* blob_for (StubID id);
  static address   entry_for(StubID id)          { return blob_for(id)->code_begin(); }
  static const char* name_for (StubID id);
  static const char* name_for_address(address entry);

  // platform might add runtime names.
  static const char* pd_name_for_address(address entry);

  // method tracing
  static void trace_block_entry(jint block_id);

  // directly accessible leaf routine
  static int  is_instance_of(oopDesc* mirror, oopDesc* obj);

  static void predicate_failed_trap(JavaThread* thread);
};

#endif

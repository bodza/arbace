#include "precompiled.hpp"

#include "c1/c1_Compilation.hpp"
#include "c1/c1_IR.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_LinearScan.hpp"
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_RangeCheckElimination.hpp"
#include "c1/c1_ValueMap.hpp"
#include "c1/c1_ValueStack.hpp"
#include "code/debugInfoRec.hpp"
#include "compiler/compilerDirectives.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/sharedRuntime.hpp"

typedef enum {
  _t_compile,
    _t_setup,
    _t_buildIR,
      _t_hir_parse,
      _t_gvn,
      _t_optimize_blocks,
      _t_optimize_null_checks,
      _t_rangeCheckElimination,
    _t_emit_lir,
      _t_linearScan,
      _t_lirGeneration,
    _t_codeemit,
    _t_codeinstall,
  max_phase_timers
} TimerName;

static const char * timer_name[] = {
  "compile",
  "setup",
  "buildIR",
  "parse_hir",
  "gvn",
  "optimize_blocks",
  "optimize_null_checks",
  "rangeCheckElimination",
  "emit_lir",
  "linearScan",
  "lirGeneration",
  "codeemit",
  "codeinstall"
};

static elapsedTimer timers[max_phase_timers];
static int totalInstructionNodes = 0;

// Implementation of Compilation

DebugInformationRecorder* Compilation::debug_info_recorder() const {
  return _env->debug_info();
}

void Compilation::initialize() {
  // Use an oop recorder bound to the CI environment.
  // (The default oop recorder is ignorant of the CI.)
  OopRecorder* ooprec = new OopRecorder(_env->arena());
  _env->set_oop_recorder(ooprec);
  _env->set_debug_info(new DebugInformationRecorder(ooprec));
  debug_info_recorder()->set_oopmaps(new OopMapSet());
}

void Compilation::build_hir() {
  CHECK_BAILOUT();

  // setup ir
  _hir = new IR(this, method());
  if (!_hir->is_valid()) {
    bailout("invalid parsing");
    return;
  }

  if (UseC1Optimizations) {
    NEEDS_CLEANUP
    // optimization
    _hir->optimize_blocks();
  }

  _hir->split_critical_edges();

  // compute block ordering for code generation
  // the control flow must not be changed from here on
  _hir->compute_code();

  if (RangeCheckElimination) {
    RangeCheckElimination::eliminate(_hir);
  }

  if (UseC1Optimizations) {
    // loop invariant code motion reorders instructions and range
    // check elimination adds new instructions so do null check
    // elimination after.
    NEEDS_CLEANUP
    // optimization
    _hir->eliminate_null_checks();
  }

  // compute use counts after global value numbering
  _hir->compute_use_counts();
}

void Compilation::emit_lir() {
  CHECK_BAILOUT();

  LIRGenerator gen(this, method());

  hir()->iterate_linear_scan_order(&gen);

  CHECK_BAILOUT();

  {
    LinearScan* allocator = new LinearScan(hir(), &gen, frame_map());
    set_allocator(allocator);
    // Assign physical registers to LIR operands using a linear scan algorithm.
    allocator->do_linear_scan();
    CHECK_BAILOUT();

    _max_spills = allocator->max_spills();
  }

  if (BailoutAfterLIR) {
    bailout("Bailing out because of -XX:+BailoutAfterLIR");
  }
}

void Compilation::emit_code_epilog(LIR_Assembler* assembler) {
  CHECK_BAILOUT();

  CodeOffsets* code_offsets = assembler->offsets();

  // generate code or slow cases
  assembler->emit_slow_case_stubs();
  CHECK_BAILOUT();

  // generate exception adapters
  assembler->emit_exception_entries(exception_info_list());
  CHECK_BAILOUT();

  // Generate code for exception handler.
  code_offsets->set_value(CodeOffsets::Exceptions, assembler->emit_exception_handler());
  CHECK_BAILOUT();

  // done
  masm()->flush();
}

bool Compilation::setup_code_buffer(CodeBuffer* code, int call_stub_estimate) {
  // Preinitialize the consts section to some large size:
  int locs_buffer_size = 20 * (relocInfo::length_limit + sizeof(relocInfo));
  char* locs_buffer = NEW_RESOURCE_ARRAY(char, locs_buffer_size);
  code->insts()->initialize_shared_locs((relocInfo*)locs_buffer, locs_buffer_size / sizeof(relocInfo));
  code->initialize_consts_size(Compilation::desired_max_constant_size());
  // Call stubs + two deopt handlers (regular and MH) + exception handler
  int stub_size = (call_stub_estimate * LIR_Assembler::call_stub_size()) + LIR_Assembler::exception_handler_size();
  if (stub_size >= code->insts_capacity()) return false;
  code->initialize_stubs_size(stub_size);
  return true;
}

int Compilation::emit_code_body() {
  // emit code
  if (!setup_code_buffer(code(), allocator()->num_calls())) {
    BAILOUT_("size requested greater than avail code buffer size", 0);
  }
  code()->initialize_oop_recorder(env()->oop_recorder());

  _masm = new C1_MacroAssembler(code());
  _masm->set_oop_recorder(env()->oop_recorder());

  LIR_Assembler lir_asm(this);

  lir_asm.emit_code(hir()->code());
  CHECK_BAILOUT_(0);

  emit_code_epilog(&lir_asm);
  CHECK_BAILOUT_(0);

  generate_exception_handler_table();

  return frame_map()->framesize();
}

int Compilation::compile_java_method() {
  if (BailoutOnExceptionHandlers) {
    if (method()->has_exception_handlers()) {
      bailout("linear scan can't handle exception handlers");
    }
  }

  CHECK_BAILOUT_(no_frame_size);

  if (is_profiling() && !method()->ensure_method_data()) {
    BAILOUT_("mdo allocation failed", no_frame_size);
  }

  build_hir();
  if (BailoutAfterHIR) {
    BAILOUT_("Bailing out because of -XX:+BailoutAfterHIR", no_frame_size);
  }

  {
    _frame_map = new FrameMap(method(), hir()->number_of_locks(), MAX2(4, hir()->max_stack()));
    emit_lir();
  }
  CHECK_BAILOUT_(no_frame_size);

  return emit_code_body();
}

void Compilation::install_code(int frame_size) {
  // frame_size is in 32-bit words so adjust it intptr_t words
  _env->register_method(
    method(),
    -1,
    &_offsets,
    in_bytes(_frame_map->sp_offset_for_orig_pc()),
    code(),
    in_bytes(frame_map()->framesize_in_bytes()) / sizeof(intptr_t),
    debug_info_recorder()->_oopmaps,
    exception_handler_table(),
    implicit_exception_table(),
    compiler(),
    has_unsafe_access(),
    SharedRuntime::is_wide_vector(max_vector_size())
  );
}

void Compilation::compile_method() {
  initialize();

  if (!method()->can_be_compiled()) {
    // Prevent race condition 6328518.
    // This can happen if the method is obsolete or breakpointed.
    bailout("Bailing out because method is not compilable");
    return;
  }

  if (directive()->BreakAtCompileOption) {
    BREAKPOINT;
  }

  int frame_size = compile_java_method();

  // bailout if method couldn't be compiled
  // Note: make sure we mark the method as not compilable!
  CHECK_BAILOUT();

  if (InstallMethods) {
    install_code(frame_size);
  }

  totalInstructionNodes += Instruction::number_of_instructions();
}

void Compilation::generate_exception_handler_table() {
  // Generate an ExceptionHandlerTable from the exception handler
  // information accumulated during the compilation.
  ExceptionInfoList* info_list = exception_info_list();

  if (info_list->length() == 0) {
    return;
  }

  // allocate some arrays for use by the collection code.
  const int num_handlers = 5;
  GrowableArray<intptr_t>* bcis = new GrowableArray<intptr_t>(num_handlers);
  GrowableArray<intptr_t>* scope_depths = new GrowableArray<intptr_t>(num_handlers);
  GrowableArray<intptr_t>* pcos = new GrowableArray<intptr_t>(num_handlers);

  for (int i = 0; i < info_list->length(); i++) {
    ExceptionInfo* info = info_list->at(i);
    XHandlers* handlers = info->exception_handlers();

    // empty the arrays
    bcis->trunc_to(0);
    scope_depths->trunc_to(0);
    pcos->trunc_to(0);

    int prev_scope = 0;
    for (int i = 0; i < handlers->length(); i++) {
      XHandler* handler = handlers->handler_at(i);

      if (handler->scope_count() == prev_scope) {
        int e = bcis->find_from_end(handler->handler_bci());
        if (e >= 0 && scope_depths->at(e) == handler->scope_count()) {
          // two different handlers are declared to dispatch to the same
          // catch bci.  During parsing we created edges for each
          // handler but we really only need one.  The exception handler
          // table will also get unhappy if we try to declare both since
          // it's nonsensical.  Just skip this handler.
          continue;
        }
      }

      bcis->append(handler->handler_bci());
      if (handler->handler_bci() == -1) {
        // insert a wildcard handler at scope depth 0 so that the
        // exception lookup logic with find it.
        scope_depths->append(0);
      } else {
        scope_depths->append(handler->scope_count());
      }
      pcos->append(handler->entry_pco());

      // stop processing once we hit a catch any
      if (handler->is_catch_all()) {
      }
      prev_scope = handler->scope_count();
    }
    exception_handler_table()->add_subtable(info->pco(), bcis, scope_depths, pcos);
  }
}

Compilation::Compilation(AbstractCompiler* compiler, ciEnv* env, ciMethod* method, BufferBlob* buffer_blob, DirectiveSet* directive)
: _compiler(compiler)
, _env(env)
, _directive(directive)
, _method(method)
, _hir(NULL)
, _max_spills(-1)
, _frame_map(NULL)
, _masm(NULL)
, _has_exception_handlers(false)
, _has_fpu_code(true)   // pessimistic assumption
, _would_profile(false)
, _has_unsafe_access(false)
, _has_method_handle_invokes(false)
, _has_reserved_stack_access(method->has_reserved_stack_access())
, _bailout_msg(NULL)
, _exception_info_list(NULL)
, _allocator(NULL)
, _next_id(0)
, _next_block_id(0)
, _code(buffer_blob)
, _has_access_indexed(false)
, _current_instruction(NULL)
{
  _arena = Thread::current()->resource_area();
  _env->set_compiler_data(this);
  _exception_info_list = new ExceptionInfoList();
  _implicit_exception_table.set_size(0);
  compile_method();
  if (bailed_out()) {
    _env->record_method_not_compilable(bailout_msg());
    if (is_profiling()) {
      // Compilation failed, create MDO, which would signal the interpreter
      // to start profiling on its own.
      _method->ensure_method_data();
    }
  } else if (is_profiling()) {
    ciMethodData *md = method->method_data_or_null();
    if (md != NULL) {
      md->set_would_profile(_would_profile);
    }
  }
}

Compilation::~Compilation() {
  _env->set_compiler_data(NULL);
}

void Compilation::add_exception_handlers_for_pco(int pco, XHandlers* exception_handlers) {
  // Note: we do not have program counters for these exception handlers yet
  exception_info_list()->push(new ExceptionInfo(pco, exception_handlers));
}

void Compilation::notice_inlined_method(ciMethod* method) {
  _env->notice_inlined_method(method);
}

void Compilation::bailout(const char* msg) {
  if (!bailed_out()) {
    // keep first bailout message
    _bailout_msg = msg;
  }
}

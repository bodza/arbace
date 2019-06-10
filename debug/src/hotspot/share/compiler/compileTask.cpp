#include "precompiled.hpp"

#include "compiler/compileTask.hpp"
#include "compiler/compileBroker.hpp"
#include "compiler/compilerDirectives.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/handles.inline.hpp"

CompileTask*  CompileTask::_task_free_list = NULL;

/**
 * Allocate a CompileTask, from the free list if possible.
 */
CompileTask* CompileTask::allocate() {
  MutexLocker locker(CompileTaskAlloc_lock);
  CompileTask* task = NULL;

  if (_task_free_list != NULL) {
    task = _task_free_list;
    _task_free_list = task->next();
    task->set_next(NULL);
  } else {
    task = new CompileTask();
    task->set_next(NULL);
    task->set_is_free(true);
  }
  task->set_is_free(false);
  return task;
}

/**
 * Add a task to the free list.
 */

void CompileTask::free(CompileTask* task) {
 MutexLocker locker(CompileTaskAlloc_lock);
 if (!task->is_free()) {
   task->set_code(NULL);
   JNIHandles::destroy_global(task->_method_holder);
   JNIHandles::destroy_global(task->_hot_method_holder);

   task->set_is_free(true);
   task->set_next(_task_free_list);
   _task_free_list = task;
 }
}

void CompileTask::initialize(int compile_id, const methodHandle& method, int comp_level, const methodHandle& hot_method, int hot_count, CompileTask::CompileReason compile_reason, bool is_blocking) {
  Thread* thread = Thread::current();
  _compile_id = compile_id;
  _method = method();
  _method_holder = JNIHandles::make_global(Handle(thread, method->method_holder()->klass_holder()));
  _is_blocking = is_blocking;
  _has_waiter = CompileBroker::compiler(comp_level)->is_jvmci();
  _jvmci_compiler_thread = NULL;
  _comp_level = comp_level;
  _num_inlined_bytecodes = 0;

  _is_complete = false;
  _is_success = false;
  _code_handle = NULL;

  _hot_method = NULL;
  _hot_method_holder = NULL;
  _hot_count = hot_count;
  _time_queued = 0;  // tidy
  _compile_reason = compile_reason;
  _failure_reason = NULL;

  _next = NULL;
}

/**
 * Returns the compiler for this task.
 */
AbstractCompiler* CompileTask::compiler() {
  return CompileBroker::compiler(_comp_level);
}

nmethod* CompileTask::code() const {
  if (_code_handle == NULL)  return NULL;
  CodeBlob *blob = _code_handle->code();
  if (blob != NULL) {
    return blob->as_nmethod();
  }
  return NULL;
}

void CompileTask::set_code(nmethod* nm) {
  if (_code_handle == NULL && nm == NULL)  return;
  guarantee(_code_handle != NULL, "");
  _code_handle->set_code(nm);
  if (nm == NULL)  _code_handle = NULL;  // drop the handle also
}

void CompileTask::mark_on_stack() {
  // Mark these methods as something redefine classes cannot remove.
  _method->set_on_stack(true);
  if (_hot_method != NULL) {
    _hot_method->set_on_stack(true);
  }
}

// RedefineClasses support
void CompileTask::metadata_do(void f(Metadata*)) {
  f(method());
  if (hot_method() != NULL && hot_method() != method()) {
    f(hot_method());
  }
}

// ------------------------------------------------------------------
// CompileTask::print_line_on_error
//
// This function is called by fatal error handler when the thread
// causing troubles is a compiler thread.
//
// Do not grab any lock, do not allocate memory.
//
// Otherwise it's the same as CompileTask::print_line()
//
void CompileTask::print_line_on_error(outputStream* st, char* buf, int buflen) {
  // print compiler name
  st->print("%s:", CompileBroker::compiler_name(comp_level()));
  print(st);
}

void CompileTask::print_tty() {
  ttyLocker ttyl;  // keep the following output all in one block
  print(tty);
}

void CompileTask::print_impl(outputStream* st, Method* method, int compile_id, int comp_level, bool is_blocking, const char* msg, bool short_form, bool cr) {
  if (!short_form) {
    st->print("%7d ", (int) st->time_stamp().milliseconds());  // print timestamp
  }
  st->print("%4d ", compile_id);    // print compilation number

  // For unloaded methods the transition to zombie occurs after the
  // method is cleared so it's impossible to report accurate
  // information for that case.
  bool is_synchronized = false;
  bool has_exception_handler = false;
  bool is_native = false;
  if (method != NULL) {
    is_synchronized       = method->is_synchronized();
    has_exception_handler = method->has_exception_handler();
    is_native             = method->is_native();
  }
  // method attributes
  const char sync_char      = is_synchronized                 ? 's' : ' ';
  const char exception_char = has_exception_handler           ? '!' : ' ';
  const char blocking_char  = is_blocking                     ? 'b' : ' ';
  const char native_char    = is_native                       ? 'n' : ' ';

  // print method attributes
  st->print(" %c%c%c%c ", sync_char, exception_char, blocking_char, native_char);
  st->print("     ");  // more indent

  if (method == NULL) {
    st->print("(method)");
  } else {
    method->print_short_name(st);
    if (method->is_native())
      st->print(" (native)");
    else
      st->print(" (%d bytes)", method->code_size());
  }

  if (msg != NULL) {
    st->print("   %s", msg);
  }
  if (cr) {
    st->cr();
  }
}

void CompileTask::print_inline_indent(int inline_level, outputStream* st) {
  //         1234567
  st->print("        ");     // print timestamp
  //         1234
  st->print("     ");        // print compilation number
  //         %s!bn
  st->print("      ");       // print method attributes
  st->print("     ");        // more indent
  st->print("    ");         // initial inlining indent
  for (int i = 0; i < inline_level; i++)
    st->print("  ");
}

void CompileTask::print(outputStream* st, const char* msg, bool short_form, bool cr) {
  print_impl(st, method(), compile_id(), comp_level(), is_blocking(), msg, short_form, cr);
}

void CompileTask::log_task(xmlStream* log) {
  Thread* thread = Thread::current();
  methodHandle method(thread, this->method());
  ResourceMark rm(thread);

  // <task id='9' method='M' level='1' blocking='1' stamp='1.234'>
  log->print(" compile_id='%d'", _compile_id);
  if (!method.is_null()) {
    log->method(method);
  }
  if (_comp_level != CompLevel_highest_tier) {
    log->print(" level='%d'", _comp_level);
  }
  if (_is_blocking) {
    log->print(" blocking='1'");
  }
  log->stamp();
}

void CompileTask::log_task_queued() {
  Thread* thread = Thread::current();
  ttyLocker ttyl;
  ResourceMark rm(thread);

  xtty->begin_elem("task_queued");
  log_task(xtty);
  xtty->print(" comment='%s'", reason_name(_compile_reason));

  if (_hot_method != NULL) {
    methodHandle hot(thread, _hot_method);
    methodHandle method(thread, _method);
    if (hot() != method()) {
      xtty->method(hot);
    }
  }
  if (_hot_count != 0) {
    xtty->print(" hot_count='%d'", _hot_count);
  }
  xtty->end_elem();
}

bool CompileTask::check_break_at_flags() {
  int compile_id = this->_compile_id;

  return (compile_id == CIBreakAt);
}

void CompileTask::print_inlining_inner(outputStream* st, ciMethod* method, int inline_level, int bci, const char* msg) {
  //         1234567
  st->print("        ");     // print timestamp
  //         1234
  st->print("     ");        // print compilation number

  // method attributes
  if (method->is_loaded()) {
    const char sync_char      = method->is_synchronized()        ? 's' : ' ';
    const char exception_char = method->has_exception_handlers() ? '!' : ' ';
    const char monitors_char  = method->has_monitor_bytecodes()  ? 'm' : ' ';

    // print method attributes
    st->print(" %c%c%c  ", sync_char, exception_char, monitors_char);
  } else {
    //         %s!bn
    st->print("      ");     // print method attributes
  }

  st->print("     ");        // more indent
  st->print("    ");         // initial inlining indent

  for (int i = 0; i < inline_level; i++)
    st->print("  ");

  st->print("@ %d  ", bci);  // print bci
  method->print_short_name(st);
  if (method->is_loaded())
    st->print(" (%d bytes)", method->code_size());
  else
    st->print(" (not loaded)");

  if (msg != NULL) {
    st->print("   %s", msg);
  }
  st->cr();
}

void CompileTask::print_ul(const char* msg) { }
void CompileTask::print_ul(const nmethod* nm, const char* msg) { }
void CompileTask::print_inlining_ul(ciMethod* method, int inline_level, int bci, const char* msg) { }

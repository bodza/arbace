#ifndef SHARE_VM_COMPILER_COMPILEBROKER_HPP
#define SHARE_VM_COMPILER_COMPILEBROKER_HPP

#include "ci/compilerInterface.hpp"
#include "compiler/abstractCompiler.hpp"
#include "compiler/compileTask.hpp"
#include "compiler/compilerDirectives.hpp"
#include "utilities/stack.hpp"
#include "jvmci/jvmciCompiler.hpp"

class nmethod;
class nmethodLocker;

// CompilerCounters
//
// Per Compiler Performance Counters.
//
class CompilerCounters : public CHeapObj<mtCompiler> {
  public:
    enum {
      cmname_buffer_length = 160
    };

  private:
    char _current_method[cmname_buffer_length];
    int  _compile_type;

  public:
    CompilerCounters();

    // these methods should be called in a thread safe context

    void set_current_method(const char* method) {
      strncpy(_current_method, method, (size_t)cmname_buffer_length - 1);
      _current_method[cmname_buffer_length - 1] = '\0';
    }

    char* current_method()                  { return _current_method; }

    void set_compile_type(int compile_type) {
      _compile_type = compile_type;
    }

    int compile_type()                       { return _compile_type; }
};

// CompileQueue
//
// A list of CompileTasks.
class CompileQueue : public CHeapObj<mtCompiler> {
 private:
  const char* _name;

  CompileTask* _first;
  CompileTask* _last;

  CompileTask* _first_stale;

  int _size;

  void purge_stale_tasks();
 public:
  CompileQueue(const char* name) {
    _name = name;
    _first = NULL;
    _last = NULL;
    _size = 0;
    _first_stale = NULL;
  }

  const char*  name()                      const { return _name; }

  void         add(CompileTask* task);
  void         remove(CompileTask* task);
  void         remove_and_mark_stale(CompileTask* task);
  CompileTask* first()                           { return _first; }
  CompileTask* last()                            { return _last; }

  CompileTask* get();

  bool         is_empty()                  const { return _first == NULL; }
  int          size()                      const { return _size; }

  // Redefine Classes support
  void mark_on_stack();
  void free_all();
  void print_tty();
  void print(outputStream* st = tty);

  ~CompileQueue() { }
};

// CompileTaskWrapper
//
// Assign this task to the current thread.  Deallocate the task
// when the compilation is complete.
class CompileTaskWrapper : StackObj {
public:
  CompileTaskWrapper(CompileTask* task);
  ~CompileTaskWrapper();
};

// Compilation
//
// The broker for all compilation requests.
class CompileBroker: AllStatic {
 friend class Threads;
 friend class CompileTaskWrapper;

 public:
  enum {
    name_buffer_length = 100
  };

  // Compile type Information for CompilerCounters
  enum { no_compile, normal_compile, native_compile };
  static int assign_compile_id(const methodHandle& method);

 private:
  static bool _initialized;
  static volatile bool _should_block;

  // This flag can be used to stop compilation or turn it back on
  static volatile jint _should_compile_new_jobs;

  // The installed compiler(s)
  static AbstractCompiler* _compilers[2];

  // The maximum numbers of compiler threads to be determined during startup.
  static int _c1_count, _c2_count;

  // An array of compiler thread Java objects
  static jobject *_compiler1_objects, *_compiler2_objects;

  // These counters are used for assigning id's to each compilation
  static volatile jint _compilation_id;

  static CompileQueue* _c2_compile_queue;
  static CompileQueue* _c1_compile_queue;

  // Timers and counters for generating statistics
  static elapsedTimer _t_total_compilation;
  static elapsedTimer _t_standard_compilation;
  static elapsedTimer _t_invalidated_compilation;
  static elapsedTimer _t_bailedout_compilation;

  static int _total_compile_count;
  static int _total_bailout_count;
  static int _total_invalidated_count;
  static int _total_native_compile_count;
  static int _total_standard_compile_count;
  static int _total_compiler_stopped_count;
  static int _total_compiler_restarted_count;
  static int _sum_standard_bytes_compiled;
  static int _sum_nmethod_size;
  static int _sum_nmethod_code_size;
  static long _peak_compilation_time;

  static volatile int _print_compilation_warning;

  static Handle create_thread_oop(const char* name, TRAPS);
  static JavaThread* make_thread(jobject thread_oop, CompileQueue* queue, AbstractCompiler* comp, bool compiler_thread, TRAPS);
  static void init_compiler_sweeper_threads();
  static void possibly_add_compiler_threads();
  static bool compilation_is_complete  (const methodHandle& method, int comp_level);
  static bool compilation_is_prohibited(const methodHandle& method, int comp_level, bool excluded);

  static CompileTask* create_compile_task(CompileQueue* queue,
                                          int compile_id,
                                          const methodHandle& method,
                                          int comp_level,
                                          const methodHandle& hot_method,
                                          int hot_count,
                                          CompileTask::CompileReason compile_reason,
                                          bool blocking);
  static void wait_for_completion(CompileTask* task);
  static bool wait_for_jvmci_completion(JVMCICompiler* comp, CompileTask* task, JavaThread* thread);

  static void invoke_compiler_on_method(CompileTask* task);
  static void post_compile(CompilerThread* thread, CompileTask* task, bool success, ciEnv* ci_env, int compilable, const char* failure_reason);
  static void push_jni_handle_block();
  static void pop_jni_handle_block();
  static void collect_statistics(CompilerThread* thread, elapsedTimer time, CompileTask* task);

  static void compile_method_base(const methodHandle& method,
                                  int comp_level,
                                  const methodHandle& hot_method,
                                  int hot_count,
                                  CompileTask::CompileReason compile_reason,
                                  bool blocking,
                                  Thread* thread);

  static CompileQueue* compile_queue(int comp_level);
  static bool init_compiler_runtime();
  static void shutdown_compiler_runtime(AbstractCompiler* comp, CompilerThread* thread);

public:
  static DirectivesStack* dirstack();
  static void set_dirstack(DirectivesStack* stack);

  static AbstractCompiler* compiler(int comp_level) {
    if (is_c2_compile(comp_level)) return _compilers[1]; // C2
    if (is_c1_compile(comp_level)) return _compilers[0]; // C1
    return NULL;
  }

  static bool compilation_is_in_queue(const methodHandle& method);
  static void print_compile_queues(outputStream* st);
  static void print_directives(outputStream* st);
  static int queue_size(int comp_level) {
    CompileQueue *q = compile_queue(comp_level);
    return q != NULL ? q->size() : 0;
  }
  static void compilation_init_phase1(TRAPS);
  static void compilation_init_phase2();
  static nmethod* compile_method(const methodHandle& method,
                                 int comp_level,
                                 const methodHandle& hot_method,
                                 int hot_count,
                                 CompileTask::CompileReason compile_reason,
                                 Thread* thread);

  static nmethod* compile_method(const methodHandle& method,
                                 int comp_level,
                                 const methodHandle& hot_method,
                                 int hot_count,
                                 CompileTask::CompileReason compile_reason,
                                 DirectiveSet* directive,
                                 Thread* thread);

  // Acquire any needed locks and assign a compile id
  static uint assign_compile_id_unlocked(Thread* thread, const methodHandle& method);

  static void compiler_thread_loop();
  static uint get_compilation_id() { return _compilation_id; }

  // Set _should_block.
  // Call this from the VM, with Threads_lock held and a safepoint requested.
  static void set_should_block();

  // Call this from the compiler at convenient points, to poll for _should_block.
  static void maybe_block();

  enum CompilerActivity {
    // Flags for toggling compiler activity
    stop_compilation     = 0,
    run_compilation      = 1,
    shutdown_compilation = 2
  };

  static jint get_compilation_activity_mode() { return _should_compile_new_jobs; }
  static bool should_compile_new_jobs() { return UseCompiler && (_should_compile_new_jobs == run_compilation); }
  static bool set_should_compile_new_jobs(jint new_state) {
    // Return success if the current caller set it
    jint old = Atomic::cmpxchg(new_state, &_should_compile_new_jobs, 1-new_state);
    bool success = (old == (1-new_state));
    if (success) {
      if (new_state == run_compilation) {
        _total_compiler_restarted_count++;
      } else {
        _total_compiler_stopped_count++;
      }
    }
    return success;
  }

  static void disable_compilation_forever() {
    UseCompiler               = false;
    AlwaysCompileLoopMethods  = false;
    Atomic::xchg(jint(shutdown_compilation), &_should_compile_new_jobs);
  }

  static bool is_compilation_disabled_forever() {
    return _should_compile_new_jobs == shutdown_compilation;
  }

  static void handle_full_code_cache(int code_blob_type);

  // Ensures that warning is only printed once.
  static bool should_print_compiler_warning() {
    jint old = Atomic::cmpxchg(1, &_print_compilation_warning, 0);
    return old == 0;
  }

  // Redefine Classes support
  static void mark_on_stack();

  // compiler name for debugging
  static const char* compiler_name(int comp_level);

  // Provide access to compiler thread Java objects
  static jobject compiler1_object(int idx)        { return _compiler1_objects[idx]; }
  static jobject compiler2_object(int idx)        { return _compiler2_objects[idx]; }

  static int get_total_compile_count()            { return _total_compile_count; }
  static int get_total_bailout_count()            { return _total_bailout_count; }
  static int get_total_invalidated_count()        { return _total_invalidated_count; }
  static int get_total_native_compile_count()     { return _total_native_compile_count; }
  static int get_total_standard_compile_count()   { return _total_standard_compile_count; }
  static int get_total_compiler_stopped_count()   { return _total_compiler_stopped_count; }
  static int get_total_compiler_restarted_count() { return _total_compiler_restarted_count; }
  static int get_sum_standard_bytes_compiled()    { return _sum_standard_bytes_compiled; }
  static int get_sum_nmethod_size()               { return _sum_nmethod_size; }
  static int get_sum_nmethod_code_size()          { return _sum_nmethod_code_size; }
  static long get_peak_compilation_time()         { return _peak_compilation_time; }
  static long get_total_compilation_time()        { return _t_total_compilation.milliseconds(); }

  // CodeHeap State Analytics.
  static void print_info(outputStream* out);
  static void print_heapinfo(outputStream* out, const char* function, const char* granularity );
};

#endif

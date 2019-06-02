#ifndef SHARE_VM_RUNTIME_VFRAMEARRAY_HPP
#define SHARE_VM_RUNTIME_VFRAMEARRAY_HPP

#include "memory/allocation.hpp"
#include "oops/arrayOop.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/frame.hpp"
#include "runtime/monitorChunk.hpp"
#include "utilities/growableArray.hpp"

// A vframeArray is an array used for momentarily storing off stack Java method activations
// during deoptimization. Essentially it is an array of vframes where each vframe
// data is stored off stack. This structure will never exist across a safepoint so
// there is no need to gc any oops that are stored in the structure.

class LocalsClosure;
class ExpressionStackClosure;
class MonitorStackClosure;
class MonitorArrayElement;
class StackValueCollection;

// A vframeArrayElement is an element of a vframeArray. Each element
// represent an interpreter frame which will eventually be created.

class vframeArrayElement {
  friend class VMStructs;

  private:

    frame _frame;                                                // the interpreter frame we will unpack into
    int  _bci;                                                   // raw bci for this vframe
    bool _reexecute;                                             // whether we should reexecute this bytecode
    Method*    _method;                                          // the method for this vframe
    MonitorChunk* _monitors;                                     // active monitors for this vframe
    StackValueCollection* _locals;
    StackValueCollection* _expressions;

  public:

  frame* iframe(void)                { return &_frame; }

  int bci(void) const;

  int raw_bci(void) const            { return _bci; }
  bool should_reexecute(void) const  { return _reexecute; }

  Method* method(void) const       { return _method; }

  MonitorChunk* monitors(void) const { return _monitors; }

  void free_monitors(JavaThread* jt);

  StackValueCollection* locals(void) const             { return _locals; }

  StackValueCollection* expressions(void) const        { return _expressions; }

  void fill_in(compiledVFrame* vf, bool realloc_failures);

  // Formerly part of deoptimizedVFrame

  // Returns the on stack word size for this frame
  // callee_parameters is the number of callee locals residing inside this frame
  int on_stack_size(int callee_parameters,
                    int callee_locals,
                    bool is_top_frame,
                    int popframe_extra_stack_expression_els) const;

  // Unpacks the element to skeletal interpreter frame
  void unpack_on_stack(int caller_actual_parameters,
                       int callee_parameters,
                       int callee_locals,
                       frame* caller,
                       bool is_top_frame,
                       bool is_bottom_frame,
                       int exec_mode);
};

// this can be a ResourceObj if we don't save the last one...
// but it does make debugging easier even if we can't look
// at the data in each vframeElement

class vframeArray: public CHeapObj<mtCompiler> {
  friend class VMStructs;

 private:

  // Here is what a vframeArray looks like in memory

  /*
      fixed part
        description of the original frame
        _frames - number of vframes in this array
        adapter info
        callee register save area
      variable part
        vframeArrayElement   [ 0 ]
        ...
        vframeArrayElement   [_frames - 1]

  */

  JavaThread*                  _owner_thread;
  vframeArray*                 _next;
  frame                        _original;          // the original frame of the deoptee
  frame                        _caller;            // caller of root frame in vframeArray
  frame                        _sender;

  Deoptimization::UnrollBlock* _unroll_block;
  int                          _frame_size;

  int                          _frames; // number of javavframes in the array (does not count any adapter)

  intptr_t                     _callee_registers[RegisterMap::reg_count];
  unsigned char                _valid[RegisterMap::reg_count];

  vframeArrayElement           _elements[1];   // First variable section.

  void fill_in_element(int index, compiledVFrame* vf);

  bool is_location_valid(int i) const        { return _valid[i] != 0; }
  void set_location_valid(int i, bool valid) { _valid[i] = valid; }

 public:

  // Tells whether index is within bounds.
  bool is_within_bounds(int index) const        { return 0 <= index && index < frames(); }

  // Accessories for instance variable
  int frames() const                            { return _frames; }

  static vframeArray* allocate(JavaThread* thread, int frame_size, GrowableArray<compiledVFrame*>* chunk, RegisterMap* reg_map, frame sender, frame caller, frame self, bool realloc_failures);

  vframeArrayElement* element(int index)        { return &_elements[index]; }

  // Allocates a new vframe in the array and fills the array with vframe information in chunk
  void fill_in(JavaThread* thread, int frame_size, GrowableArray<compiledVFrame*>* chunk, const RegisterMap *reg_map, bool realloc_failures);

  // Returns the owner of this vframeArray
  JavaThread* owner_thread() const           { return _owner_thread; }

  // Accessors for next
  vframeArray* next() const                  { return _next; }
  void set_next(vframeArray* value)          { _next = value; }

  // Accessors for sp
  intptr_t* sp() const                       { return _original.sp(); }

  intptr_t* unextended_sp() const;

  address original_pc() const                { return _original.pc(); }

  frame original() const                     { return _original; }

  frame caller() const                       { return _caller; }

  frame sender() const                       { return _sender; }

  // Accessors for unroll block
  Deoptimization::UnrollBlock* unroll_block() const         { return _unroll_block; }
  void set_unroll_block(Deoptimization::UnrollBlock* block) { _unroll_block = block; }

  // Returns the size of the frame that got deoptimized
  int frame_size() const { return _frame_size; }

  // Unpack the array on the stack passed in stack interval
  void unpack_to_stack(frame &unpack_frame, int exec_mode, int caller_actual_parameters);

  // Deallocates monitor chunks allocated during deoptimization.
  // This should be called when the array is not used anymore.
  void deallocate_monitor_chunks();

  // Accessor for register map
  address register_location(int i) const;

  void print_on_2(outputStream* st) { };
  void print_value_on(outputStream* st) const { };
};

#endif

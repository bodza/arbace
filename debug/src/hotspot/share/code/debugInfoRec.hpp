#ifndef SHARE_VM_CODE_DEBUGINFOREC_HPP
#define SHARE_VM_CODE_DEBUGINFOREC_HPP

#include "ci/ciClassList.hpp"
#include "ci/ciInstanceKlass.hpp"
#include "ci/ciMethod.hpp"
#include "code/debugInfo.hpp"
#include "code/location.hpp"
#include "code/pcDesc.hpp"
#include "compiler/oopMap.hpp"
#include "oops/oop.hpp"
#include "utilities/growableArray.hpp"

//** The DebugInformationRecorder collects debugging information
//   for a compiled method.
//   Debugging information is used for:
//   - garbage collecting compiled frames
//   - stack tracing across compiled frames
//   - deoptimizating compiled frames
//
//   The implementation requires the compiler to use the recorder
//   in the following order:
//   1) Describe debug information for safepoints at increasing addresses.
//      a) Add safepoint entry (use add_safepoint or add_non_safepoint)
//      b) Describe scopes for that safepoint
//         - create locals if needed (use create_scope_values)
//         - create expressions if needed (use create_scope_values)
//         - create monitor stack if needed (use create_monitor_values)
//         - describe scope (use describe_scope)
//         "repeat last four steps for all scopes"
//         "outer most scope first and inner most scope last"
//         NB: nodes from create_scope_values and create_locations
//             can be reused for simple sharing.
//         - mark the end of the scopes (end_safepoint or end_non_safepoint)
//   2) Use oop_size, metadata_size, data_size, pcs_size to create the nmethod
//      and finally migrate the debugging information into the nmethod
//      by calling copy_to.

class DebugToken; // Opaque datatype for stored:
                  //  - GrowableArray<ScopeValue*>
                  //  - GrowableArray<MonitorValue*>

// Alias for InvocationEntryBci.
// Both constants are used for a pseudo-BCI which refers
// to the state just _before_ a method is entered.
// SynchronizationEntryBCI is used where the emphasis
// is on the implicit monitorenter of a synchronized method.
const int SynchronizationEntryBCI = InvocationEntryBci;

class DIR_Chunk; // private class, a nugget of collected information

class DebugInformationRecorder: public ResourceObj {
 public:
  // constructor
  DebugInformationRecorder(OopRecorder* oop_recorder);

  // adds an oopmap at a specific offset
  void add_oopmap(int pc_offset, OopMap* map);

  // adds a jvm mapping at pc-offset, for a safepoint only
  void add_safepoint(int pc_offset, OopMap* map);

  // adds a jvm mapping at pc-offset, for a non-safepoint (profile point)
  void add_non_safepoint(int pc_offset);

  // Describes debugging information for a scope at the given pc_offset.
  // Calls must be in non-decreasing order of pc_offset.
  // If there are several calls at a single pc_offset,
  // then they occur in the same order as they were performed by the JVM,
  // with the most recent (innermost) call being described last.
  // For a safepoint, the pc_offset must have been mentioned
  // previously by add_safepoint.
  // Otherwise, the pc_offset must have been mentioned previously
  // by add_non_safepoint, and the locals, expressions, and monitors
  // must all be null.
  void describe_scope(int         pc_offset,
                      const methodHandle& methodH,
                      ciMethod*   method,
                      int         bci,
                      bool        reexecute,
                      bool        rethrow_exception = false,
                      bool        is_method_handle_invoke = false,
                      bool        return_oop = false,
                      DebugToken* locals      = NULL,
                      DebugToken* expressions = NULL,
                      DebugToken* monitors    = NULL);

  void dump_object_pool(GrowableArray<ScopeValue*>* objects);

  // This call must follow every add_safepoint,
  // after any intervening describe_scope calls.
  void end_safepoint(int pc_offset)      { end_scopes(pc_offset, true); }
  void end_non_safepoint(int pc_offset)  { end_scopes(pc_offset, false); }

  // helper fuctions for describe_scope to enable sharing
  DebugToken* create_scope_values(GrowableArray<ScopeValue*>* values);
  DebugToken* create_monitor_values(GrowableArray<MonitorValue*>* monitors);

  // returns the size of the generated scopeDescs.
  int data_size();
  int pcs_size();
  int oop_size() { return oop_recorder()->oop_size(); }
  int metadata_size() { return oop_recorder()->metadata_size(); }

  // copy the generated debugging information to nmethod
  void copy_to(nmethod* nm);

  // Method for setting oopmaps to temporarily preserve old handling of oopmaps
  OopMapSet *_oopmaps;
  void set_oopmaps(OopMapSet *oopmaps) { _oopmaps = oopmaps; }

  OopRecorder* oop_recorder() { return _oop_recorder; }

  int last_pc_offset() { return last_pc()->pc_offset(); }

  PcDesc* pcs() const { return _pcs; }
  int pcs_length() const { return _pcs_length; }

  DebugInfoWriteStream* stream() const { return _stream; }

 private:
  friend class ScopeDesc;
  friend class vframeStreamCommon;
  friend class DIR_Chunk;

  DebugInfoWriteStream* _stream;

  OopRecorder* _oop_recorder;

  // Scopes that have been described so far.
  GrowableArray<DIR_Chunk*>* _all_chunks;
  DIR_Chunk* _next_chunk;
  DIR_Chunk* _next_chunk_limit;

  PcDesc* _pcs;
  int     _pcs_size;
  int     _pcs_length;
  // Note:  Would use GrowableArray<PcDesc>, but structs are not supported.

  // PC of most recent real safepoint before the current one,
  // updated after end_scopes.
  int _prev_safepoint_pc;

  PcDesc* last_pc() {
    guarantee(_pcs_length > 0, "a safepoint must be declared already");
    return &_pcs[_pcs_length - 1];
  }
  PcDesc* prev_pc() {
    guarantee(_pcs_length > 1, "a safepoint must be declared already");
    return &_pcs[_pcs_length - 2];
  }
  void add_new_pc_offset(int pc_offset);
  void end_scopes(int pc_offset, bool is_safepoint);

  int  serialize_monitor_values(GrowableArray<MonitorValue*>* monitors);
  int  serialize_scope_values(GrowableArray<ScopeValue*>* values);
  int  find_sharable_decode_offset(int stream_offset);

 public:
  enum { serialized_null = 0 };
};

#endif

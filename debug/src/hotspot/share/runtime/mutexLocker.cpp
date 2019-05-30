#include "precompiled.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/os.inline.hpp"
#include "runtime/safepoint.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vmThread.hpp"

// Mutexes used in the VM (see comment in mutexLocker.hpp):
//
// Note that the following pointers are effectively final -- after having been
// set at JVM startup-time, they should never be subsequently mutated.
// Instead of using pointers to malloc()ed monitors and mutexes we should consider
// eliminating the indirection and using instances instead.
// Consider using GCC's __read_mostly.

Mutex*   Patching_lock                = NULL;
Monitor* SystemDictionary_lock        = NULL;
Mutex*   SharedDictionary_lock        = NULL;
Mutex*   Module_lock                  = NULL;
Mutex*   CompiledIC_lock              = NULL;
Mutex*   InlineCacheBuffer_lock       = NULL;
Mutex*   VMStatistic_lock             = NULL;
Mutex*   JNIGlobalAlloc_lock          = NULL;
Mutex*   JNIGlobalActive_lock         = NULL;
Mutex*   JNIWeakAlloc_lock            = NULL;
Mutex*   JNIWeakActive_lock           = NULL;
Mutex*   StringTableWeakAlloc_lock    = NULL;
Mutex*   StringTableWeakActive_lock   = NULL;
Mutex*   JNIHandleBlockFreeList_lock  = NULL;
Mutex*   VMWeakAlloc_lock             = NULL;
Mutex*   VMWeakActive_lock            = NULL;
Mutex*   ResolvedMethodTable_lock     = NULL;
Mutex*   JmethodIdCreation_lock       = NULL;
Mutex*   JfieldIdCreation_lock        = NULL;
Monitor* JNICritical_lock             = NULL;
Monitor* Heap_lock                    = NULL;
Mutex*   ExpandHeap_lock              = NULL;
Mutex*   AdapterHandlerLibrary_lock   = NULL;
Mutex*   SignatureHandlerLibrary_lock = NULL;
Mutex*   VtableStubs_lock             = NULL;
Mutex*   SymbolTable_lock             = NULL;
Mutex*   StringTable_lock             = NULL;
Monitor* StringDedupQueue_lock        = NULL;
Mutex*   StringDedupTable_lock        = NULL;
Monitor* CodeCache_lock               = NULL;
Mutex*   MethodData_lock              = NULL;
Mutex*   TouchedMethodLog_lock        = NULL;
Mutex*   RetData_lock                 = NULL;
Monitor* VMOperationQueue_lock        = NULL;
Monitor* VMOperationRequest_lock      = NULL;
Monitor* Safepoint_lock               = NULL;
Monitor* SerializePage_lock           = NULL;
Monitor* Threads_lock                 = NULL;
Monitor* CGC_lock                     = NULL;
Monitor* STS_lock                     = NULL;
Monitor* FullGCCount_lock             = NULL;
Mutex*   SATB_Q_FL_lock               = NULL;
Monitor* SATB_Q_CBL_mon               = NULL;
Mutex*   Shared_SATB_Q_lock           = NULL;
Mutex*   DirtyCardQ_FL_lock           = NULL;
Monitor* DirtyCardQ_CBL_mon           = NULL;
Mutex*   Shared_DirtyCardQ_lock       = NULL;
Mutex*   MarkStackFreeList_lock       = NULL;
Mutex*   MarkStackChunkList_lock      = NULL;
Mutex*   ParGCRareEvent_lock          = NULL;
Mutex*   DerivedPointerTableGC_lock   = NULL;
Monitor* CGCPhaseManager_lock         = NULL;
Mutex*   Compile_lock                 = NULL;
Monitor* MethodCompileQueue_lock      = NULL;
Monitor* CompileThread_lock           = NULL;
Monitor* Compilation_lock             = NULL;
Mutex*   CompileTaskAlloc_lock        = NULL;
Mutex*   CompileStatistics_lock       = NULL;
Mutex*   DirectivesStack_lock         = NULL;
Mutex*   MultiArray_lock              = NULL;
Monitor* Terminator_lock              = NULL;
Monitor* BeforeExit_lock              = NULL;
Monitor* Notify_lock                  = NULL;
Mutex*   ProfilePrint_lock            = NULL;
Mutex*   ExceptionCache_lock          = NULL;
Mutex*   OsrList_lock                 = NULL;

Mutex*   Debug1_lock                  = NULL;
Mutex*   Debug2_lock                  = NULL;
Mutex*   Debug3_lock                  = NULL;

Mutex*   tty_lock                     = NULL;

Mutex*   RawMonitor_lock              = NULL;
Mutex*   PerfDataMemAlloc_lock        = NULL;
Mutex*   PerfDataManager_lock         = NULL;
Mutex*   OopMapCacheAlloc_lock        = NULL;

Mutex*   FreeList_lock                = NULL;
Mutex*   OldSets_lock                 = NULL;
Monitor* RootRegionScan_lock          = NULL;

Monitor* GCTaskManager_lock           = NULL;

Mutex*   Management_lock              = NULL;
Monitor* Service_lock                 = NULL;
Monitor* PeriodicTask_lock            = NULL;
Monitor* RedefineClasses_lock         = NULL;

Mutex*   ThreadHeapSampler_lock       = NULL;

#ifndef SUPPORTS_NATIVE_CX8
Mutex*   UnsafeJlong_lock             = NULL;
#endif
Monitor* CodeHeapStateAnalytics_lock  = NULL;

Mutex*   MetaspaceExpand_lock         = NULL;

#define MAX_NUM_MUTEX 128
static Monitor * _mutex_array[MAX_NUM_MUTEX];
static int _num_mutex;

#define def(var, type, pri, vm_block, safepoint_check_allowed ) { \
  var = new type(Mutex::pri, #var, vm_block, safepoint_check_allowed); \
  assert(_num_mutex < MAX_NUM_MUTEX, "increase MAX_NUM_MUTEX"); \
  _mutex_array[_num_mutex++] = var; \
}

// Using Padded subclasses to prevent false sharing of these global monitors and mutexes.
void mutex_init() {
  def(tty_lock                     , PaddedMutex  , event,       true,  Monitor::_safepoint_check_never);      // allow to lock in VM

  def(CGC_lock                     , PaddedMonitor, special,     true,  Monitor::_safepoint_check_never);      // coordinate between fore- and background GC
  def(STS_lock                     , PaddedMonitor, leaf,        true,  Monitor::_safepoint_check_never);

  def(VMWeakAlloc_lock             , PaddedMutex  , vmweak,      true,  Monitor::_safepoint_check_never);
  def(VMWeakActive_lock            , PaddedMutex  , vmweak-1,    true,  Monitor::_safepoint_check_never);

  def(StringTableWeakAlloc_lock    , PaddedMutex  , vmweak,      true,  Monitor::_safepoint_check_never);
  def(StringTableWeakActive_lock   , PaddedMutex  , vmweak-1,    true,  Monitor::_safepoint_check_never);

  if (UseConcMarkSweepGC || UseG1GC) {
    def(FullGCCount_lock           , PaddedMonitor, leaf,        true,  Monitor::_safepoint_check_never);      // in support of ExplicitGCInvokesConcurrent
  }
  if (UseG1GC) {
    def(SATB_Q_FL_lock             , PaddedMutex  , access,      true,  Monitor::_safepoint_check_never);
    def(SATB_Q_CBL_mon             , PaddedMonitor, access,      true,  Monitor::_safepoint_check_never);
    def(Shared_SATB_Q_lock         , PaddedMutex  , access + 1,  true,  Monitor::_safepoint_check_never);

    def(DirtyCardQ_FL_lock         , PaddedMutex  , access,      true,  Monitor::_safepoint_check_never);
    def(DirtyCardQ_CBL_mon         , PaddedMonitor, access,      true,  Monitor::_safepoint_check_never);
    def(Shared_DirtyCardQ_lock     , PaddedMutex  , access + 1,  true,  Monitor::_safepoint_check_never);

    def(FreeList_lock              , PaddedMutex  , leaf     ,   true,  Monitor::_safepoint_check_never);
    def(OldSets_lock               , PaddedMutex  , leaf     ,   true,  Monitor::_safepoint_check_never);
    def(RootRegionScan_lock        , PaddedMonitor, leaf     ,   true,  Monitor::_safepoint_check_never);

    def(StringDedupQueue_lock      , PaddedMonitor, leaf,        true,  Monitor::_safepoint_check_never);
    def(StringDedupTable_lock      , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_never);

    def(MarkStackFreeList_lock     , PaddedMutex  , leaf     ,   true,  Monitor::_safepoint_check_never);
    def(MarkStackChunkList_lock    , PaddedMutex  , leaf     ,   true,  Monitor::_safepoint_check_never);
  }
  def(ParGCRareEvent_lock          , PaddedMutex  , leaf     ,   true,  Monitor::_safepoint_check_sometimes);
  def(DerivedPointerTableGC_lock   , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_never);
  def(CGCPhaseManager_lock         , PaddedMonitor, leaf,        false, Monitor::_safepoint_check_sometimes);
  def(CodeCache_lock               , PaddedMutex  , special,     true,  Monitor::_safepoint_check_never);
  def(RawMonitor_lock              , PaddedMutex  , special,     true,  Monitor::_safepoint_check_never);
  def(OopMapCacheAlloc_lock        , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_always);     // used for oop_map_cache allocation.

  def(MetaspaceExpand_lock         , PaddedMutex  , leaf-1,      true,  Monitor::_safepoint_check_never);

  def(Patching_lock                , PaddedMutex  , special,     true,  Monitor::_safepoint_check_never);      // used for safepointing and code patching.
  def(Service_lock                 , PaddedMonitor, special,     true,  Monitor::_safepoint_check_never);      // used for service thread operations
  def(JmethodIdCreation_lock       , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_always);     // used for creating jmethodIDs.

  def(SystemDictionary_lock        , PaddedMonitor, leaf,        true,  Monitor::_safepoint_check_always);     // lookups done by VM thread
  def(SharedDictionary_lock        , PaddedMutex,   leaf,        true,  Monitor::_safepoint_check_always);     // lookups done by VM thread
  def(Module_lock                  , PaddedMutex  , leaf+2,      true,  Monitor::_safepoint_check_always);
  def(InlineCacheBuffer_lock       , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_always);
  def(VMStatistic_lock             , PaddedMutex  , leaf,        false, Monitor::_safepoint_check_always);
  def(ExpandHeap_lock              , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_always);     // Used during compilation by VM thread
  def(JNIHandleBlockFreeList_lock  , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_never);      // handles are used by VM thread
  def(SignatureHandlerLibrary_lock , PaddedMutex  , leaf,        false, Monitor::_safepoint_check_always);
  def(SymbolTable_lock             , PaddedMutex  , leaf+2,      true,  Monitor::_safepoint_check_always);
  def(StringTable_lock             , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_always);
  def(ProfilePrint_lock            , PaddedMutex  , leaf,        false, Monitor::_safepoint_check_always);     // serial profile printing
  def(ExceptionCache_lock          , PaddedMutex  , leaf,        false, Monitor::_safepoint_check_always);     // serial profile printing
  def(OsrList_lock                 , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_never);
  def(Debug1_lock                  , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_never);
  def(BeforeExit_lock              , PaddedMonitor, leaf,        true,  Monitor::_safepoint_check_always);
  def(PerfDataMemAlloc_lock        , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_always);     // used for allocating PerfData memory for performance data
  def(PerfDataManager_lock         , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_always);     // used for synchronized access to PerfDataManager resources

  // CMS_modUnionTable_lock                   leaf
  // CMS_bitMap_lock                          leaf 1
  // CMS_freeList_lock                        leaf 2

  def(Safepoint_lock               , PaddedMonitor, safepoint,   true,  Monitor::_safepoint_check_sometimes);  // locks SnippetCache_lock/Threads_lock

  def(Threads_lock                 , PaddedMonitor, barrier,     true,  Monitor::_safepoint_check_sometimes);

  def(VMOperationQueue_lock        , PaddedMonitor, nonleaf,     true,  Monitor::_safepoint_check_sometimes);  // VM_thread allowed to block on these
  def(VMOperationRequest_lock      , PaddedMonitor, nonleaf,     true,  Monitor::_safepoint_check_sometimes);
  def(RetData_lock                 , PaddedMutex  , nonleaf,     false, Monitor::_safepoint_check_always);
  def(Terminator_lock              , PaddedMonitor, nonleaf,     true,  Monitor::_safepoint_check_sometimes);
  def(VtableStubs_lock             , PaddedMutex  , nonleaf,     true,  Monitor::_safepoint_check_always);
  def(Notify_lock                  , PaddedMonitor, nonleaf,     true,  Monitor::_safepoint_check_always);
  // OopStorage-based JNI may lock the alloc_locks while releasing a handle,
  // while previous JNI didn't need a lock for handle release.  This runs afoul
  // of some places which hold other locks while releasing a handle, including
  // the Patching_lock, which is of "special" rank.  As a temporary workaround,
  // lower the JNI oopstorage lock ranks to make them super-special.
  def(JNIGlobalAlloc_lock          , PaddedMutex  , nonleaf,     true,  Monitor::_safepoint_check_never);
  def(JNIGlobalActive_lock         , PaddedMutex  , nonleaf-1,   true,  Monitor::_safepoint_check_never);
  def(JNIWeakAlloc_lock            , PaddedMutex  , nonleaf,     true,  Monitor::_safepoint_check_never);
  def(JNIWeakActive_lock           , PaddedMutex  , nonleaf-1,   true,  Monitor::_safepoint_check_never);
  def(JNICritical_lock             , PaddedMonitor, nonleaf,     true,  Monitor::_safepoint_check_always);     // used for JNI critical regions
  def(AdapterHandlerLibrary_lock   , PaddedMutex  , nonleaf,     true,  Monitor::_safepoint_check_always);

  def(Heap_lock                    , PaddedMonitor, nonleaf+1,   false, Monitor::_safepoint_check_sometimes);
  def(JfieldIdCreation_lock        , PaddedMutex  , nonleaf+1,   true,  Monitor::_safepoint_check_always);     // jfieldID, Used in VM_Operation
  def(ResolvedMethodTable_lock     , PaddedMutex  , nonleaf+1,   false, Monitor::_safepoint_check_always);     // Used to protect ResolvedMethodTable

  def(CompiledIC_lock              , PaddedMutex  , nonleaf+2,   false, Monitor::_safepoint_check_always);     // locks VtableStubs_lock, InlineCacheBuffer_lock
  def(CompileTaskAlloc_lock        , PaddedMutex  , nonleaf+2,   true,  Monitor::_safepoint_check_always);
  def(CompileStatistics_lock       , PaddedMutex  , nonleaf+2,   false, Monitor::_safepoint_check_always);
  def(DirectivesStack_lock         , PaddedMutex  , special,     true,  Monitor::_safepoint_check_never);
  def(MultiArray_lock              , PaddedMutex  , nonleaf+2,   false, Monitor::_safepoint_check_always);     // locks SymbolTable_lock

  def(Management_lock              , PaddedMutex  , nonleaf+2,   false, Monitor::_safepoint_check_always);     // used for JVM management

  def(Compile_lock                 , PaddedMutex  , nonleaf+3,   true,  Monitor::_safepoint_check_sometimes);
  def(MethodData_lock              , PaddedMutex  , nonleaf+3,   false, Monitor::_safepoint_check_always);
  def(TouchedMethodLog_lock        , PaddedMutex  , nonleaf+3,   false, Monitor::_safepoint_check_always);

  def(MethodCompileQueue_lock      , PaddedMonitor, nonleaf+4,   true,  Monitor::_safepoint_check_always);
  def(Debug2_lock                  , PaddedMutex  , nonleaf+4,   true,  Monitor::_safepoint_check_never);
  def(Debug3_lock                  , PaddedMutex  , nonleaf+4,   true,  Monitor::_safepoint_check_never);
  def(CompileThread_lock           , PaddedMonitor, nonleaf+5,   false, Monitor::_safepoint_check_always);
  def(PeriodicTask_lock            , PaddedMonitor, nonleaf+5,   true,  Monitor::_safepoint_check_sometimes);
  def(RedefineClasses_lock         , PaddedMonitor, nonleaf+5,   true,  Monitor::_safepoint_check_always);

  def(ThreadHeapSampler_lock       , PaddedMutex,   nonleaf,     false, Monitor::_safepoint_check_never);

  if (WhiteBoxAPI) {
    def(Compilation_lock           , PaddedMonitor, leaf,        false, Monitor::_safepoint_check_never);
  }

#ifndef SUPPORTS_NATIVE_CX8
  def(UnsafeJlong_lock             , PaddedMutex  , special,     false, Monitor::_safepoint_check_never);
#endif

  def(CodeHeapStateAnalytics_lock  , PaddedMutex  , leaf,        true,  Monitor::_safepoint_check_never);
}

GCMutexLocker::GCMutexLocker(Monitor * mutex) {
  if (SafepointSynchronize::is_at_safepoint()) {
    _locked = false;
  } else {
    _mutex = mutex;
    _locked = true;
    _mutex->lock();
  }
}

// Print all mutexes/monitors that are currently owned by a thread; called
// by fatal error handler.
void print_owned_locks_on_error(outputStream* st) {
  st->print("VM Mutex/Monitor currently owned by a thread: ");
  bool none = true;
  for (int i = 0; i < _num_mutex; i++) {
     // see if it has an owner
     if (_mutex_array[i]->owner() != NULL) {
       if (none) {
          // print format used by Mutex::print_on_error()
          st->print_cr(" ([mutex/lock_event])");
          none = false;
       }
       _mutex_array[i]->print_on_error(st);
       st->cr();
     }
  }
  if (none) st->print_cr("None");
}

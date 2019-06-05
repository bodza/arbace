#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "memory/heap.hpp"
#include "memory/memRegion.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/globals.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "services/management.hpp"
#include "services/memoryManager.hpp"
#include "services/memoryPool.hpp"
#include "services/memoryService.hpp"
#include "utilities/growableArray.hpp"
#include "utilities/macros.hpp"

GrowableArray<MemoryPool*>* MemoryService::_pools_list = new (ResourceObj::C_HEAP, mtInternal) GrowableArray<MemoryPool*>(init_pools_list_size, true);
GrowableArray<MemoryManager*>* MemoryService::_managers_list = new (ResourceObj::C_HEAP, mtInternal) GrowableArray<MemoryManager*>(init_managers_list_size, true);

MemoryManager*   MemoryService::_code_cache_manager    = NULL;
GrowableArray<MemoryPool*>* MemoryService::_code_heap_pools = new (ResourceObj::C_HEAP, mtInternal) GrowableArray<MemoryPool*>(init_code_heap_pools_size, true);
MemoryPool*      MemoryService::_metaspace_pool        = NULL;
MemoryPool*      MemoryService::_compressed_class_pool = NULL;

class GcThreadCountClosure: public ThreadClosure {
 private:
  int _count;
 public:
  GcThreadCountClosure() : _count(0) { };
  void do_thread(Thread* thread);
  int count() { return _count; }
};

void GcThreadCountClosure::do_thread(Thread* thread) {
  _count++;
}

void MemoryService::set_universe_heap(CollectedHeap* heap) {
  ResourceMark rm; // For internal allocations in GrowableArray.

  GrowableArray<MemoryPool*> gc_mem_pools = heap->memory_pools();
  _pools_list->appendAll(&gc_mem_pools);

  // set the GC thread count
  GcThreadCountClosure gctcc;
  heap->gc_threads_do(&gctcc);
  int count = gctcc.count();

  GrowableArray<GCMemoryManager*> gc_memory_managers = heap->memory_managers();
  for (int i = 0; i < gc_memory_managers.length(); i++) {
    GCMemoryManager* gc_manager = gc_memory_managers.at(i);

    if (count > 0) {
      gc_manager->set_num_gc_threads(count);
    }
    gc_manager->initialize_gc_stat_info();
    _managers_list->append(gc_manager);
  }
}

void MemoryService::add_code_heap_memory_pool(CodeHeap* heap, const char* name) {
  // Create new memory pool for this heap
  MemoryPool* code_heap_pool = new CodeHeapPool(heap, name, true /* support_usage_threshold */);

  // Append to lists
  _code_heap_pools->append(code_heap_pool);
  _pools_list->append(code_heap_pool);

  if (_code_cache_manager == NULL) {
    // Create CodeCache memory manager
    _code_cache_manager = MemoryManager::get_code_cache_memory_manager();
    _managers_list->append(_code_cache_manager);
  }

  _code_cache_manager->add_pool(code_heap_pool);
}

void MemoryService::add_metaspace_memory_pools() {
  MemoryManager* mgr = MemoryManager::get_metaspace_memory_manager();

  _metaspace_pool = new MetaspacePool();
  mgr->add_pool(_metaspace_pool);
  _pools_list->append(_metaspace_pool);

  if (UseCompressedClassPointers) {
    _compressed_class_pool = new CompressedKlassSpacePool();
    mgr->add_pool(_compressed_class_pool);
    _pools_list->append(_compressed_class_pool);
  }

  _managers_list->append(mgr);
}

MemoryManager* MemoryService::get_memory_manager(instanceHandle mh) {
  for (int i = 0; i < _managers_list->length(); i++) {
    MemoryManager* mgr = _managers_list->at(i);
    if (mgr->is_manager(mh)) {
      return mgr;
    }
  }
  return NULL;
}

MemoryPool* MemoryService::get_memory_pool(instanceHandle ph) {
  for (int i = 0; i < _pools_list->length(); i++) {
    MemoryPool* pool = _pools_list->at(i);
    if (pool->is_pool(ph)) {
      return pool;
    }
  }
  return NULL;
}

void MemoryService::track_memory_usage() {
  // Track the peak memory usage
  for (int i = 0; i < _pools_list->length(); i++) {
    MemoryPool* pool = _pools_list->at(i);
    pool->record_peak_memory_usage();
  }
}

void MemoryService::track_memory_pool_usage(MemoryPool* pool) {
  // Track the peak memory usage
  pool->record_peak_memory_usage();
}

void MemoryService::gc_begin(GCMemoryManager* manager, bool recordGCBeginTime, bool recordAccumulatedGCTime, bool recordPreGCUsage, bool recordPeakUsage) {
  manager->gc_begin(recordGCBeginTime, recordPreGCUsage, recordAccumulatedGCTime);

  // Track the peak memory usage when GC begins
  if (recordPeakUsage) {
    for (int i = 0; i < _pools_list->length(); i++) {
      MemoryPool* pool = _pools_list->at(i);
      pool->record_peak_memory_usage();
    }
  }
}

void MemoryService::gc_end(GCMemoryManager* manager, bool recordPostGCUsage, bool recordAccumulatedGCTime, bool recordGCEndTime, bool countCollection, GCCause::Cause cause, bool allMemoryPoolsAffected) {
  // register the GC end statistics and memory usage
  manager->gc_end(recordPostGCUsage, recordAccumulatedGCTime, recordGCEndTime, countCollection, cause, allMemoryPoolsAffected);
}

void MemoryService::oops_do(OopClosure* f) {
  int i;

  for (i = 0; i < _pools_list->length(); i++) {
    MemoryPool* pool = _pools_list->at(i);
    pool->oops_do(f);
  }
  for (i = 0; i < _managers_list->length(); i++) {
    MemoryManager* mgr = _managers_list->at(i);
    mgr->oops_do(f);
  }
}

Handle MemoryService::create_MemoryUsage_obj(MemoryUsage usage, TRAPS) {
  InstanceKlass* ik = Management::java_lang_management_MemoryUsage_klass(CHECK_NH);

  JavaCallArguments args(10);
  args.push_long(usage.init_size_as_jlong());
  args.push_long(usage.used_as_jlong());
  args.push_long(usage.committed_as_jlong());
  args.push_long(usage.max_size_as_jlong());

  return JavaCalls::construct_new_instance(ik, vmSymbols::long_long_long_long_void_signature(), &args, CHECK_NH);
}

TraceMemoryManagerStats::TraceMemoryManagerStats(GCMemoryManager* gc_memory_manager,
                                                 GCCause::Cause cause,
                                                 bool allMemoryPoolsAffected,
                                                 bool recordGCBeginTime,
                                                 bool recordPreGCUsage,
                                                 bool recordPeakUsage,
                                                 bool recordPostGCUsage,
                                                 bool recordAccumulatedGCTime,
                                                 bool recordGCEndTime,
                                                 bool countCollection) {
  initialize(gc_memory_manager, cause, allMemoryPoolsAffected,
             recordGCBeginTime, recordPreGCUsage, recordPeakUsage,
             recordPostGCUsage, recordAccumulatedGCTime, recordGCEndTime,
             countCollection);
}

// for a subclass to create then initialize an instance before invoking
// the MemoryService
void TraceMemoryManagerStats::initialize(GCMemoryManager* gc_memory_manager,
                                         GCCause::Cause cause,
                                         bool allMemoryPoolsAffected,
                                         bool recordGCBeginTime,
                                         bool recordPreGCUsage,
                                         bool recordPeakUsage,
                                         bool recordPostGCUsage,
                                         bool recordAccumulatedGCTime,
                                         bool recordGCEndTime,
                                         bool countCollection) {
  _gc_memory_manager = gc_memory_manager;
  _allMemoryPoolsAffected = allMemoryPoolsAffected;
  _recordGCBeginTime = recordGCBeginTime;
  _recordPreGCUsage = recordPreGCUsage;
  _recordPeakUsage = recordPeakUsage;
  _recordPostGCUsage = recordPostGCUsage;
  _recordAccumulatedGCTime = recordAccumulatedGCTime;
  _recordGCEndTime = recordGCEndTime;
  _countCollection = countCollection;
  _cause = cause;

  MemoryService::gc_begin(_gc_memory_manager, _recordGCBeginTime, _recordAccumulatedGCTime, _recordPreGCUsage, _recordPeakUsage);
}

TraceMemoryManagerStats::~TraceMemoryManagerStats() {
  MemoryService::gc_end(_gc_memory_manager, _recordPostGCUsage, _recordAccumulatedGCTime, _recordGCEndTime, _countCollection, _cause, _allMemoryPoolsAffected);
}

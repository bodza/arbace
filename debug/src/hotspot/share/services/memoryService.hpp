#ifndef SHARE_VM_SERVICES_MEMORYSERVICE_HPP
#define SHARE_VM_SERVICES_MEMORYSERVICE_HPP

#include "gc/shared/gcCause.hpp"
#include "memory/allocation.hpp"
#include "runtime/handles.hpp"
#include "services/memoryUsage.hpp"

// Forward declaration
class MemoryPool;
class MemoryManager;
class GCMemoryManager;
class CollectedHeap;
class CodeHeap;

// VM Monitoring and Management Support

class MemoryService : public AllStatic {
private:
  enum {
    init_pools_list_size = 10,
    init_managers_list_size = 5,
    init_code_heap_pools_size = 9
  };

  static GrowableArray<MemoryPool*>*    _pools_list;
  static GrowableArray<MemoryManager*>* _managers_list;

  // memory manager and code heap pools for the CodeCache
  static MemoryManager*                 _code_cache_manager;
  static GrowableArray<MemoryPool*>*    _code_heap_pools;

  static MemoryPool*                    _metaspace_pool;
  static MemoryPool*                    _compressed_class_pool;

public:
  static void set_universe_heap(CollectedHeap* heap);
  static void add_code_heap_memory_pool(CodeHeap* heap, const char* name);
  static void add_metaspace_memory_pools();

  static MemoryPool*    get_memory_pool(instanceHandle pool);
  static MemoryManager* get_memory_manager(instanceHandle mgr);

  static const int num_memory_pools()    { return _pools_list->length(); }
  static const int num_memory_managers() { return _managers_list->length(); }

  static MemoryPool* get_memory_pool(int index)       { return _pools_list->at(index); }
  static MemoryManager* get_memory_manager(int index) { return _managers_list->at(index); }

  static void oops_do(OopClosure* f);
};

#endif

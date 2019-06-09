#ifndef SHARE_VM_SERVICES_MEMORYPOOL_HPP
#define SHARE_VM_SERVICES_MEMORYPOOL_HPP

#include "memory/heap.hpp"
#include "oops/oop.hpp"
#include "services/memoryUsage.hpp"
#include "utilities/macros.hpp"

// A memory pool represents the memory area that the VM manages.
// The Java virtual machine has at least one memory pool
// and it may create or remove memory pools during execution.
// A memory pool can belong to the heap or the non-heap memory.
// A Java virtual machine may also have memory pools belonging to
// both heap and non-heap memory.

class MemoryManager;

class MemoryPool : public CHeapObj<mtInternal> {
  friend class MemoryManager;
 public:
  enum PoolType {
    Heap    = 1,
    NonHeap = 2
  };

 private:
  enum {
    max_num_managers = 5
  };

  // We could make some of the following as performance counters
  // for external monitoring.
  const char*    _name;
  PoolType       _type;
  size_t         _initial_size;
  size_t         _max_size;
  bool           _available_for_allocation; // Default is true
  MemoryManager* _managers[max_num_managers];
  int            _num_managers;
  MemoryUsage    _peak_usage;               // Peak memory usage
  MemoryUsage    _after_gc_usage;           // After GC memory usage

  volatile instanceOop _memory_pool_obj;

  void add_manager(MemoryManager* mgr);

 public:
  MemoryPool(const char* name, PoolType type, size_t init_size, size_t max_size, bool support_usage_threshold, bool support_gc_threshold);

  const char* name()                       { return _name; }
  bool        is_heap()                    { return _type == Heap; }
  bool        is_non_heap()                { return _type == NonHeap; }
  size_t      initial_size()         const { return _initial_size; }
  int         num_memory_managers()  const { return _num_managers; }
  // max size could be changed
  virtual size_t max_size()          const { return _max_size; }

  bool is_pool(instanceHandle pool) { return oopDesc::equals(pool(), _memory_pool_obj); }

  bool available_for_allocation()   { return _available_for_allocation; }
  bool set_available_for_allocation(bool value) {
    bool prev = _available_for_allocation;
    _available_for_allocation = value;
    return prev;
  }

  MemoryManager* get_memory_manager(int index) {
    return _managers[index];
  }

  // Records current memory usage if it's a peak usage
  void record_peak_memory_usage();

  MemoryUsage get_peak_memory_usage() {
    // check current memory usage first and then return peak usage
    record_peak_memory_usage();
    return _peak_usage;
  }
  void reset_peak_memory_usage() {
    _peak_usage = get_memory_usage();
  }

  void set_last_collection_usage(MemoryUsage u)   { _after_gc_usage = u; }

  virtual instanceOop get_memory_pool_instance(TRAPS);
  virtual MemoryUsage get_memory_usage() = 0;
  virtual size_t      used_in_bytes() = 0;
  virtual bool        is_collected_pool()         { return false; }
  virtual MemoryUsage get_last_collection_usage() { return _after_gc_usage; }

  // GC support
  void oops_do(OopClosure* f);
};

class CollectedMemoryPool : public MemoryPool {
public:
  CollectedMemoryPool(const char* name, size_t init_size, size_t max_size, bool support_usage_threshold) :
    MemoryPool(name, MemoryPool::Heap, init_size, max_size, support_usage_threshold, true) { };
  bool is_collected_pool() { return true; }
};

class CodeHeapPool: public MemoryPool {
private:
  CodeHeap* _codeHeap;
public:
  CodeHeapPool(CodeHeap* codeHeap, const char* name, bool support_usage_threshold);
  MemoryUsage get_memory_usage();
  size_t used_in_bytes() { return _codeHeap->allocated_capacity(); }
};

class MetaspacePool : public MemoryPool {
  size_t calculate_max_size() const;
 public:
  MetaspacePool();
  MemoryUsage get_memory_usage();
  size_t used_in_bytes();
};

class CompressedKlassSpacePool : public MemoryPool {
 public:
  CompressedKlassSpacePool();
  MemoryUsage get_memory_usage();
  size_t used_in_bytes();
};

#endif

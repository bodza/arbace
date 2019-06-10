#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "memory/allocation.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/orderAccess.hpp"
#include "services/management.hpp"
#include "services/memoryManager.hpp"
#include "services/memoryPool.hpp"
#include "services/memoryService.hpp"

MemoryManager::MemoryManager(const char* name) : _name(name) {
  _num_pools = 0;
  (void)const_cast<instanceOop&>(_memory_mgr_obj = instanceOop(NULL));
}

int MemoryManager::add_pool(MemoryPool* pool) {
  int index = _num_pools;
  if (index < MemoryManager::max_num_pools) {
    _pools[index] = pool;
    _num_pools++;
  }
  pool->add_manager(this);
  return index;
}

MemoryManager* MemoryManager::get_code_cache_memory_manager() {
  return new MemoryManager("CodeCacheManager");
}

MemoryManager* MemoryManager::get_metaspace_memory_manager() {
  return new MemoryManager("Metaspace Manager");
}

instanceOop MemoryManager::get_memory_manager_instance(TRAPS) {
  // Must do an acquire so as to force ordering of subsequent
  // loads from anything _memory_mgr_obj points to or implies.
  instanceOop mgr_obj = OrderAccess::load_acquire(&_memory_mgr_obj);
  if (mgr_obj == NULL) {
    // It's ok for more than one thread to execute the code up to the locked region.
    // Extra manager instances will just be gc'ed.
    Klass* k = Management::sun_management_ManagementFactoryHelper_klass(CHECK_0);

    Handle mgr_name = java_lang_String::create_from_str(name(), CHECK_0);

    JavaValue result(T_OBJECT);
    JavaCallArguments args;
    args.push_oop(mgr_name);    // Argument 1

    Symbol* method_name = NULL;
    Symbol* signature = NULL;
    if (is_gc_memory_manager()) {
      Klass* extKlass = Management::com_sun_management_internal_GarbageCollectorExtImpl_klass(CHECK_0);
      // com.sun.management.GarbageCollectorMXBean is in jdk.management module which may not be present.
      if (extKlass != NULL) {
        k = extKlass;
      }

      method_name = vmSymbols::createGarbageCollector_name();

      signature = vmSymbols::createGarbageCollector_signature();
      args.push_oop(Handle());      // Argument 2 (for future extension)
    } else {
      method_name = vmSymbols::createMemoryManager_name();
      signature = vmSymbols::createMemoryManager_signature();
    }

    InstanceKlass* ik = InstanceKlass::cast(k);

    JavaCalls::call_static(&result, ik, method_name, signature, &args, CHECK_0);

    instanceOop m = (instanceOop) result.get_jobject();
    instanceHandle mgr(THREAD, m);

    {
      // Get lock before setting _memory_mgr_obj
      // since another thread may have created the instance
      MutexLocker ml(Management_lock);

      // Check if another thread has created the management object.  We reload
      // _memory_mgr_obj here because some other thread may have initialized
      // it while we were executing the code before the lock.
      //
      // The lock has done an acquire, so the load can't float above it, but
      // we need to do a load_acquire as above.
      mgr_obj = OrderAccess::load_acquire(&_memory_mgr_obj);
      if (mgr_obj != NULL) {
         return mgr_obj;
      }

      // Get the address of the object we created via call_special.
      mgr_obj = mgr();

      // Use store barrier to make sure the memory accesses associated
      // with creating the management object are visible before publishing
      // its address.  The unlock will publish the store to _memory_mgr_obj
      // because it does a release first.
      OrderAccess::release_store(&_memory_mgr_obj, mgr_obj);
    }
  }

  return mgr_obj;
}

void MemoryManager::oops_do(OopClosure* f) {
  f->do_oop((oop*) &_memory_mgr_obj);
}

GCStatInfo::GCStatInfo(int num_pools) {
  // initialize the arrays for memory usage
  _before_gc_usage_array = (MemoryUsage*) NEW_C_HEAP_ARRAY(MemoryUsage, num_pools, mtInternal);
  _after_gc_usage_array  = (MemoryUsage*) NEW_C_HEAP_ARRAY(MemoryUsage, num_pools, mtInternal);
  _usage_array_size = num_pools;
  clear();
}

GCStatInfo::~GCStatInfo() {
  FREE_C_HEAP_ARRAY(MemoryUsage*, _before_gc_usage_array);
  FREE_C_HEAP_ARRAY(MemoryUsage*, _after_gc_usage_array);
}

void GCStatInfo::set_gc_usage(int pool_index, MemoryUsage usage, bool before_gc) {
  MemoryUsage* gc_usage_array;
  if (before_gc) {
    gc_usage_array = _before_gc_usage_array;
  } else {
    gc_usage_array = _after_gc_usage_array;
  }
  gc_usage_array[pool_index] = usage;
}

void GCStatInfo::clear() {
  _index = 0;
  _start_time = 0L;
  _end_time = 0L;
  size_t len = _usage_array_size * sizeof(MemoryUsage);
  memset(_before_gc_usage_array, 0, len);
  memset(_after_gc_usage_array, 0, len);
}

GCMemoryManager::GCMemoryManager(const char* name, const char* gc_end_message) :
  MemoryManager(name), _gc_end_message(gc_end_message) {
  _num_collections = 0;
  _last_gc_stat = NULL;
  _last_gc_lock = new Mutex(Mutex::leaf, "_last_gc_lock", true, Monitor::_safepoint_check_never);
  _current_gc_stat = NULL;
  _num_gc_threads = 1;
}

GCMemoryManager::~GCMemoryManager() {
  delete _last_gc_stat;
  delete _last_gc_lock;
  delete _current_gc_stat;
}

void GCMemoryManager::add_pool(MemoryPool* pool) {
  add_pool(pool, true);
}

void GCMemoryManager::add_pool(MemoryPool* pool, bool always_affected_by_gc) {
  int index = MemoryManager::add_pool(pool);
  _pool_always_affected_by_gc[index] = always_affected_by_gc;
}

void GCMemoryManager::initialize_gc_stat_info() {
  _last_gc_stat = new(ResourceObj::C_HEAP, mtGC) GCStatInfo(MemoryService::num_memory_pools());
  _current_gc_stat = new(ResourceObj::C_HEAP, mtGC) GCStatInfo(MemoryService::num_memory_pools());
  // tracking concurrent collections we need two objects: one to update, and one to
  // hold the publicly available "last (completed) gc" information.
}

size_t GCMemoryManager::get_last_gc_stat(GCStatInfo* dest) {
  MutexLockerEx ml(_last_gc_lock, Mutex::_no_safepoint_check_flag);
  if (_last_gc_stat->gc_index() != 0) {
    dest->set_index(_last_gc_stat->gc_index());
    dest->set_start_time(_last_gc_stat->start_time());
    dest->set_end_time(_last_gc_stat->end_time());
    size_t len = dest->usage_array_size() * sizeof(MemoryUsage);
    memcpy(dest->before_gc_usage_array(), _last_gc_stat->before_gc_usage_array(), len);
    memcpy(dest->after_gc_usage_array(), _last_gc_stat->after_gc_usage_array(), len);
  }
  return _last_gc_stat->gc_index();
}

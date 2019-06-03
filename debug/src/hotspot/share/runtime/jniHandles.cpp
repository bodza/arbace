#include "precompiled.hpp"

#include "gc/shared/oopStorage.inline.hpp"
#include "memory/iterator.hpp"
#include "oops/access.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/align.hpp"
#include "utilities/debug.hpp"

OopStorage* JNIHandles::_global_handles = NULL;
OopStorage* JNIHandles::_weak_global_handles = NULL;

OopStorage* JNIHandles::global_handles() {
  return _global_handles;
}

OopStorage* JNIHandles::weak_global_handles() {
  return _weak_global_handles;
}

jobject JNIHandles::make_local(oop obj) {
  if (obj == NULL) {
    return NULL;                // ignore null handles
  } else {
    Thread* thread = Thread::current();
    return thread->active_handles()->allocate_handle(obj);
  }
}

// optimized versions

jobject JNIHandles::make_local(Thread* thread, oop obj) {
  if (obj == NULL) {
    return NULL;                // ignore null handles
  } else {
    return thread->active_handles()->allocate_handle(obj);
  }
}

jobject JNIHandles::make_local(JNIEnv* env, oop obj) {
  if (obj == NULL) {
    return NULL;                // ignore null handles
  } else {
    JavaThread* thread = JavaThread::thread_from_jni_environment(env);
    return thread->active_handles()->allocate_handle(obj);
  }
}

static void report_handle_allocation_failure(AllocFailType alloc_failmode, const char* handle_kind) {
  if (alloc_failmode == AllocFailStrategy::EXIT_OOM) {
    // Fake size value, since we don't know the min allocation size here.
    vm_exit_out_of_memory(sizeof(oop), OOM_MALLOC_ERROR, "Cannot create %s JNI handle", handle_kind);
  }
}

jobject JNIHandles::make_global(Handle obj, AllocFailType alloc_failmode) {
  jobject res = NULL;
  if (!obj.is_null()) {
    oop* ptr = global_handles()->allocate();
    // Return NULL on allocation failure.
    if (ptr != NULL) {
      NativeAccess<>::oop_store(ptr, obj());
      res = reinterpret_cast<jobject>(ptr);
    } else {
      report_handle_allocation_failure(alloc_failmode, "global");
    }
  } else {
    CHECK_UNHANDLED_OOPS_ONLY(Thread::current()->clear_unhandled_oops());
  }

  return res;
}

jobject JNIHandles::make_weak_global(Handle obj, AllocFailType alloc_failmode) {
  jobject res = NULL;
  if (!obj.is_null()) {
    oop* ptr = weak_global_handles()->allocate();
    // Return NULL on allocation failure.
    if (ptr != NULL) {
      NativeAccess<ON_PHANTOM_OOP_REF>::oop_store(ptr, obj());
      char* tptr = reinterpret_cast<char*>(ptr) + weak_tag_value;
      res = reinterpret_cast<jobject>(tptr);
    } else {
      report_handle_allocation_failure(alloc_failmode, "weak global");
    }
  } else {
    CHECK_UNHANDLED_OOPS_ONLY(Thread::current()->clear_unhandled_oops());
  }
  return res;
}

// Resolve some erroneous cases to NULL, rather than treating them as
// possibly unchecked errors.  In particular, deleted handles are
// treated as NULL (though a deleted and later reallocated handle
// isn't detected).
oop JNIHandles::resolve_external_guard(jobject handle) {
  oop result = NULL;
  if (handle != NULL) {
    result = resolve_impl<true /* external_guard */ >(handle);
  }
  return result;
}

oop JNIHandles::resolve_jweak(jweak handle) {
  return NativeAccess<ON_PHANTOM_OOP_REF>::oop_load(jweak_ptr(handle));
}

bool JNIHandles::is_global_weak_cleared(jweak handle) {
  oop* oop_ptr = jweak_ptr(handle);
  oop value = NativeAccess<ON_PHANTOM_OOP_REF | AS_NO_KEEPALIVE>::oop_load(oop_ptr);
  return value == NULL;
}

void JNIHandles::destroy_global(jobject handle) {
  if (handle != NULL) {
    oop* oop_ptr = jobject_ptr(handle);
    NativeAccess<>::oop_store(oop_ptr, (oop)NULL);
    global_handles()->release(oop_ptr);
  }
}

void JNIHandles::destroy_weak_global(jobject handle) {
  if (handle != NULL) {
    oop* oop_ptr = jweak_ptr(handle);
    NativeAccess<ON_PHANTOM_OOP_REF>::oop_store(oop_ptr, (oop)NULL);
    weak_global_handles()->release(oop_ptr);
  }
}

void JNIHandles::oops_do(OopClosure* f) {
  global_handles()->oops_do(f);
}

void JNIHandles::weak_oops_do(BoolObjectClosure* is_alive, OopClosure* f) {
  weak_global_handles()->weak_oops_do(is_alive, f);
}

void JNIHandles::weak_oops_do(OopClosure* f) {
  weak_global_handles()->weak_oops_do(f);
}

void JNIHandles::initialize() {
  _global_handles = new OopStorage("JNI Global", JNIGlobalAlloc_lock, JNIGlobalActive_lock);
  _weak_global_handles = new OopStorage("JNI Weak", JNIWeakAlloc_lock, JNIWeakActive_lock);
}

inline bool is_storage_handle(const OopStorage* storage, const oop* ptr) {
  return storage->allocation_status(ptr) == OopStorage::ALLOCATED_ENTRY;
}

jobjectRefType JNIHandles::handle_type(Thread* thread, jobject handle) {
  jobjectRefType result = JNIInvalidRefType;
  if (is_jweak(handle)) {
    if (is_storage_handle(weak_global_handles(), jweak_ptr(handle))) {
      result = JNIWeakGlobalRefType;
    }
  } else {
    switch (global_handles()->allocation_status(jobject_ptr(handle))) {
    case OopStorage::ALLOCATED_ENTRY:
      result = JNIGlobalRefType;
      break;

    case OopStorage::UNALLOCATED_ENTRY:
      break;                    // Invalid global handle

    case OopStorage::INVALID_ENTRY:
      // Not in global storage.  Might be a local handle.
      if (is_local_handle(thread, handle) || (thread->is_Java_thread() && is_frame_handle((JavaThread*)thread, handle))) {
        result = JNILocalRefType;
      }
      break;

    default:
      ShouldNotReachHere();
    }
  }
  return result;
}

bool JNIHandles::is_local_handle(Thread* thread, jobject handle) {
  JNIHandleBlock* block = thread->active_handles();

  // Look back past possible native calls to jni_PushLocalFrame.
  while (block != NULL) {
    if (block->chain_contains(handle)) {
      return true;
    }
    block = block->pop_frame_link();
  }
  return false;
}

// Determine if the handle is somewhere in the current thread's stack.
// We easily can't isolate any particular stack frame the handle might
// come from, so we'll check the whole stack.

bool JNIHandles::is_frame_handle(JavaThread* thr, jobject handle) {
  // If there is no java frame, then this must be top level code, such
  // as the java command executable, in which case, this type of handle
  // is not permitted.
  return (thr->has_last_Java_frame() && (void*)handle < (void*)thr->stack_base() && (void*)handle >= (void*)thr->last_Java_sp());
}

bool JNIHandles::is_global_handle(jobject handle) {
  return !is_jweak(handle) && is_storage_handle(global_handles(), jobject_ptr(handle));
}

bool JNIHandles::is_weak_global_handle(jobject handle) {
  return is_jweak(handle) && is_storage_handle(weak_global_handles(), jweak_ptr(handle));
}

size_t JNIHandles::global_handle_memory_usage() {
  return global_handles()->total_memory_usage();
}

size_t JNIHandles::weak_global_handle_memory_usage() {
  return weak_global_handles()->total_memory_usage();
}

// We assume this is called at a safepoint: no lock is needed.
void JNIHandles::print_on(outputStream* st) {
  st->print_cr("JNI global refs: " SIZE_FORMAT ", weak refs: " SIZE_FORMAT,
               global_handles()->allocation_count(),
               weak_global_handles()->allocation_count());
  st->cr();
  st->flush();
}

class VerifyJNIHandles: public OopClosure {
public:
  virtual void do_oop(oop* root) {
    (*root)->verify();
  }
  virtual void do_oop(narrowOop* root) { ShouldNotReachHere(); }
};

void JNIHandles::verify() {
  VerifyJNIHandles verify_handle;

  oops_do(&verify_handle);

  // JNI weaks are handled concurrently in ZGC, so they can't be verified here
  if (!UseZGC) {
    weak_oops_do(&verify_handle);
  }
}

// This method is implemented here to avoid circular includes between
// jniHandles.hpp and thread.hpp.
bool JNIHandles::current_thread_in_native() {
  Thread* thread = Thread::current();
  return (thread->is_Java_thread() && JavaThread::current()->thread_state() == _thread_in_native);
}

void jni_handles_init() {
  JNIHandles::initialize();
}

int             JNIHandleBlock::_blocks_allocated     = 0;
JNIHandleBlock* JNIHandleBlock::_block_free_list      = NULL;

JNIHandleBlock* JNIHandleBlock::allocate_block(Thread* thread) {
  JNIHandleBlock* block;
  // Check the thread-local free list for a block so we don't
  // have to acquire a mutex.
  if (thread != NULL && thread->free_handle_block() != NULL) {
    block = thread->free_handle_block();
    thread->set_free_handle_block(block->_next);
  } else {
    // locking with safepoint checking introduces a potential deadlock:
    // - we would hold JNIHandleBlockFreeList_lock and then Threads_lock
    // - another would hold Threads_lock (jni_AttachCurrentThread) and then
    //   JNIHandleBlockFreeList_lock (JNIHandleBlock::allocate_block)
    MutexLockerEx ml(JNIHandleBlockFreeList_lock,
                     Mutex::_no_safepoint_check_flag);
    if (_block_free_list == NULL) {
      // Allocate new block
      block = new JNIHandleBlock();
      _blocks_allocated++;
      block->zap();
    } else {
      // Get block from free list
      block = _block_free_list;
      _block_free_list = _block_free_list->_next;
    }
  }
  block->_top = 0;
  block->_next = NULL;
  block->_pop_frame_link = NULL;
  block->_planned_capacity = block_size_in_oops;
  return block;
}

void JNIHandleBlock::release_block(JNIHandleBlock* block, Thread* thread) {
  JNIHandleBlock* pop_frame_link = block->pop_frame_link();
  // Put returned block at the beginning of the thread-local free list.
  // Note that if thread == NULL, we use it as an implicit argument that
  // we _don't_ want the block to be kept on the free_handle_block.
  // See for instance JavaThread::exit().
  if (thread != NULL ) {
    block->zap();
    JNIHandleBlock* freelist = thread->free_handle_block();
    block->_pop_frame_link = NULL;
    thread->set_free_handle_block(block);

    // Add original freelist to end of chain
    if (freelist != NULL ) {
      while ( block->_next != NULL ) block = block->_next;
      block->_next = freelist;
    }
    block = NULL;
  }
  if (block != NULL) {
    // Return blocks to free list
    // locking with safepoint checking introduces a potential deadlock:
    // - we would hold JNIHandleBlockFreeList_lock and then Threads_lock
    // - another would hold Threads_lock (jni_AttachCurrentThread) and then
    //   JNIHandleBlockFreeList_lock (JNIHandleBlock::allocate_block)
    MutexLockerEx ml(JNIHandleBlockFreeList_lock,
                     Mutex::_no_safepoint_check_flag);
    while (block != NULL) {
      block->zap();
      JNIHandleBlock* next = block->_next;
      block->_next = _block_free_list;
      _block_free_list = block;
      block = next;
    }
  }
  if (pop_frame_link != NULL) {
    // As a sanity check we release blocks pointed to by the pop_frame_link.
    // This should never happen (only if PopLocalFrame is not called the
    // correct number of times).
    release_block(pop_frame_link, thread);
  }
}

void JNIHandleBlock::oops_do(OopClosure* f) {
  JNIHandleBlock* current_chain = this;
  // Iterate over chain of blocks, followed by chains linked through the
  // pop frame links.
  while (current_chain != NULL) {
    for (JNIHandleBlock* current = current_chain; current != NULL; current = current->_next) {
      for (int index = 0; index < current->_top; index++) {
        oop* root = &(current->_handles)[index];
        oop value = *root;
        // traverse heap pointers only, not deleted handles or free list
        // pointers
        if (value != NULL && Universe::heap()->is_in_reserved(value)) {
          f->do_oop(root);
        }
      }
      // the next handle block is valid only if current block is full
      if (current->_top < block_size_in_oops) {
        break;
      }
    }
    current_chain = current_chain->pop_frame_link();
  }
}

jobject JNIHandleBlock::allocate_handle(oop obj) {
  if (_top == 0) {
    // This is the first allocation or the initial block got zapped when
    // entering a native function. If we have any following blocks they are
    // not valid anymore.
    for (JNIHandleBlock* current = _next; current != NULL; current = current->_next) {
      if (current->_top == 0) {
        // All blocks after the first clear trailing block are already cleared.
        break;
      }
      current->_top = 0;
      current->zap();
    }
    // Clear initial block
    _free_list = NULL;
    _allocate_before_rebuild = 0;
    _last = this;
    zap();
  }

  // Try last block
  if (_last->_top < block_size_in_oops) {
    oop* handle = &(_last->_handles)[_last->_top++];
    NativeAccess<IS_DEST_UNINITIALIZED>::oop_store(handle, obj);
    return (jobject) handle;
  }

  // Try free list
  if (_free_list != NULL) {
    oop* handle = _free_list;
    _free_list = (oop*) *_free_list;
    NativeAccess<IS_DEST_UNINITIALIZED>::oop_store(handle, obj);
    return (jobject) handle;
  }
  // Check if unused block follow last
  if (_last->_next != NULL) {
    // update last and retry
    _last = _last->_next;
    return allocate_handle(obj);
  }

  // No space available, we have to rebuild free list or expand
  if (_allocate_before_rebuild == 0) {
      rebuild_free_list();        // updates _allocate_before_rebuild counter
  } else {
    // Append new block
    Thread* thread = Thread::current();
    Handle obj_handle(thread, obj);
    // This can block, so we need to preserve obj across call.
    _last->_next = JNIHandleBlock::allocate_block(thread);
    _last = _last->_next;
    _allocate_before_rebuild--;
    obj = obj_handle();
  }
  return allocate_handle(obj);  // retry
}

void JNIHandleBlock::rebuild_free_list() {
  int free = 0;
  int blocks = 0;
  for (JNIHandleBlock* current = this; current != NULL; current = current->_next) {
    for (int index = 0; index < current->_top; index++) {
      oop* handle = &(current->_handles)[index];
      if (*handle == NULL) {
        // this handle was cleared out by a delete call, reuse it
        *handle = (oop) _free_list;
        _free_list = handle;
        free++;
      }
    }
    blocks++;
  }
  // Heuristic: if more than half of the handles are free we rebuild next time
  // as well, otherwise we append a corresponding number of new blocks before
  // attempting a free list rebuild again.
  int total = blocks * block_size_in_oops;
  int extra = total - 2*free;
  if (extra > 0) {
    // Not as many free handles as we would like - compute number of new blocks to append
    _allocate_before_rebuild = (extra + block_size_in_oops - 1) / block_size_in_oops;
  }
}

bool JNIHandleBlock::contains(jobject handle) const {
  return ((jobject)&_handles[0] <= handle && handle<(jobject)&_handles[_top]);
}

bool JNIHandleBlock::chain_contains(jobject handle) const {
  for (JNIHandleBlock* current = (JNIHandleBlock*) this; current != NULL; current = current->_next) {
    if (current->contains(handle)) {
      return true;
    }
  }
  return false;
}

size_t JNIHandleBlock::length() const {
  size_t result = 1;
  for (JNIHandleBlock* current = _next; current != NULL; current = current->_next) {
    result++;
  }
  return result;
}

class CountJNIHandleClosure: public OopClosure {
private:
  int _count;
public:
  CountJNIHandleClosure(): _count(0) { }
  virtual void do_oop(oop* ooph) { _count++; }
  virtual void do_oop(narrowOop* unused) { ShouldNotReachHere(); }
  int count() { return _count; }
};

const size_t JNIHandleBlock::get_number_of_live_handles() {
  CountJNIHandleClosure counter;
  oops_do(&counter);
  return counter.count();
}

// This method is not thread-safe, i.e., must be called while holding a lock on the
// structure.
size_t JNIHandleBlock::memory_usage() const {
  return length() * sizeof(JNIHandleBlock);
}

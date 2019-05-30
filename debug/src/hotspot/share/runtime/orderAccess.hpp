#ifndef SHARE_VM_RUNTIME_ORDERACCESS_HPP
#define SHARE_VM_RUNTIME_ORDERACCESS_HPP

#include "memory/allocation.hpp"
#include "runtime/atomic.hpp"
#include "utilities/macros.hpp"

//                Memory Access Ordering Model
//
// This interface is based on the JSR-133 Cookbook for Compiler Writers.
//
// In the following, the terms 'previous', 'subsequent', 'before',
// 'after', 'preceding' and 'succeeding' refer to program order.  The
// terms 'down' and 'below' refer to forward load or store motion
// relative to program order, while 'up' and 'above' refer to backward
// motion.
//
// We define four primitive memory barrier operations.
//
// LoadLoad:   Load1(s); LoadLoad; Load2
//
// Ensures that Load1 completes (obtains the value it loads from memory)
// before Load2 and any subsequent load operations.  Loads before Load1
// may *not* float below Load2 and any subsequent load operations.
//
// StoreStore: Store1(s); StoreStore; Store2
//
// Ensures that Store1 completes (the effect on memory of Store1 is made
// visible to other processors) before Store2 and any subsequent store
// operations.  Stores before Store1 may *not* float below Store2 and any
// subsequent store operations.
//
// LoadStore:  Load1(s); LoadStore; Store2
//
// Ensures that Load1 completes before Store2 and any subsequent store
// operations.  Loads before Load1 may *not* float below Store2 and any
// subsequent store operations.
//
// StoreLoad:  Store1(s); StoreLoad; Load2
//
// Ensures that Store1 completes before Load2 and any subsequent load
// operations.  Stores before Store1 may *not* float below Load2 and any
// subsequent load operations.
//
// We define two further barriers: acquire and release.
//
// Conceptually, acquire/release semantics form unidirectional and
// asynchronous barriers w.r.t. a synchronizing load(X) and store(X) pair.
// They should always be used in pairs to publish (release store) and
// access (load acquire) some implicitly understood shared data between
// threads in a relatively cheap fashion not requiring storeload. If not
// used in such a pair, it is advised to use a membar instead:
// acquire/release only make sense as pairs.
//
// T1: access_shared_data
// T1: ]release
// T1: (...)
// T1: store(X)
//
// T2: load(X)
// T2: (...)
// T2: acquire[
// T2: access_shared_data
//
// It is guaranteed that if T2: load(X) synchronizes with (observes the
// value written by) T1: store(X), then the memory accesses before the T1:
// ]release happen before the memory accesses after the T2: acquire[.
//
// Total Store Order (TSO) machines can be seen as machines issuing a
// release store for each store and a load acquire for each load. Therefore
// there is an inherent resemblence between TSO and acquire/release
// semantics. TSO can be seen as an abstract machine where loads are
// executed immediately when encountered (hence loadload reordering not
// happening) but enqueues stores in a FIFO queue
// for asynchronous serialization (neither storestore or loadstore
// reordering happening). The only reordering happening is storeload due to
// the queue asynchronously serializing stores (yet in order).
//
// Acquire/release semantics essentially exploits this asynchronicity: when
// the load(X) acquire[ observes the store of ]release store(X), the
// accesses before the release must have happened before the accesses after
// acquire.
//
// The API offers both stand-alone acquire() and release() as well as bound
// load_acquire() and release_store(). It is guaranteed that these are
// semantically equivalent w.r.t. the defined model. However, since
// stand-alone acquire()/release() does not know which previous
// load/subsequent store is considered the synchronizing load/store, they
// may be more conservative in implementations. We advise using the bound
// variants whenever possible.
//
// Finally, we define a "fence" operation, as a bidirectional barrier.
// It guarantees that any memory access preceding the fence is not
// reordered w.r.t. any memory accesses subsequent to the fence in program
// order. This may be used to prevent sequences of loads from floating up
// above sequences of stores.
//
// The following table shows the implementations on some architectures:
//
//                       Constraint     x86          sparc TSO          ppc
// ---------------------------------------------------------------------------
// fence                 LoadStore  |   lock         membar #StoreLoad  sync
//                       StoreStore |   addl 0,(sp)
//                       LoadLoad   |
//                       StoreLoad
//
// release               LoadStore  |                                   lwsync
//                       StoreStore
//
// acquire               LoadLoad   |                                   lwsync
//                       LoadStore
//
// release_store                        <store>      <store>            lwsync
//                                                                      <store>
//
// release_store_fence                  xchg         <store>            lwsync
//                                                   membar #StoreLoad  <store>
//                                                                      sync
//
//
// load_acquire                         <load>       <load>             <load>
//                                                                      lwsync
//
// Ordering a load relative to preceding stores requires a StoreLoad,
// which implies a membar #StoreLoad between the store and load under
// sparc-TSO. On x86, we use explicitly locked add.
//
// Conventional usage is to issue a load_acquire for ordered loads.  Use
// release_store for ordered stores when you care only that prior stores
// are visible before the release_store, but don't care exactly when the
// store associated with the release_store becomes visible.  Use
// release_store_fence to update values like the thread state, where we
// don't want the current thread to continue until all our prior memory
// accesses (including the new thread state) are visible to other threads.
// This is equivalent to the volatile semantics of the Java Memory Model.
//
//                    C++ Volatile Semantics
//
// C++ volatile semantics prevent compiler re-ordering between
// volatile memory accesses. However, reordering between non-volatile
// and volatile memory accesses is in general undefined. For compiler
// reordering constraints taking non-volatile memory accesses into
// consideration, a compiler barrier has to be used instead.  Some
// compiler implementations may choose to enforce additional
// constraints beyond those required by the language. Note also that
// both volatile semantics and compiler barrier do not prevent
// hardware reordering.
//
//                os::is_MP Considered Redundant
//
// Callers of this interface do not need to test os::is_MP() before
// issuing an operation. The test is taken care of by the implementation
// of the interface (depending on the vm version and platform, the test
// may or may not be actually done by the implementation).
//
//
//                A Note on Memory Ordering and Cache Coherency
//
// Cache coherency and memory ordering are orthogonal concepts, though they
// interact.  E.g., all existing itanium machines are cache-coherent, but
// the hardware can freely reorder loads wrt other loads unless it sees a
// load-acquire instruction.  All existing sparc machines are cache-coherent
// and, unlike itanium, TSO guarantees that the hardware orders loads wrt
// loads and stores, and stores wrt to each other.
//
// Consider the implementation of loadload.  *If* your platform *isn't*
// cache-coherent, then loadload must not only prevent hardware load
// instruction reordering, but it must *also* ensure that subsequent
// loads from addresses that could be written by other processors (i.e.,
// that are broadcast by other processors) go all the way to the first
// level of memory shared by those processors and the one issuing
// the loadload.
//
// So if we have a MP that has, say, a per-processor D$ that doesn't see
// writes by other processors, and has a shared E$ that does, the loadload
// barrier would have to make sure that either
//
// 1. cache lines in the issuing processor's D$ that contained data from
// addresses that could be written by other processors are invalidated, so
// subsequent loads from those addresses go to the E$, (it could do this
// by tagging such cache lines as 'shared', though how to tell the hardware
// to do the tagging is an interesting problem), or
//
// 2. there never are such cache lines in the issuing processor's D$, which
// means all references to shared data (however identified: see above)
// bypass the D$ (i.e., are satisfied from the E$).
//
// If your machine doesn't have an E$, substitute 'main memory' for 'E$'.
//
// Either of these alternatives is a pain, so no current machine we know of
// has incoherent caches.
//
// If loadload didn't have these properties, the store-release sequence for
// publishing a shared data structure wouldn't work, because a processor
// trying to read data newly published by another processor might go to
// its own incoherent caches to satisfy the read instead of to the newly
// written shared memory.
//
//
//                NOTE WELL!!
//
//                A Note on MutexLocker and Friends
//
// See mutexLocker.hpp.  We assume throughout the VM that MutexLocker's
// and friends' constructors do a fence, a lock and an acquire *in that
// order*.  And that their destructors do a release and unlock, in *that*
// order.  If their implementations change such that these assumptions
// are violated, a whole lot of code will break.

enum ScopedFenceType {
    X_ACQUIRE
  , RELEASE_X
  , RELEASE_X_FENCE
};

template <ScopedFenceType T>
class ScopedFenceGeneral: public StackObj {
 public:
  void prefix() {}
  void postfix() {}
};

template <ScopedFenceType T>
class ScopedFence : public ScopedFenceGeneral<T> {
  void *const _field;
 public:
  ScopedFence(void *const field) : _field(field) { prefix(); }
  ~ScopedFence() { postfix(); }
  void prefix() { ScopedFenceGeneral<T>::prefix(); }
  void postfix() { ScopedFenceGeneral<T>::postfix(); }
};

class OrderAccess : private Atomic {
 public:
  // barriers
  static void     loadload();
  static void     storestore();
  static void     loadstore();
  static void     storeload();

  static void     acquire();
  static void     release();
  static void     fence();

  template <typename T>
  static T        load_acquire(const volatile T* p);

  template <typename T, typename D>
  static void     release_store(volatile D* p, T v);

  template <typename T, typename D>
  static void     release_store_fence(volatile D* p, T v);

 private:
  // This is a helper that invokes the StubRoutines::fence_entry()
  // routine if it exists, It should only be used by platforms that
  // don't have another way to do the inline assembly.
  static void StubRoutines_fence();

  // Give platforms a variation point to specialize.
  template<size_t byte_size, ScopedFenceType type> struct PlatformOrderedStore;
  template<size_t byte_size, ScopedFenceType type> struct PlatformOrderedLoad;

  template<typename FieldType, ScopedFenceType FenceType>
  static void ordered_store(volatile FieldType* p, FieldType v);

  template<typename FieldType, ScopedFenceType FenceType>
  static FieldType ordered_load(const volatile FieldType* p);
};

// The following methods can be specialized using simple template specialization
// in the platform specific files for optimization purposes. Otherwise the
// generalized variant is used.

template<size_t byte_size, ScopedFenceType type>
struct OrderAccess::PlatformOrderedStore {
  template <typename T>
  void operator()(T v, volatile T* p) const {
    ordered_store<T, type>(p, v);
  }
};

template<size_t byte_size, ScopedFenceType type>
struct OrderAccess::PlatformOrderedLoad {
  template <typename T>
  T operator()(const volatile T* p) const {
    return ordered_load<T, type>(p);
  }
};

#include OS_CPU_HEADER(orderAccess)

template<> inline void ScopedFenceGeneral<X_ACQUIRE>::postfix()       { OrderAccess::acquire(); }
template<> inline void ScopedFenceGeneral<RELEASE_X>::prefix()        { OrderAccess::release(); }
template<> inline void ScopedFenceGeneral<RELEASE_X_FENCE>::prefix()  { OrderAccess::release(); }
template<> inline void ScopedFenceGeneral<RELEASE_X_FENCE>::postfix() { OrderAccess::fence();   }

template <typename FieldType, ScopedFenceType FenceType>
inline void OrderAccess::ordered_store(volatile FieldType* p, FieldType v) {
  ScopedFence<FenceType> f((void*)p);
  Atomic::store(v, p);
}

template <typename FieldType, ScopedFenceType FenceType>
inline FieldType OrderAccess::ordered_load(const volatile FieldType* p) {
  ScopedFence<FenceType> f((void*)p);
  return Atomic::load(p);
}

template <typename T>
inline T OrderAccess::load_acquire(const volatile T* p) {
  return LoadImpl<T, PlatformOrderedLoad<sizeof(T), X_ACQUIRE> >()(p);
}

template <typename T, typename D>
inline void OrderAccess::release_store(volatile D* p, T v) {
  StoreImpl<T, D, PlatformOrderedStore<sizeof(D), RELEASE_X> >()(v, p);
}

template <typename T, typename D>
inline void OrderAccess::release_store_fence(volatile D* p, T v) {
  StoreImpl<T, D, PlatformOrderedStore<sizeof(D), RELEASE_X_FENCE> >()(v, p);
}
#endif

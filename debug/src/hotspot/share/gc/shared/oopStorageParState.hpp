/*
 * Copyright (c) 2018, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#ifndef SHARE_GC_SHARED_OOPSTORAGEPARSTATE_HPP
#define SHARE_GC_SHARED_OOPSTORAGEPARSTATE_HPP

#include "gc/shared/oopStorage.hpp"
#include "utilities/macros.hpp"

//////////////////////////////////////////////////////////////////////////////
// Support for parallel and optionally concurrent state iteration.
//
// Parallel iteration is for the exclusive use of the GC.  Other iteration
// clients must use serial iteration.
//
// Concurrent Iteration
//
// Iteration involves the _active_array (an ActiveArray), which contains all
// of the blocks owned by a storage object.
//
// At most one concurrent ParState can exist at a time for a given storage
// object.
//
// A concurrent ParState sets the associated storage's
// _concurrent_iteration_active flag true when the state is constructed, and
// sets it false when the state is destroyed.  These assignments are made with
// _active_mutex locked.  Meanwhile, empty block deletion is not done while
// _concurrent_iteration_active is true.  The flag check and the dependent
// removal of a block from the _active_array is performed with _active_mutex
// locked.  This prevents concurrent iteration and empty block deletion from
// interfering with with each other.
//
// Both allocate() and delete_empty_blocks_concurrent() lock the
// _allocate_mutex while performing their respective list and array
// manipulations, preventing them from interfering with each other.
//
// When allocate() creates a new block, it is added to the end of the
// _active_array.  Then _active_array's _block_count is incremented to account
// for the new block.  When concurrent iteration is started (by a parallel
// worker thread calling the state's iterate() function), the current
// _active_array and its _block_count are captured for use by the iteration,
// with iteration processing all blocks in that array up to that block count.
//
// As a result, the sequence over which concurrent iteration operates is
// stable.  However, once the iteration is started, later allocations may add
// blocks to the end of the array that won't be examined by the iteration.
// An allocation may even require expansion of the array, so the iteration is
// no longer processing the current array, but rather the previous one.
// And while the sequence is stable, concurrent allocate() and release()
// operations may change the set of allocated entries in a block at any time
// during the iteration.
//
// As a result, a concurrent iteration handler must accept that some
// allocations and releases that occur after the iteration started will not be
// seen by the iteration.  Further, some may overlap examination by the
// iteration.  To help with this, allocate() and release() have an invariant
// that an entry's value must be NULL when it is not in use.
//
// An in-progress delete_empty_blocks_concurrent() operation can contend with
// the start of a concurrent iteration over the _active_mutex.  Since both are
// under GC control, that potential contention can be eliminated by never
// scheduling both operations to run at the same time.
//
// ParState<concurrent, is_const>
//   concurrent must be true if iteration is concurrent with the
//   mutator, false if iteration is at a safepoint.
//
//   is_const must be true if the iteration is over a constant storage
//   object, false if the iteration may modify the storage object.
//
// ParState([const] OopStorage* storage)
//   Construct an object for managing an iteration over storage.  For a
//   concurrent ParState, empty block deletion for the associated storage
//   is inhibited for the life of the ParState.  There can be no more
//   than one live concurrent ParState at a time for a given storage object.
//
// template<typename F> void iterate(F f)
//   Repeatedly claims a block from the associated storage that has
//   not been processed by this iteration (possibly by other threads),
//   and applies f to each entry in the claimed block. Assume p is of
//   type const oop* or oop*, according to is_const. Then f(p) must be
//   a valid expression whose value is ignored.  Concurrent uses must
//   be prepared for an entry's value to change at any time, due to
//   mutator activity.
//
// template<typename Closure> void oops_do(Closure* cl)
//   Wrapper around iterate, providing an adaptation layer allowing
//   the use of OopClosures and similar objects for iteration.  Assume
//   p is of type const oop* or oop*, according to is_const.  Then
//   cl->do_oop(p) must be a valid expression whose value is ignored.
//   Concurrent uses must be prepared for the entry's value to change
//   at any time, due to mutator activity.
//
// Optional operations, provided only if !concurrent && !is_const.
// These are not provided when is_const, because the storage object
// may be modified by the iteration infrastructure, even if the
// provided closure doesn't modify the storage object.  These are not
// provided when concurrent because any pre-filtering behavior by the
// iteration infrastructure is inappropriate for concurrent iteration;
// modifications of the storage by the mutator could result in the
// pre-filtering being applied (successfully or not) to objects that
// are unrelated to what the closure finds in the entry.
//
// template<typename Closure> void weak_oops_do(Closure* cl)
// template<typename IsAliveClosure, typename Closure>
// void weak_oops_do(IsAliveClosure* is_alive, Closure* cl)
//   Wrappers around iterate, providing an adaptation layer allowing
//   the use of is-alive closures and OopClosures for iteration.
//   Assume p is of type oop*.  Then
//
//   - cl->do_oop(p) must be a valid expression whose value is ignored.
//
//   - is_alive->do_object_b(*p) must be a valid expression whose value
//   is convertible to bool.
//
//   If *p == NULL then neither is_alive nor cl will be invoked for p.
//   If is_alive->do_object_b(*p) is false, then cl will not be
//   invoked on p.

class OopStorage::BasicParState {
  const OopStorage* _storage;
  ActiveArray* _active_array;
  size_t _block_count;
  volatile size_t _next_block;
  uint _estimated_thread_count;
  bool _concurrent;

  // Noncopyable.
  BasicParState(const BasicParState&);
  BasicParState& operator=(const BasicParState&);

  struct IterationData;

  void update_iteration_state(bool value);
  bool claim_next_segment(IterationData* data);
  bool finish_iteration(const IterationData* data) const;

  // Wrapper for iteration handler; ignore handler result and return true.
  template<typename F> class AlwaysTrueFn;

public:
  BasicParState(const OopStorage* storage,
                uint estimated_thread_count,
                bool concurrent);
  ~BasicParState();

  template<bool is_const, typename F> void iterate(F f);

  static uint default_estimated_thread_count(bool concurrent);
};

template<bool concurrent, bool is_const>
class OopStorage::ParState {
  BasicParState _basic_state;

  typedef typename Conditional<is_const,
                               const OopStorage*,
                               OopStorage*>::type StoragePtr;

public:
  ParState(StoragePtr storage,
           uint estimated_thread_count = BasicParState::default_estimated_thread_count(concurrent)) :
    _basic_state(storage, estimated_thread_count, concurrent)
  {}

  template<typename F> void iterate(F f);
  template<typename Closure> void oops_do(Closure* cl);
};

template<>
class OopStorage::ParState<false, false> {
  BasicParState _basic_state;

public:
  ParState(OopStorage* storage,
           uint estimated_thread_count = BasicParState::default_estimated_thread_count(false)) :
    _basic_state(storage, estimated_thread_count, false)
  {}

  template<typename F> void iterate(F f);
  template<typename Closure> void oops_do(Closure* cl);
  template<typename Closure> void weak_oops_do(Closure* cl);
  template<typename IsAliveClosure, typename Closure>
  void weak_oops_do(IsAliveClosure* is_alive, Closure* cl);
};

#endif // SHARE_GC_SHARED_OOPSTORAGEPARSTATE_HPP

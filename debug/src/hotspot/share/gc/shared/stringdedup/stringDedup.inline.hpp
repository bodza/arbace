#ifndef SHARE_VM_GC_SHARED_STRINGDEDUP_STRINGDEDUP_INLINE_HPP
#define SHARE_VM_GC_SHARED_STRINGDEDUP_STRINGDEDUP_INLINE_HPP

#include "gc/shared/stringdedup/stringDedup.hpp"
#include "gc/shared/stringdedup/stringDedupThread.inline.hpp"

template <typename Q, typename S>
void StringDedup::initialize_impl() {
  if (UseStringDeduplication) {
    _enabled = true;
    StringDedupQueue::create<Q>();
    StringDedupTable::create();
    StringDedupThreadImpl<S>::create();
  }
}

#endif

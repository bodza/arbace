#ifndef SHARE_VM_CODE_COMPILEDMETHOD_INLINE_HPP
#define SHARE_VM_CODE_COMPILEDMETHOD_INLINE_HPP

#include "code/compiledMethod.hpp"
#include "code/nativeInst.hpp"
#include "runtime/frame.hpp"
#include "runtime/orderAccess.hpp"

inline void CompiledMethod::release_set_exception_cache(ExceptionCache *ec) {
  OrderAccess::release_store(&_exception_cache, ec);
}

// class ExceptionCache methods

inline int ExceptionCache::count()            { return OrderAccess::load_acquire(&_count); }

address ExceptionCache::pc_at(int index)      { return _pc[index]; }
address ExceptionCache::handler_at(int index) { return _handler[index]; }

// increment_count is only called under lock, but there may be concurrent readers.
inline void ExceptionCache::increment_count() { OrderAccess::release_store(&_count, _count + 1); }

#endif

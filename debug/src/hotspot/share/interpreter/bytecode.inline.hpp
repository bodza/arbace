#ifndef SHARE_VM_INTERPRETER_BYTECODE_INLINE_HPP
#define SHARE_VM_INTERPRETER_BYTECODE_INLINE_HPP

#include "interpreter/bytecode.hpp"
#include "oops/cpCache.inline.hpp"

inline bool Bytecode_invoke::has_appendix() { return cpcache_entry()->has_appendix(); }

#endif

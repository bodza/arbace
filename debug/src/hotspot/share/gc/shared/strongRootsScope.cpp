#include "precompiled.hpp"
#include "classfile/stringTable.hpp"
#include "code/nmethod.hpp"
#include "gc/shared/strongRootsScope.hpp"
#include "runtime/thread.hpp"

MarkScope::MarkScope() {
  nmethod::oops_do_marking_prologue();
}

MarkScope::~MarkScope() {
  nmethod::oops_do_marking_epilogue();
}

StrongRootsScope::StrongRootsScope(uint n_threads) : _n_threads(n_threads) {
  Threads::change_thread_claim_parity();
}

StrongRootsScope::~StrongRootsScope() {
  Threads::assert_all_threads_claimed();
}

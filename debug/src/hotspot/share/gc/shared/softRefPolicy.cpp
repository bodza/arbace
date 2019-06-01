#include "precompiled.hpp"

#include "gc/shared/softRefPolicy.hpp"

SoftRefPolicy::SoftRefPolicy() :
    _should_clear_all_soft_refs(false),
    _all_soft_refs_clear(false) {
}

bool SoftRefPolicy::use_should_clear_all_soft_refs(bool v) {
  bool result = _should_clear_all_soft_refs;
  set_should_clear_all_soft_refs(false);
  return result;
}

void SoftRefPolicy::cleared_all_soft_refs() {
  _all_soft_refs_clear = true;
}

#include "precompiled.hpp"

#include "oops/markOop.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/objectMonitor.inline.hpp"

void markOopDesc::print_on(outputStream* st) const {
  if (is_marked()) {
    st->print(" marked(" INTPTR_FORMAT ")", value());
  } else if (has_monitor()) {
    // have to check has_monitor() before is_locked()
    st->print(" monitor(" INTPTR_FORMAT ")=", value());
    ObjectMonitor* mon = monitor();
    if (mon == NULL) {
      st->print("NULL (this should never be seen!)");
    } else {
      st->print("{count=0x%08x,waiters=0x%08x,recursions=" INTPTR_FORMAT ",owner=" INTPTR_FORMAT "}", mon->count(), mon->waiters(), mon->recursions(), p2i(mon->owner()));
    }
  } else if (is_locked()) {
    st->print(" locked(" INTPTR_FORMAT ")->", value());
    if (is_neutral()) {
      st->print("is_neutral");
      if (has_no_hash()) {
        st->print(" no_hash");
      } else {
        st->print(" hash=" INTPTR_FORMAT, hash());
      }
      st->print(" age=%d", age());
    } else if (has_bias_pattern()) {
      st->print("is_biased");
      JavaThread* jt = biased_locker();
      st->print(" biased_locker=" INTPTR_FORMAT, p2i(jt));
    } else {
      st->print("??");
    }
  } else {
    st->print("mark(");
    if (has_bias_pattern()) st->print("biased,");
    st->print("hash " INTPTR_FORMAT ",", hash());
    st->print("age %d)", age());
  }
}

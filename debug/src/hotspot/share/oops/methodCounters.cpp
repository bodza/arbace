#include "precompiled.hpp"

#include "memory/metaspaceClosure.hpp"
#include "oops/methodCounters.hpp"
#include "runtime/handles.inline.hpp"

MethodCounters* MethodCounters::allocate(const methodHandle& mh, TRAPS) {
  ClassLoaderData* loader_data = mh->method_holder()->class_loader_data();
  return new(loader_data, method_counters_size(), MetaspaceObj::MethodCountersType, THREAD) MethodCounters(mh);
}

void MethodCounters::clear_counters() {
  invocation_counter()->reset();
  backedge_counter()->reset();
  set_interpreter_throwout_count(0);
  set_interpreter_invocation_count(0);
  set_nmethod_age(INT_MAX);
}

int MethodCounters::highest_comp_level() const {
  return CompLevel_none;
}

void MethodCounters::set_highest_comp_level(int level) { }

void MethodCounters::metaspace_pointers_do(MetaspaceClosure* it) { }

void MethodCounters::print_value_on(outputStream* st) const {
  st->print("method counters");
  print_address_on(st);
}

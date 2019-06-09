#include "precompiled.hpp"

#include "oops/compiledICHolder.hpp"
#include "runtime/atomic.hpp"

volatile int CompiledICHolder::_live_count;
volatile int CompiledICHolder::_live_not_claimed_count;

CompiledICHolder::CompiledICHolder(Metadata* metadata, Klass* klass, bool is_method)
  : _holder_metadata(metadata), _holder_klass(klass), _is_metadata_method(is_method) {
}

// Printing

void CompiledICHolder::print_on(outputStream* st) const {
  st->print("%s", internal_name());
  st->print(" - metadata: "); holder_metadata()->print_value_on(st); st->cr();
  st->print(" - klass:    "); holder_klass()->print_value_on(st); st->cr();
}

void CompiledICHolder::print_value_on(outputStream* st) const {
  st->print("%s", internal_name());
}

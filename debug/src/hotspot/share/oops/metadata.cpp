#include "precompiled.hpp"

#include "oops/metadata.hpp"
#include "memory/metaspace.hpp"
#include "memory/resourceArea.hpp"

void Metadata::set_on_stack(const bool value) {
  // nothing to set for most metadata
  // Can't inline because this materializes the vtable on some C++ compilers.
}

void Metadata::print_on(outputStream* st) const {
  ResourceMark rm;
  // print title
  st->print("%s", internal_name());
  print_address_on(st);
  st->cr();
}

char* Metadata::print_value_string() const {
  char buf[256];
  stringStream st(buf, sizeof(buf));
  if (this == NULL) {
    st.print("NULL");
  } else {
    print_value_on(&st);
  }
  return st.as_string();
}

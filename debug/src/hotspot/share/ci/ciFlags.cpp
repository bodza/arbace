#include "precompiled.hpp"

#include "ci/ciFlags.hpp"

// ciFlags
//
// This class represents klass or method flags

// ------------------------------------------------------------------
// ciFlags::print_klass_flags
void ciFlags::print_klass_flags(outputStream* st) {
  if (is_public()) {
    st->print("public");
  } else {
    st->print("DEFAULT_ACCESS");
  }

  if (is_final()) {
    st->print(",final");
  }
  if (is_super()) {
    st->print(",super");
  }
  if (is_interface()) {
    st->print(",interface");
  }
  if (is_abstract()) {
    st->print(",abstract");
  }
}

// ------------------------------------------------------------------
// ciFlags::print_member_flags
void ciFlags::print_member_flags(outputStream* st) {
  if (is_public()) {
    st->print("public");
  } else if (is_private()) {
    st->print("private");
  } else if (is_protected()) {
    st->print("protected");
  } else {
    st->print("DEFAULT_ACCESS");
  }

  if (is_static()) {
    st->print(",static");
  }
  if (is_final()) {
    st->print(",final");
  }
  if (is_synchronized()) {
    st->print(",synchronized");
  }
  if (is_volatile()) {
    st->print(",volatile");
  }
  if (is_transient()) {
    st->print(",transient");
  }
  if (is_native()) {
    st->print(",native");
  }
  if (is_abstract()) {
    st->print(",abstract");
  }
  if (is_strict()) {
    st->print(",strict");
  }
}

// ------------------------------------------------------------------
// ciFlags::print
void ciFlags::print(outputStream* st) {
  st->print(" flags=%x", _flags);
}

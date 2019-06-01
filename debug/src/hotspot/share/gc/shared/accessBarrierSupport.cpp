#include "precompiled.hpp"

#include "classfile/javaClasses.inline.hpp"
#include "gc/shared/accessBarrierSupport.inline.hpp"
#include "oops/access.hpp"

DecoratorSet AccessBarrierSupport::resolve_unknown_oop_ref_strength(DecoratorSet decorators, oop base, ptrdiff_t offset) {
  DecoratorSet ds = decorators & ~ON_UNKNOWN_OOP_REF;
  if (!java_lang_ref_Reference::is_referent_field(base, offset)) {
    ds |= ON_STRONG_OOP_REF;
  } else if (java_lang_ref_Reference::is_phantom(base)) {
    ds |= ON_PHANTOM_OOP_REF;
  } else {
    ds |= ON_WEAK_OOP_REF;
  }
  return ds;
}

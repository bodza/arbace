#include "precompiled.hpp"

#include "memory/iterator.inline.hpp"
#include "memory/universe.hpp"
#include "oops/oop.inline.hpp"
#include "utilities/debug.hpp"
#include "utilities/globalDefinitions.hpp"

DoNothingClosure do_nothing_cl;

void CLDToOopClosure::do_cld(ClassLoaderData* cld) {
  cld->oops_do(_oop_closure, _must_claim_cld);
}

void ObjectToOopClosure::do_object(oop obj) {
  obj->oop_iterate(_cl);
}

void VoidClosure::do_void() {
  ShouldNotCallThis();
}

void CodeBlobToOopClosure::do_nmethod(nmethod* nm) {
  nm->oops_do(_cl);
  if (_fix_relocations) {
    nm->fix_oop_relocations();
  }
}

void CodeBlobToOopClosure::do_code_blob(CodeBlob* cb) {
  nmethod* nm = cb->as_nmethod_or_null();
  if (nm != NULL) {
    do_nmethod(nm);
  }
}

void MarkingCodeBlobClosure::do_code_blob(CodeBlob* cb) {
  nmethod* nm = cb->as_nmethod_or_null();
  if (nm != NULL && !nm->test_set_oops_do_mark()) {
    do_nmethod(nm);
  }
}

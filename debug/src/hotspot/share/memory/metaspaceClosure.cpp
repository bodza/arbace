#include "precompiled.hpp"
#include "memory/metaspaceClosure.hpp"

// Update the reference to point to new_loc.
void MetaspaceClosure::Ref::update(address new_loc) const {
  log_trace(cds)("Ref: [" PTR_FORMAT "] -> " PTR_FORMAT " => " PTR_FORMAT,
                 p2i(mpp()), p2i(obj()), p2i(new_loc));
  uintx p = (uintx)new_loc;
  p |= flag_bits(); // Make sure the flag bits are copied to the new pointer.
  *(address*)mpp() = (address)p;
}

void MetaspaceClosure::push_impl(MetaspaceClosure::Ref* ref, Writability w) {
  if (ref->not_null()) {
    bool read_only;
    switch (w) {
    case _writable:
      read_only = false;
      break;
    case _not_writable:
      read_only = true;
      break;
    default:
      read_only = ref->is_read_only_by_default();
    }
    if (do_ref(ref, read_only)) { // true means we want to iterate the embedded pointer in <ref>
      ref->metaspace_pointers_do(this);
    }
  }
}

bool UniqueMetaspaceClosure::do_ref(MetaspaceClosure::Ref* ref, bool read_only) {
  bool* found = _has_been_visited.get(ref->obj());
  if (found != NULL) {
    return false; // Already visited: no need to iterate embedded pointers.
  } else {
    bool isnew = _has_been_visited.put(ref->obj(), read_only);
    do_unique_ref(ref, read_only);
    return true;  // Saw this for the first time: iterate the embedded pointers.
  }
}

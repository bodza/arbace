#ifndef SHARE_VM_GC_SHARED_REFERENCEPROCESSOR_INLINE_HPP
#define SHARE_VM_GC_SHARED_REFERENCEPROCESSOR_INLINE_HPP

#include "gc/shared/referenceProcessor.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.hpp"

oop DiscoveredList::head() const {
  return UseCompressedOops ?  CompressedOops::decode(_compressed_head) :
    _oop_head;
}

void DiscoveredList::set_head(oop o) {
  if (UseCompressedOops) {
    // Must compress the head ptr.
    _compressed_head = CompressedOops::encode(o);
  } else {
    _oop_head = o;
  }
}

bool DiscoveredList::is_empty() const {
 return head() == NULL;
}

void DiscoveredList::clear() {
  set_head(NULL);
  set_length(0);
}

DiscoveredListIterator::DiscoveredListIterator(DiscoveredList& refs_list, OopClosure* keep_alive, BoolObjectClosure* is_alive) :
  _refs_list(refs_list),
  _prev_discovered_addr(refs_list.adr_head()),
  _prev_discovered(NULL),
  _current_discovered(refs_list.head()),
  _processed(0),
  _removed(0),
  _next_discovered(NULL),
  _keep_alive(keep_alive),
  _is_alive(is_alive) {
}

#endif

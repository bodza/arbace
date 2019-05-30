#ifndef SHARE_VM_CLASSFILE_DICTIONARY_INLINE_HPP
#define SHARE_VM_CLASSFILE_DICTIONARY_INLINE_HPP

#include "classfile/dictionary.hpp"
#include "runtime/orderAccess.hpp"

inline ProtectionDomainEntry* DictionaryEntry::pd_set_acquire() const {
  return OrderAccess::load_acquire(&_pd_set);
}

inline void DictionaryEntry::release_set_pd_set(ProtectionDomainEntry* new_head) {
  OrderAccess::release_store(&_pd_set, new_head);
}

#endif

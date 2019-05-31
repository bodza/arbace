#ifndef SHARE_VM_CLASSFILE_COMPACTHASHTABLE_INLINE_HPP
#define SHARE_VM_CLASSFILE_COMPACTHASHTABLE_INLINE_HPP

#include "classfile/compactHashtable.hpp"
#include "classfile/javaClasses.hpp"
#include "memory/allocation.inline.hpp"
#include "oops/compressedOops.inline.hpp"
#include "oops/oop.hpp"

template <class T, class N>
inline Symbol* CompactHashtable<T, N>::decode_entry(CompactHashtable<Symbol*, char>* const t,
                                                    u4 offset, const char* name, int len) {
  Symbol* sym = (Symbol*)(_base_address + offset);
  if (sym->equals(name, len)) {
    return sym;
  }

  return NULL;
}

template <class T, class N>
inline oop CompactHashtable<T, N>::decode_entry(CompactHashtable<oop, char>* const t,
                                                u4 offset, const char* name, int len) {
  narrowOop obj = (narrowOop)offset;
  oop string = CompressedOops::decode(obj);
  if (java_lang_String::equals(string, (jchar*)name, len)) {
    return string;
  }

  return NULL;
}

template <class T, class N>
inline T CompactHashtable<T,N>::lookup(const N* name, unsigned int hash, int len) {
  if (_entry_count > 0) {
    int index = hash % _bucket_count;
    u4 bucket_info = _buckets[index];
    u4 bucket_offset = BUCKET_OFFSET(bucket_info);
    int bucket_type = BUCKET_TYPE(bucket_info);
    u4* entry = _entries + bucket_offset;

    if (bucket_type == VALUE_ONLY_BUCKET_TYPE) {
      T res = decode_entry(this, entry[0], name, len);
      if (res != NULL) {
        return res;
      }
    } else {
      // This is a regular bucket, which has more than one
      // entries. Each entry is a pair of entry (hash, offset).
      // Seek until the end of the bucket.
      u4* entry_max = _entries + BUCKET_OFFSET(_buckets[index + 1]);
      while (entry < entry_max) {
        unsigned int h = (unsigned int)(entry[0]);
        if (h == hash) {
          T res = decode_entry(this, entry[1], name, len);
          if (res != NULL) {
            return res;
          }
        }
        entry += 2;
      }
    }
  }
  return NULL;
}

#endif

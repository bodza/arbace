#ifndef SHARE_VM_CLASSFILE_METADATAONSTACKMARK_HPP
#define SHARE_VM_CLASSFILE_METADATAONSTACKMARK_HPP

#include "memory/allocation.hpp"
#include "utilities/chunkedList.hpp"

class Metadata;

typedef ChunkedList<Metadata*, mtInternal> MetadataOnStackBuffer;

// Helper class to mark and unmark metadata used on the stack as either handles
// or executing methods, so that it can't be deleted during class redefinition
// and class unloading.
// This is also used for other things that can be deallocated, like class
// metadata during parsing if errors occur, relocated methods, and temporary
// constant pools.
class MetadataOnStackMark : public StackObj {
  static MetadataOnStackBuffer* _used_buffers;
  static MetadataOnStackBuffer* _free_buffers;
  static MetadataOnStackBuffer* _current_buffer;

  static MetadataOnStackBuffer* allocate_buffer();
  static void retire_buffer(MetadataOnStackBuffer* buffer);

 public:
  MetadataOnStackMark(bool redefinition_walk);
   ~MetadataOnStackMark();

  static void record(Metadata* m);
  static void retire_current_buffer();
};

#endif

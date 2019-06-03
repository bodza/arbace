#include "precompiled.hpp"

#include "classfile/metadataOnStackMark.hpp"
#include "code/codeCache.hpp"
#include "compiler/compileBroker.hpp"
#include "oops/metadata.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/thread.hpp"
#include "services/threadService.hpp"
#include "utilities/chunkedList.hpp"
#include "jvmci/jvmciRuntime.hpp"

MetadataOnStackBuffer* MetadataOnStackMark::_used_buffers = NULL;
MetadataOnStackBuffer* MetadataOnStackMark::_free_buffers = NULL;

MetadataOnStackBuffer* MetadataOnStackMark::_current_buffer = NULL;

// Walk metadata on the stack and mark it so that redefinition doesn't delete
// it.  Class unloading only deletes in-error class files, methods created by
// the relocator and dummy constant pools.  None of these appear anywhere except
// in metadata Handles.
MetadataOnStackMark::MetadataOnStackMark(bool redefinition_walk) {
  Threads::metadata_handles_do(Metadata::mark_on_stack);

  if (redefinition_walk) {
    Threads::metadata_do(Metadata::mark_on_stack);
    CodeCache::metadata_do(Metadata::mark_on_stack);
    CompileBroker::mark_on_stack();
    ThreadService::metadata_do(Metadata::mark_on_stack);
    JVMCIRuntime::metadata_do(Metadata::mark_on_stack);
  }
}

MetadataOnStackMark::~MetadataOnStackMark() {
  // Unmark everything that was marked.   Can't do the same walk because
  // redefine classes messes up the code cache so the set of methods
  // might not be the same.
  retire_current_buffer();

  MetadataOnStackBuffer* buffer = _used_buffers;
  while (buffer != NULL) {
    // Clear on stack state for all metadata.
    size_t size = buffer->size();
    for (size_t i = 0; i < size; i++) {
      Metadata* md = buffer->at(i);
      md->set_on_stack(false);
    }

    MetadataOnStackBuffer* next = buffer->next_used();

    // Move the buffer to the free list.
    buffer->clear();
    buffer->set_next_used(NULL);
    buffer->set_next_free(_free_buffers);
    _free_buffers = buffer;

    // Step to next used buffer.
    buffer = next;
  }

  _used_buffers = NULL;
}

void MetadataOnStackMark::retire_buffer(MetadataOnStackBuffer* buffer) {
  if (buffer == NULL) {
    return;
  }
  buffer->set_next_used(_used_buffers);
  _used_buffers = buffer;
}

// Current buffer is full or we're ready to walk them, add it to the used list.
void MetadataOnStackMark::retire_current_buffer() {
  retire_buffer(_current_buffer);
  _current_buffer = NULL;
}

// Get buffer off free list.
MetadataOnStackBuffer* MetadataOnStackMark::allocate_buffer() {
  MetadataOnStackBuffer* allocated = _free_buffers;

  if (allocated != NULL) {
    _free_buffers = allocated->next_free();
  }

  if (allocated == NULL) {
    allocated = new MetadataOnStackBuffer();
  }

  return allocated;
}

// Record which objects are marked so we can unmark the same objects.
void MetadataOnStackMark::record(Metadata* m) {
  MetadataOnStackBuffer* buffer = _current_buffer;

  if (buffer != NULL && buffer->is_full()) {
    retire_buffer(buffer);
    buffer = NULL;
  }

  if (buffer == NULL) {
    buffer = allocate_buffer();
    _current_buffer = buffer;
  }

  buffer->push(m);
}

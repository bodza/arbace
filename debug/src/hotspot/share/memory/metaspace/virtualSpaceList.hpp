#ifndef SHARE_MEMORY_METASPACE_VIRTUALSPACELIST_HPP
#define SHARE_MEMORY_METASPACE_VIRTUALSPACELIST_HPP

#include "memory/allocation.hpp"
#include "memory/metaspace/virtualSpaceNode.hpp"
#include "utilities/globalDefinitions.hpp"

namespace metaspace {
class Metachunk;
class ChunkManager;

// List of VirtualSpaces for metadata allocation.
class VirtualSpaceList : public CHeapObj<mtClass> {
  friend class VirtualSpaceNode;

  enum VirtualSpaceSizes {
    VirtualSpaceSize = 256 * K
  };

  // Head of the list
  VirtualSpaceNode* _virtual_space_list;
  // virtual space currently being used for allocations
  VirtualSpaceNode* _current_virtual_space;

  // Is this VirtualSpaceList used for the compressed class space
  bool _is_class;

  // Sum of reserved and committed memory in the virtual spaces
  size_t _reserved_words;
  size_t _committed_words;

  // Number of virtual spaces
  size_t _virtual_space_count;

  ~VirtualSpaceList();

  VirtualSpaceNode* virtual_space_list() const { return _virtual_space_list; }

  void set_virtual_space_list(VirtualSpaceNode* v) {
    _virtual_space_list = v;
  }
  void set_current_virtual_space(VirtualSpaceNode* v) {
    _current_virtual_space = v;
  }

  void link_vs(VirtualSpaceNode* new_entry);

  // Get another virtual space and add it to the list.  This
  // is typically prompted by a failed attempt to allocate a chunk
  // and is typically followed by the allocation of a chunk.
  bool create_new_virtual_space(size_t vs_word_size);

  // Chunk up the unused committed space in the current
  // virtual space and add the chunks to the free list.
  void retire_current_virtual_space();

 public:
  VirtualSpaceList(size_t word_size);
  VirtualSpaceList(ReservedSpace rs);

  size_t free_bytes();

  Metachunk* get_new_chunk(size_t chunk_word_size,
                           size_t suggested_commit_granularity);

  bool expand_node_by(VirtualSpaceNode* node,
                      size_t min_words,
                      size_t preferred_words);

  bool expand_by(size_t min_words,
                 size_t preferred_words);

  VirtualSpaceNode* current_virtual_space() {
    return _current_virtual_space;
  }

  bool is_class() const { return _is_class; }

  bool initialization_succeeded() { return _virtual_space_list != NULL; }

  size_t reserved_words()  { return _reserved_words; }
  size_t reserved_bytes()  { return reserved_words() * BytesPerWord; }
  size_t committed_words() { return _committed_words; }
  size_t committed_bytes() { return committed_words() * BytesPerWord; }

  void inc_reserved_words(size_t v);
  void dec_reserved_words(size_t v);
  void inc_committed_words(size_t v);
  void dec_committed_words(size_t v);
  void inc_virtual_space_count();
  void dec_virtual_space_count();

  VirtualSpaceNode* find_enclosing_space(const void* ptr);
  bool contains(const void* ptr) { return find_enclosing_space(ptr) != NULL; }

  // Unlink empty VirtualSpaceNodes and free it.
  void purge(ChunkManager* chunk_manager);

  void print_on(outputStream* st) const                 { print_on(st, K); }
  void print_on(outputStream* st, size_t scale) const;
  void print_map(outputStream* st) const;

  class VirtualSpaceListIterator : public StackObj {
    VirtualSpaceNode* _virtual_spaces;
   public:
    VirtualSpaceListIterator(VirtualSpaceNode* virtual_spaces) :
      _virtual_spaces(virtual_spaces) { }

    bool repeat() {
      return _virtual_spaces != NULL;
    }

    VirtualSpaceNode* get_next() {
      VirtualSpaceNode* result = _virtual_spaces;
      if (_virtual_spaces != NULL) {
        _virtual_spaces = _virtual_spaces->next();
      }
      return result;
    }
  };
};
}

#endif

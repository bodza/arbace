#ifndef SHARE_VM_GC_SHARED_CARDGENERATION_HPP
#define SHARE_VM_GC_SHARED_CARDGENERATION_HPP

// Class CardGeneration is a generation that is covered by a card table,
// and uses a card-size block-offset array to implement block_start.

#include "gc/shared/generation.hpp"

class BlockOffsetSharedArray;
class CardTableRS;
class CompactibleSpace;

class CardGeneration: public Generation {
  friend class VMStructs;
 protected:
  // This is shared with other generations.
  CardTableRS* _rs;
  // This is local to this generation.
  BlockOffsetSharedArray* _bts;

  // Current shrinking effect: this damps shrinking when the heap gets empty.
  size_t _shrink_factor;

  size_t _min_heap_delta_bytes;   // Minimum amount to expand.

  // Some statistics from before gc started.
  // These are gathered in the gc_prologue (and should_collect)
  // to control growing/shrinking policy in spite of promotions.
  size_t _capacity_at_prologue;
  size_t _used_at_prologue;

  CardGeneration(ReservedSpace rs, size_t initial_byte_size, CardTableRS* remset);

  virtual void assert_correct_size_change_locking() = 0;

  virtual CompactibleSpace* space() const = 0;

 public:

  // Attempt to expand the generation by "bytes".  Expand by at a
  // minimum "expand_bytes".  Return true if some amount (not
  // necessarily the full "bytes") was done.
  virtual bool expand(size_t bytes, size_t expand_bytes);

  // Shrink generation with specified size
  virtual void shrink(size_t bytes);

  virtual void compute_new_size();

  virtual void clear_remembered_set();

  virtual void invalidate_remembered_set();

  virtual void prepare_for_verify();

  // Grow generation with specified size (returns false if unable to grow)
  bool grow_by(size_t bytes);
  // Grow generation to reserved size.
  bool grow_to_reserved();

  size_t capacity() const;
  size_t used() const;
  size_t free() const;
  MemRegion used_region() const;

  void space_iterate(SpaceClosure* blk, bool usedOnly = false);

  void younger_refs_iterate(OopsInGenClosure* blk, uint n_threads);

  bool is_in(const void* p) const;

  CompactibleSpace* first_compaction_space() const;
};

#endif

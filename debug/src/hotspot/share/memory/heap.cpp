#include "precompiled.hpp"
#include "memory/heap.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/os.hpp"
#include "services/memTracker.hpp"
#include "utilities/align.hpp"

size_t CodeHeap::header_size() {
  return sizeof(HeapBlock);
}

// Implementation of Heap

CodeHeap::CodeHeap(const char* name, const int code_blob_type)
  : _code_blob_type(code_blob_type) {
  _name                         = name;
  _number_of_committed_segments = 0;
  _number_of_reserved_segments  = 0;
  _segment_size                 = 0;
  _log2_segment_size            = 0;
  _next_segment                 = 0;
  _freelist                     = NULL;
  _freelist_segments            = 0;
  _freelist_length              = 0;
  _max_allocated_capacity       = 0;
  _blob_count                   = 0;
  _nmethod_count                = 0;
  _adapter_count                = 0;
  _full_count                   = 0;
}

void CodeHeap::mark_segmap_as_free(size_t beg, size_t end) {
  // setup _segmap pointers for faster indexing
  address p = (address)_segmap.low() + beg;
  address q = (address)_segmap.low() + end;
  // initialize interval
  while (p < q) *p++ = free_sentinel;
}

void CodeHeap::mark_segmap_as_used(size_t beg, size_t end) {
  // setup _segmap pointers for faster indexing
  address p = (address)_segmap.low() + beg;
  address q = (address)_segmap.low() + end;
  // initialize interval
  int i = 0;
  while (p < q) {
    *p++ = i++;
    if (i == free_sentinel) i = 1;
  }
}

static size_t align_to_page_size(size_t size) {
  const size_t alignment = (size_t)os::vm_page_size();
  return (size + alignment - 1) & ~(alignment - 1);
}

void CodeHeap::on_code_mapping(char* base, size_t size) {
#ifdef LINUX
  extern void linux_wrap_code(char* base, size_t size);
  linux_wrap_code(base, size);
#endif
}

bool CodeHeap::reserve(ReservedSpace rs, size_t committed_size, size_t segment_size) {

  _segment_size      = segment_size;
  _log2_segment_size = exact_log2(segment_size);

  // Reserve and initialize space for _memory.
  size_t page_size = os::vm_page_size();
  if (os::can_execute_large_page_memory()) {
    const size_t min_pages = 8;
    page_size = MIN2(os::page_size_for_region_aligned(committed_size, min_pages),
                     os::page_size_for_region_aligned(rs.size(), min_pages));
  }

  const size_t granularity = os::vm_allocation_granularity();
  const size_t c_size = align_up(committed_size, page_size);

  os::trace_page_sizes(_name, committed_size, rs.size(), page_size,
                       rs.base(), rs.size());
  if (!_memory.initialize(rs, c_size)) {
    return false;
  }

  on_code_mapping(_memory.low(), _memory.committed_size());
  _number_of_committed_segments = size_to_segments(_memory.committed_size());
  _number_of_reserved_segments  = size_to_segments(_memory.reserved_size());
  const size_t reserved_segments_alignment = MAX2((size_t)os::vm_page_size(), granularity);
  const size_t reserved_segments_size = align_up(_number_of_reserved_segments, reserved_segments_alignment);
  const size_t committed_segments_size = align_to_page_size(_number_of_committed_segments);

  // reserve space for _segmap
  if (!_segmap.initialize(reserved_segments_size, committed_segments_size)) {
    return false;
  }

  MemTracker::record_virtual_memory_type((address)_segmap.low_boundary(), mtCode);

  // initialize remaining instance variables
  clear();
  return true;
}

bool CodeHeap::expand_by(size_t size) {
  // expand _memory space
  size_t dm = align_to_page_size(_memory.committed_size() + size) - _memory.committed_size();
  if (dm > 0) {
    // Use at least the available uncommitted space if 'size' is larger
    if (_memory.uncommitted_size() != 0 && dm > _memory.uncommitted_size()) {
      dm = _memory.uncommitted_size();
    }
    char* base = _memory.low() + _memory.committed_size();
    if (!_memory.expand_by(dm)) return false;
    on_code_mapping(base, dm);
    size_t i = _number_of_committed_segments;
    _number_of_committed_segments = size_to_segments(_memory.committed_size());
    // expand _segmap space
    size_t ds = align_to_page_size(_number_of_committed_segments) - _segmap.committed_size();
    if ((ds > 0) && !_segmap.expand_by(ds)) {
      return false;
    }
    // initialize additional segmap entries
    mark_segmap_as_free(i, _number_of_committed_segments);
  }
  return true;
}

void CodeHeap::clear() {
  _next_segment = 0;
  mark_segmap_as_free(0, _number_of_committed_segments);
}

void* CodeHeap::allocate(size_t instance_size) {
  size_t number_of_segments = size_to_segments(instance_size + header_size());

  // First check if we can satisfy request from freelist
  HeapBlock* block = search_freelist(number_of_segments);

  if (block != NULL) {
    guarantee((char*) block >= _memory.low_boundary() && (char*) block < _memory.high(),
              "The newly allocated block " INTPTR_FORMAT " is not within the heap "
              "starting with "  INTPTR_FORMAT " and ending with "  INTPTR_FORMAT,
              p2i(block), p2i(_memory.low_boundary()), p2i(_memory.high()));
    _max_allocated_capacity = MAX2(_max_allocated_capacity, allocated_capacity());
    _blob_count++;
    return block->allocated_space();
  }

  // Ensure minimum size for allocation to the heap.
  number_of_segments = MAX2((int)CodeCacheMinBlockLength, (int)number_of_segments);

  if (_next_segment + number_of_segments <= _number_of_committed_segments) {
    mark_segmap_as_used(_next_segment, _next_segment + number_of_segments);
    HeapBlock* b =  block_at(_next_segment);
    b->initialize(number_of_segments);
    _next_segment += number_of_segments;
    guarantee((char*) b >= _memory.low_boundary() && (char*) block < _memory.high(),
              "The newly allocated block " INTPTR_FORMAT " is not within the heap "
              "starting with "  INTPTR_FORMAT " and ending with " INTPTR_FORMAT,
              p2i(b), p2i(_memory.low_boundary()), p2i(_memory.high()));
    _max_allocated_capacity = MAX2(_max_allocated_capacity, allocated_capacity());
    _blob_count++;
    return b->allocated_space();
  } else {
    return NULL;
  }
}

void CodeHeap::deallocate_tail(void* p, size_t used_size) {
  // Find start of HeapBlock
  HeapBlock* b = (((HeapBlock *)p) - 1);
  size_t used_number_of_segments = size_to_segments(used_size + header_size());
  size_t actual_number_of_segments = b->length();
  guarantee(used_number_of_segments <= actual_number_of_segments, "Must be!");
  guarantee(b == block_at(_next_segment - actual_number_of_segments), "Intermediate allocation!");
  size_t number_of_segments_to_deallocate = actual_number_of_segments - used_number_of_segments;
  _next_segment -= number_of_segments_to_deallocate;
  mark_segmap_as_free(_next_segment, _next_segment + number_of_segments_to_deallocate);
  b->initialize(used_number_of_segments);
}

void CodeHeap::deallocate(void* p) {
  // Find start of HeapBlock
  HeapBlock* b = (((HeapBlock *)p) - 1);
  guarantee((char*) b >= _memory.low_boundary() && (char*) b < _memory.high(),
            "The block to be deallocated " INTPTR_FORMAT " is not within the heap "
            "starting with "  INTPTR_FORMAT " and ending with " INTPTR_FORMAT,
            p2i(b), p2i(_memory.low_boundary()), p2i(_memory.high()));
  add_to_freelist(b);
}

/**
 * Uses segment map to find the the start (header) of a nmethod. This works as follows:
 * The memory of the code cache is divided into 'segments'. The size of a segment is
 * determined by -XX:CodeCacheSegmentSize=XX. Allocation in the code cache can only
 * happen at segment boundaries. A pointer in the code cache can be mapped to a segment
 * by calling segment_for(addr). Each time memory is requested from the code cache,
 * the segmap is updated accordingly. See the following example, which illustrates the
 * state of code cache and the segment map: (seg -> segment, nm ->nmethod)
 *
 *          code cache          segmap
 *         -----------        ---------
 * seg 1   | nm 1    |   ->   | 0     |
 * seg 2   | nm 1    |   ->   | 1     |
 * ...     | nm 1    |   ->   | ..    |
 * seg m   | nm 2    |   ->   | 0     |
 * seg m+1 | nm 2    |   ->   | 1     |
 * ...     | nm 2    |   ->   | 2     |
 * ...     | nm 2    |   ->   | ..    |
 * ...     | nm 2    |   ->   | 0xFE  |
 * seg m+n | nm 2    |   ->   | 1     |
 * ...     | nm 2    |   ->   |       |
 *
 * A value of '0' in the segmap indicates that this segment contains the beginning of
 * an nmethod. Let's walk through a simple example: If we want to find the start of
 * an nmethod that falls into seg 2, we read the value of the segmap[2]. The value
 * is an offset that points to the segment that contains the start of the nmethod.
 * Another example: If we want to get the start of nm 2, and we happen to get a pointer
 * that points to seg m+n, we first read seg[n+m], which returns '1'. So we have to
 * do one more read of the segmap[m+n-1] to finally get the segment header.
 */
void* CodeHeap::find_start(void* p) const {
  if (!contains(p)) {
    return NULL;
  }
  size_t seg_idx = segment_for(p);
  address seg_map = (address)_segmap.low();
  if (is_segment_unused(seg_map[seg_idx])) {
    return NULL;
  }
  while (seg_map[seg_idx] > 0) {
    seg_idx -= (int)seg_map[seg_idx];
  }

  HeapBlock* h = block_at(seg_idx);
  if (h->free()) {
    return NULL;
  }
  return h->allocated_space();
}

CodeBlob* CodeHeap::find_blob_unsafe(void* start) const {
  CodeBlob* result = (CodeBlob*)CodeHeap::find_start(start);
  if (result != NULL && result->blob_contains((address)start)) {
    return result;
  }
  return NULL;
}

size_t CodeHeap::alignment_unit() const {
  // this will be a power of two
  return _segment_size;
}

size_t CodeHeap::alignment_offset() const {
  // The lowest address in any allocated block will be
  // equal to alignment_offset (mod alignment_unit).
  return sizeof(HeapBlock) & (_segment_size - 1);
}

// Returns the current block if available and used.
// If not, it returns the subsequent block (if available), NULL otherwise.
// Free blocks are merged, therefore there is at most one free block
// between two used ones. As a result, the subsequent block (if available) is
// guaranteed to be used.
void* CodeHeap::next_used(HeapBlock* b) const {
  if (b != NULL && b->free()) b = next_block(b);
  return (b == NULL) ? NULL : b->allocated_space();
}

// Returns the first used HeapBlock
HeapBlock* CodeHeap::first_block() const {
  if (_next_segment > 0)
    return block_at(0);
  return NULL;
}

HeapBlock* CodeHeap::block_start(void* q) const {
  HeapBlock* b = (HeapBlock*)find_start(q);
  if (b == NULL) return NULL;
  return b - 1;
}

// Returns the next Heap block an offset into one
HeapBlock* CodeHeap::next_block(HeapBlock *b) const {
  if (b == NULL) return NULL;
  size_t i = segment_for(b) + b->length();
  if (i < _next_segment)
    return block_at(i);
  return NULL;
}

// Returns current capacity
size_t CodeHeap::capacity() const {
  return _memory.committed_size();
}

size_t CodeHeap::max_capacity() const {
  return _memory.reserved_size();
}

int CodeHeap::allocated_segments() const {
  return (int)_next_segment;
}

size_t CodeHeap::allocated_capacity() const {
  // size of used heap - size on freelist
  return segments_to_size(_next_segment - _freelist_segments);
}

// Returns size of the unallocated heap block
size_t CodeHeap::heap_unallocated_capacity() const {
  // Total number of segments - number currently used
  return segments_to_size(_number_of_reserved_segments - _next_segment);
}

// Free list management

FreeBlock* CodeHeap::following_block(FreeBlock *b) {
  return (FreeBlock*)(((address)b) + _segment_size * b->length());
}

// Inserts block b after a
void CodeHeap::insert_after(FreeBlock* a, FreeBlock* b) {

  // Link b into the list after a
  b->set_link(a->link());
  a->set_link(b);

  // See if we can merge blocks
  merge_right(b); // Try to make b bigger
  merge_right(a); // Try to make a include b
}

// Try to merge this block with the following block
bool CodeHeap::merge_right(FreeBlock* a) {
  if (following_block(a) == a->link()) {
    // Update block a to include the following block
    a->set_length(a->length() + a->link()->length());
    a->set_link(a->link()->link());
    // Update find_start map
    size_t beg = segment_for(a);
    mark_segmap_as_used(beg, beg + a->length());
    _freelist_length--;
    return true;
  }
  return false;
}

void CodeHeap::add_to_freelist(HeapBlock* a) {
  FreeBlock* b = (FreeBlock*)a;
  _freelist_length++;

  // Mark as free and update free space count
  _freelist_segments += b->length();
  b->set_free();

  // First element in list?
  if (_freelist == NULL) {
    _freelist = b;
    b->set_link(NULL);
    return;
  }

  // Since the freelist is ordered (smaller addresses -> larger addresses) and the
  // element we want to insert into the freelist has a smaller address than the first
  // element, we can simply add 'b' as the first element and we are done.
  if (b < _freelist) {
    // Insert first in list
    b->set_link(_freelist);
    _freelist = b;
    merge_right(_freelist);
    return;
  }

  // Scan for right place to put into list. List
  // is sorted by increasing addresses
  FreeBlock* prev = _freelist;
  FreeBlock* cur  = _freelist->link();
  while (cur != NULL && cur < b) {
    prev = cur;
    cur  = cur->link();
  }
  insert_after(prev, b);
}

/**
 * Search freelist for an entry on the list with the best fit.
 * @return NULL, if no one was found
 */
FreeBlock* CodeHeap::search_freelist(size_t length) {
  FreeBlock* found_block = NULL;
  FreeBlock* found_prev  = NULL;
  size_t     found_length = 0;

  FreeBlock* prev = NULL;
  FreeBlock* cur = _freelist;

  // Search for first block that fits
  while (cur != NULL) {
    if (cur->length() >= length) {
      // Remember block, its previous element, and its length
      found_block = cur;
      found_prev  = prev;
      found_length = found_block->length();

      break;
    }
    // Next element in list
    prev = cur;
    cur  = cur->link();
  }

  if (found_block == NULL) {
    // None found
    return NULL;
  }

  // Exact (or at least good enough) fit. Remove from list.
  // Don't leave anything on the freelist smaller than CodeCacheMinBlockLength.
  if (found_length - length < CodeCacheMinBlockLength) {
    _freelist_length--;
    length = found_length;
    if (found_prev == NULL) {
      _freelist = _freelist->link();
    } else {
      // Unmap element
      found_prev->set_link(found_block->link());
    }
  } else {
    // Truncate block and return a pointer to the following block
    // Set used bit and length on new block
    found_block->set_length(found_length - length);
    found_block = following_block(found_block);

    size_t beg = segment_for(found_block);
    mark_segmap_as_used(beg, beg + length);
    found_block->set_length(length);
  }

  found_block->set_used();
  _freelist_segments -= length;
  return found_block;
}

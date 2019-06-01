#include "precompiled.hpp"

#include "asm/codeBuffer.hpp"
#include "compiler/disassembler.hpp"
#include "oops/methodData.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/icache.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "utilities/align.hpp"
#include "utilities/copy.hpp"
#include "utilities/xmlstream.hpp"

// The structure of a CodeSection:
//
//    _start ->           +----------------+
//                        | machine code...|
//    _end ->             |----------------|
//                        |                |
//                        |    (empty)     |
//                        |                |
//                        |                |
//                        +----------------+
//    _limit ->           |                |
//
//    _locs_start ->      +----------------+
//                        |reloc records...|
//                        |----------------|
//    _locs_end ->        |                |
//                        |                |
//                        |    (empty)     |
//                        |                |
//                        |                |
//                        +----------------+
//    _locs_limit ->      |                |
// The _end (resp. _limit) pointer refers to the first
// unused (resp. unallocated) byte.

// The structure of the CodeBuffer while code is being accumulated:
//
//    _total_start -> \
//    _insts._start ->              +----------------+
//                                  |                |
//                                  |     Code       |
//                                  |                |
//    _stubs._start ->              |----------------|
//                                  |                |
//                                  |    Stubs       | (also handlers for deopt/exception)
//                                  |                |
//    _consts._start ->             |----------------|
//                                  |                |
//                                  |   Constants    |
//                                  |                |
//                                  +----------------+
//    + _total_size ->              |                |
//
// When the code and relocations are copied to the code cache,
// the empty parts of each section are removed, and everything
// is copied into contiguous locations.

typedef CodeBuffer::csize_t csize_t;  // file-local definition

// External buffer, in a predefined CodeBlob.
// Important: The code_start must be taken exactly, and not realigned.
CodeBuffer::CodeBuffer(CodeBlob* blob) {
  initialize_misc("static buffer");
  initialize(blob->content_begin(), blob->content_size());
  verify_section_allocation();
}

void CodeBuffer::initialize(csize_t code_size, csize_t locs_size) {
  // Compute maximal alignment.
  int align = _insts.alignment();
  // Always allow for empty slop around each section.
  int slop = (int) CodeSection::end_slop();

  set_blob(BufferBlob::create(_name, code_size + (align+slop) * (SECT_LIMIT+1)));
  if (blob() == NULL) {
    // The assembler constructor will throw a fatal on an empty CodeBuffer.
    return;  // caller must test this
  }

  // Set up various pointers into the blob.
  initialize(_total_start, _total_size);

  pd_initialize();

  if (locs_size != 0) {
    _insts.initialize_locs(locs_size / sizeof(relocInfo));
  }

  verify_section_allocation();
}

CodeBuffer::~CodeBuffer() {
  verify_section_allocation();

  // If we allocate our code buffer from the CodeCache
  // via a BufferBlob, and it's not permanent, then
  // free the BufferBlob.
  // The rest of the memory will be freed when the ResourceObj
  // is released.
  for (CodeBuffer* cb = this; cb != NULL; cb = cb->before_expand()) {
    // Previous incarnations of this buffer are held live, so that internal
    // addresses constructed before expansions will not be confused.
    cb->free_blob();
  }

  // free any overflow storage
  delete _overflow_arena;

  // Claim is that stack allocation ensures resources are cleaned up.
  // This is resource clean up, let's hope that all were properly copied out.
  free_strings();
}

void CodeBuffer::initialize_oop_recorder(OopRecorder* r) {
  _oop_recorder = r;
}

void CodeBuffer::initialize_section_size(CodeSection* cs, csize_t size) {
  csize_t slop = CodeSection::end_slop();  // margin between sections
  int align = cs->alignment();
  address start  = _insts._start;
  address limit  = _insts._limit;
  address middle = limit - size;
  middle -= (intptr_t)middle & (align-1);  // align the division point downward
  guarantee(middle - slop > start, "need enough space to divide up");
  _insts._limit = middle - slop;  // subtract desired space, plus slop
  cs->initialize(middle, limit - middle);
  // give it some relocations to start with, if the main section has them
  if (_insts.has_locs())  cs->initialize_locs(1);
}

void CodeBuffer::freeze_section(CodeSection* cs) {
  CodeSection* next_cs = (cs == consts())? NULL: code_section(cs->index()+1);
  csize_t frozen_size = cs->size();
  if (next_cs != NULL) {
    frozen_size = next_cs->align_at_start(frozen_size);
  }
  address old_limit = cs->limit();
  address new_limit = cs->start() + frozen_size;
  relocInfo* old_locs_limit = cs->locs_limit();
  relocInfo* new_locs_limit = cs->locs_end();
  // Patch the limits.
  cs->_limit = new_limit;
  cs->_locs_limit = new_locs_limit;
  cs->_frozen = true;
  if (next_cs != NULL && !next_cs->is_allocated() && !next_cs->is_frozen()) {
    // Give remaining buffer space to the following section.
    next_cs->initialize(new_limit, old_limit - new_limit);
    next_cs->initialize_shared_locs(new_locs_limit,
                                    old_locs_limit - new_locs_limit);
  }
}

void CodeBuffer::set_blob(BufferBlob* blob) {
  _blob = blob;
  if (blob != NULL) {
    address start = blob->content_begin();
    address end   = blob->content_end();
    // Round up the starting address.
    int align = _insts.alignment();
    start += (-(intptr_t)start) & (align-1);
    _total_start = start;
    _total_size  = end - start;
  }
}

void CodeBuffer::free_blob() {
  if (_blob != NULL) {
    BufferBlob::free(_blob);
    set_blob(NULL);
  }
}

const char* CodeBuffer::code_section_name(int n) {
  return NULL;
}

int CodeBuffer::section_index_of(address addr) const {
  for (int n = 0; n < (int)SECT_LIMIT; n++) {
    const CodeSection* cs = code_section(n);
    if (cs->allocates(addr))  return n;
  }
  return SECT_NONE;
}

int CodeBuffer::locator(address addr) const {
  for (int n = 0; n < (int)SECT_LIMIT; n++) {
    const CodeSection* cs = code_section(n);
    if (cs->allocates(addr)) {
      return locator(addr - cs->start(), n);
    }
  }
  return -1;
}

address CodeBuffer::locator_address(int locator) const {
  if (locator < 0)  return NULL;
  address start = code_section(locator_sect(locator))->start();
  return start + locator_pos(locator);
}

bool CodeBuffer::is_backward_branch(Label& L) {
  return L.is_bound() && insts_end() <= locator_address(L.loc());
}

address CodeBuffer::decode_begin() {
  address begin = _insts.start();
  if (_decode_begin != NULL && _decode_begin > begin)
    begin = _decode_begin;
  return begin;
}

GrowableArray<int>* CodeBuffer::create_patch_overflow() {
  if (_overflow_arena == NULL) {
    _overflow_arena = new (mtCode) Arena(mtCode);
  }
  return new (_overflow_arena) GrowableArray<int>(_overflow_arena, 8, 0, 0);
}

// Helper function for managing labels and their target addresses.
// Returns a sensible address, and if it is not the label's final
// address, notes the dependency (at 'branch_pc') on the label.
address CodeSection::target(Label& L, address branch_pc) {
  if (L.is_bound()) {
    int loc = L.loc();
    if (index() == CodeBuffer::locator_sect(loc)) {
      return start() + CodeBuffer::locator_pos(loc);
    } else {
      return outer()->locator_address(loc);
    }
  } else {
    address base = start();
    int patch_loc = CodeBuffer::locator(branch_pc - base, index());
    L.add_patch_at(outer(), patch_loc);

    // Need to return a pc, doesn't matter what it is since it will be
    // replaced during resolution later.
    // Don't return NULL or badAddress, since branches shouldn't overflow.
    // Don't return base either because that could overflow displacements
    // for shorter branches.  It will get checked when bound.
    return branch_pc;
  }
}

void CodeSection::relocate(address at, relocInfo::relocType rtype, int format, jint method_index) {
  RelocationHolder rh;
  switch (rtype) {
    case relocInfo::none: return;
    case relocInfo::opt_virtual_call_type: {
      rh = opt_virtual_call_Relocation::spec(method_index);
      break;
    }
    case relocInfo::static_call_type: {
      rh = static_call_Relocation::spec(method_index);
      break;
    }
    case relocInfo::virtual_call_type: {
      rh = Relocation::spec_simple(rtype);
      break;
    }
    default: {
      rh = Relocation::spec_simple(rtype);
      break;
    }
  }
  relocate(at, rh, format);
}

void CodeSection::relocate(address at, RelocationHolder const& spec, int format) {
  // Do not relocate in scratch buffers.
  if (scratch_emit()) { return; }
  Relocation* reloc = spec.reloc();
  relocInfo::relocType rtype = (relocInfo::relocType) reloc->type();
  if (rtype == relocInfo::none)  return;

  if (!has_locs()) {
    return;
  }

  // Advance the point, noting the offset we'll have to record.
  csize_t offset = at - locs_point();
  set_locs_point(at);

  // Test for a couple of overflow conditions; maybe expand the buffer.
  relocInfo* end = locs_end();
  relocInfo* req = end + relocInfo::length_limit;
  // Check for (potential) overflow
  if (req >= locs_limit() || offset >= relocInfo::offset_limit()) {
    req += (uint)offset / (uint)relocInfo::offset_limit();
    if (req >= locs_limit()) {
      // Allocate or reallocate.
      expand_locs(locs_count() + (req - end));
      // reload pointer
      end = locs_end();
    }
  }

  // If the offset is giant, emit filler relocs, of type 'none', but
  // each carrying the largest possible offset, to advance the locs_point.
  while (offset >= relocInfo::offset_limit()) {
    *end++ = filler_relocInfo();
    offset -= filler_relocInfo().addr_offset();
  }

  // If it's a simple reloc with no data, we'll just write (rtype | offset).
  (*end) = relocInfo(rtype, offset, format);

  // If it has data, insert the prefix, as (data_prefix_tag | data1), data2.
  end->initialize(this, reloc);
}

void CodeSection::initialize_locs(int locs_capacity) {
  // Apply a priori lower limits to relocation size:
  csize_t min_locs = MAX2(size() / 16, (csize_t)4);
  if (locs_capacity < min_locs)  locs_capacity = min_locs;
  relocInfo* locs_start = NEW_RESOURCE_ARRAY(relocInfo, locs_capacity);
  _locs_start    = locs_start;
  _locs_end      = locs_start;
  _locs_limit    = locs_start + locs_capacity;
  _locs_own      = true;
}

void CodeSection::initialize_shared_locs(relocInfo* buf, int length) {
  // Internal invariant:  locs buf must be fully aligned.
  // See copy_relocations_to() below.
  while ((uintptr_t)buf % HeapWordSize != 0 && length > 0) {
    ++buf; --length;
  }
  if (length > 0) {
    _locs_start = buf;
    _locs_end   = buf;
    _locs_limit = buf + length;
    _locs_own   = false;
  }
}

void CodeSection::initialize_locs_from(const CodeSection* source_cs) {
  int lcount = source_cs->locs_count();
  if (lcount != 0) {
    initialize_shared_locs(source_cs->locs_start(), lcount);
    _locs_end = _locs_limit = _locs_start + lcount;
    set_locs_point(start() + source_cs->locs_point_off());
  }
}

void CodeSection::expand_locs(int new_capacity) {
  if (_locs_start == NULL) {
    initialize_locs(new_capacity);
    return;
  } else {
    int old_count    = locs_count();
    int old_capacity = locs_capacity();
    if (new_capacity < old_capacity * 2)
      new_capacity = old_capacity * 2;
    relocInfo* locs_start;
    if (_locs_own) {
      locs_start = REALLOC_RESOURCE_ARRAY(relocInfo, _locs_start, old_capacity, new_capacity);
    } else {
      locs_start = NEW_RESOURCE_ARRAY(relocInfo, new_capacity);
      Copy::conjoint_jbytes(_locs_start, locs_start, old_capacity * sizeof(relocInfo));
      _locs_own = true;
    }
    _locs_start    = locs_start;
    _locs_end      = locs_start + old_count;
    _locs_limit    = locs_start + new_capacity;
  }
}

/// Support for emitting the code to its final location.
/// The pattern is the same for all functions.
/// We iterate over all the sections, padding each to alignment.

csize_t CodeBuffer::total_content_size() const {
  csize_t size_so_far = 0;
  for (int n = 0; n < (int)SECT_LIMIT; n++) {
    const CodeSection* cs = code_section(n);
    if (cs->is_empty())  continue;  // skip trivial section
    size_so_far = cs->align_at_start(size_so_far);
    size_so_far += cs->size();
  }
  return size_so_far;
}

void CodeBuffer::compute_final_layout(CodeBuffer* dest) const {
  address buf = dest->_total_start;
  csize_t buf_offset = 0;

  {
    // not sure why this is here, but why not...
    int alignSize = MAX2((intx) sizeof(jdouble), CodeEntryAlignment);
  }

  const CodeSection* prev_cs      = NULL;
  CodeSection*       prev_dest_cs = NULL;

  for (int n = (int) SECT_FIRST; n < (int) SECT_LIMIT; n++) {
    // figure compact layout of each section
    const CodeSection* cs = code_section(n);
    csize_t csize = cs->size();

    CodeSection* dest_cs = dest->code_section(n);
    if (!cs->is_empty()) {
      // Compute initial padding; assign it to the previous non-empty guy.
      // Cf. figure_expanded_capacities.
      csize_t padding = cs->align_at_start(buf_offset) - buf_offset;
      if (prev_dest_cs != NULL) {
        if (padding != 0) {
          buf_offset += padding;
          prev_dest_cs->_limit += padding;
        }
      } else {
        guarantee(padding == 0, "In first iteration no padding should be needed.");
      }
      prev_dest_cs = dest_cs;
      prev_cs      = cs;
    }

    dest_cs->initialize(buf+buf_offset, csize);
    dest_cs->set_end(buf+buf_offset+csize);

    buf_offset += csize;
  }

  dest->verify_section_allocation();
}

// Append an oop reference that keeps the class alive.
static void append_oop_references(GrowableArray<oop>* oops, Klass* k) {
  oop cl = k->klass_holder();
  if (cl != NULL && !oops->contains(cl)) {
    oops->append(cl);
  }
}

void CodeBuffer::finalize_oop_references(const methodHandle& mh) {
  NoSafepointVerifier nsv;

  GrowableArray<oop> oops;

  // Make sure that immediate metadata records something in the OopRecorder
  for (int n = (int) SECT_FIRST; n < (int) SECT_LIMIT; n++) {
    // pull code out of each section
    CodeSection* cs = code_section(n);
    if (cs->is_empty())  continue;  // skip trivial section
    RelocIterator iter(cs);
    while (iter.next()) {
      if (iter.type() == relocInfo::metadata_type) {
        metadata_Relocation* md = iter.metadata_reloc();
        if (md->metadata_is_immediate()) {
          Metadata* m = md->metadata_value();
          if (oop_recorder()->is_real(m)) {
            if (m->is_methodData()) {
              m = ((MethodData*)m)->method();
            }
            if (m->is_method()) {
              m = ((Method*)m)->method_holder();
            }
            if (m->is_klass()) {
              append_oop_references(&oops, (Klass*)m);
            } else {
              // XXX This will currently occur for MDO which don't
              // have a backpointer.  This has to be fixed later.
              m->print();
              ShouldNotReachHere();
            }
          }
        }
      }
    }
  }

  if (!oop_recorder()->is_unused()) {
    for (int i = 0; i < oop_recorder()->metadata_count(); i++) {
      Metadata* m = oop_recorder()->metadata_at(i);
      if (oop_recorder()->is_real(m)) {
        if (m->is_methodData()) {
          m = ((MethodData*)m)->method();
        }
        if (m->is_method()) {
          m = ((Method*)m)->method_holder();
        }
        if (m->is_klass()) {
          append_oop_references(&oops, (Klass*)m);
        } else {
          m->print();
          ShouldNotReachHere();
        }
      }
    }
  }

  // Add the class loader of Method* for the nmethod itself
  append_oop_references(&oops, mh->method_holder());

  // Add any oops that we've found
  Thread* thread = Thread::current();
  for (int i = 0; i < oops.length(); i++) {
    oop_recorder()->find_index((jobject)thread->handle_area()->allocate_handle(oops.at(i)));
  }
}

csize_t CodeBuffer::total_offset_of(const CodeSection* cs) const {
  csize_t size_so_far = 0;
  for (int n = (int) SECT_FIRST; n < (int) SECT_LIMIT; n++) {
    const CodeSection* cur_cs = code_section(n);
    if (!cur_cs->is_empty()) {
      size_so_far = cur_cs->align_at_start(size_so_far);
    }
    if (cur_cs->index() == cs->index()) {
      return size_so_far;
    }
    size_so_far += cur_cs->size();
  }
  ShouldNotReachHere();
  return -1;
}

csize_t CodeBuffer::total_relocation_size() const {
  csize_t total = copy_relocations_to(NULL);  // dry run only
  return (csize_t) align_up(total, HeapWordSize);
}

csize_t CodeBuffer::copy_relocations_to(address buf, csize_t buf_limit, bool only_inst) const {
  csize_t buf_offset = 0;
  csize_t code_end_so_far = 0;
  csize_t code_point_so_far = 0;

  for (int n = (int) SECT_FIRST; n < (int)SECT_LIMIT; n++) {
    if (only_inst && (n != (int)SECT_INSTS)) {
      // Need only relocation info for code.
      continue;
    }
    // pull relocs out of each section
    const CodeSection* cs = code_section(n);
    if (cs->is_empty())  continue;  // skip trivial section
    relocInfo* lstart = cs->locs_start();
    relocInfo* lend   = cs->locs_end();
    csize_t    lsize  = (csize_t)( (address)lend - (address)lstart );
    csize_t    csize  = cs->size();
    code_end_so_far = cs->align_at_start(code_end_so_far);

    if (lsize > 0) {
      // Figure out how to advance the combined relocation point
      // first to the beginning of this section.
      // We'll insert one or more filler relocs to span that gap.
      // (Don't bother to improve this by editing the first reloc's offset.)
      csize_t new_code_point = code_end_so_far;
      for (csize_t jump; code_point_so_far < new_code_point; code_point_so_far += jump) {
        jump = new_code_point - code_point_so_far;
        relocInfo filler = filler_relocInfo();
        if (jump >= filler.addr_offset()) {
          jump = filler.addr_offset();
        } else {  // else shrink the filler to fit
          filler = relocInfo(relocInfo::none, jump);
        }
        if (buf != NULL) {
          *(relocInfo*)(buf+buf_offset) = filler;
        }
        buf_offset += sizeof(filler);
      }

      // Update code point and end to skip past this section:
      csize_t last_code_point = code_end_so_far + cs->locs_point_off();
      code_point_so_far = last_code_point; // advance past this guy's relocs
    }
    code_end_so_far += csize;  // advance past this guy's instructions too

    // Done with filler; emit the real relocations:
    if (buf != NULL && lsize != 0) {
      if (buf_offset % HeapWordSize == 0) {
        // Use wordwise copies if possible:
        Copy::disjoint_words((HeapWord*)lstart,
                             (HeapWord*)(buf+buf_offset),
                             (lsize + HeapWordSize-1) / HeapWordSize);
      } else {
        Copy::conjoint_jbytes(lstart, buf+buf_offset, lsize);
      }
    }
    buf_offset += lsize;
  }

  // Align end of relocation info in target.
  while (buf_offset % HeapWordSize != 0) {
    if (buf != NULL) {
      relocInfo padding = relocInfo(relocInfo::none, 0);
      *(relocInfo*)(buf+buf_offset) = padding;
    }
    buf_offset += sizeof(relocInfo);
  }

  return buf_offset;
}

csize_t CodeBuffer::copy_relocations_to(CodeBlob* dest) const {
  address buf = NULL;
  csize_t buf_offset = 0;
  csize_t buf_limit = 0;

  if (dest != NULL) {
    buf = (address)dest->relocation_begin();
    buf_limit = (address)dest->relocation_end() - buf;
  }
  // if dest == NULL, this is just the sizing pass
  //
  buf_offset = copy_relocations_to(buf, buf_limit, false);

  return buf_offset;
}

void CodeBuffer::copy_code_to(CodeBlob* dest_blob) {

  CodeBuffer dest(dest_blob);
  this->compute_final_layout(&dest);

  // Set beginning of constant table before relocating.
  dest_blob->set_ctable_begin(dest.consts()->start());

  relocate_code_to(&dest);

  // transfer strings and comments from buffer to blob
  dest_blob->set_strings(_code_strings);

  // Flush generated code
  ICache::invalidate_range(dest_blob->code_begin(), dest_blob->code_size());
}

// Move all my code into another code buffer.  Consult applicable
// relocs to repair embedded addresses.  The layout in the destination
// CodeBuffer is different to the source CodeBuffer: the destination
// CodeBuffer gets the final layout (consts, insts, stubs in order of
// ascending address).
void CodeBuffer::relocate_code_to(CodeBuffer* dest) const {
  address dest_end = dest->_total_start + dest->_total_size;
  address dest_filled = NULL;
  for (int n = (int) SECT_FIRST; n < (int) SECT_LIMIT; n++) {
    // pull code out of each section
    const CodeSection* cs = code_section(n);
    if (cs->is_empty())  continue;  // skip trivial section
    CodeSection* dest_cs = dest->code_section(n);
    csize_t usize = dest_cs->size();
    csize_t wsize = align_up(usize, HeapWordSize);
    // Copy the code as aligned machine words.
    // This may also include an uninitialized partial word at the end.
    Copy::disjoint_words((HeapWord*)cs->start(),
                         (HeapWord*)dest_cs->start(),
                         wsize / HeapWordSize);

    if (dest->blob() == NULL) {
      // Destination is a final resting place, not just another buffer.
      // Normalize uninitialized bytes in the final padding.
      Copy::fill_to_bytes(dest_cs->end(), dest_cs->remaining(),
                          Assembler::code_fill_byte());
    }
    // Keep track of the highest filled address
    dest_filled = MAX2(dest_filled, dest_cs->end() + dest_cs->remaining());

    // Make the new code copy use the old copy's relocations:
    dest_cs->initialize_locs_from(cs);
  }

  // Do relocation after all sections are copied.
  // This is necessary if the code uses constants in stubs, which are
  // relocated when the corresponding instruction in the code (e.g., a
  // call) is relocated. Stubs are placed behind the main code
  // section, so that section has to be copied before relocating.
  for (int n = (int) SECT_FIRST; n < (int)SECT_LIMIT; n++) {
    // pull code out of each section
    const CodeSection* cs = code_section(n);
    if (cs->is_empty()) continue;  // skip trivial section
    CodeSection* dest_cs = dest->code_section(n);
    { // Repair the pc relative information in the code after the move
      RelocIterator iter(dest_cs);
      while (iter.next()) {
        iter.reloc()->fix_relocation_after_move(this, dest);
      }
    }
  }

  if (dest->blob() == NULL && dest_filled != NULL) {
    // Destination is a final resting place, not just another buffer.
    // Normalize uninitialized bytes in the final padding.
    Copy::fill_to_bytes(dest_filled, dest_end - dest_filled,
                        Assembler::code_fill_byte());
  }
}

csize_t CodeBuffer::figure_expanded_capacities(CodeSection* which_cs,
                                               csize_t amount,
                                               csize_t* new_capacity) {
  csize_t new_total_cap = 0;

  for (int n = (int) SECT_FIRST; n < (int) SECT_LIMIT; n++) {
    const CodeSection* sect = code_section(n);

    if (!sect->is_empty()) {
      // Compute initial padding; assign it to the previous section,
      // even if it's empty (e.g. consts section can be empty).
      // Cf. compute_final_layout
      csize_t padding = sect->align_at_start(new_total_cap) - new_total_cap;
      if (padding != 0) {
        new_total_cap += padding;
        new_capacity[n - 1] += padding;
      }
    }

    csize_t exp = sect->size();  // 100% increase
    if ((uint)exp < 4*K)  exp = 4*K;       // minimum initial increase
    if (sect == which_cs) {
      if (exp < amount)  exp = amount;
      if (StressCodeBuffers)  exp = amount;  // expand only slightly
    } else if (n == SECT_INSTS) {
      // scale down inst increases to a more modest 25%
      exp = 4*K + ((exp - 4*K) >> 2);
      if (StressCodeBuffers)  exp = amount / 2;  // expand only slightly
    } else if (sect->is_empty()) {
      // do not grow an empty secondary section
      exp = 0;
    }
    // Allow for inter-section slop:
    exp += CodeSection::end_slop();
    csize_t new_cap = sect->size() + exp;
    if (new_cap < sect->capacity()) {
      // No need to expand after all.
      new_cap = sect->capacity();
    }
    new_capacity[n] = new_cap;
    new_total_cap += new_cap;
  }

  return new_total_cap;
}

void CodeBuffer::expand(CodeSection* which_cs, csize_t amount) {

  // Resizing must be allowed
  {
    if (blob() == NULL)  return;  // caller must check for blob == NULL
    for (int n = 0; n < (int)SECT_LIMIT; n++) {
      guarantee(!code_section(n)->is_frozen(), "resizing not allowed when frozen");
    }
  }

  // Figure new capacity for each section.
  csize_t new_capacity[SECT_LIMIT];
  memset(new_capacity, 0, sizeof(csize_t) * SECT_LIMIT);
  csize_t new_total_cap = figure_expanded_capacities(which_cs, amount, new_capacity);

  // Create a new (temporary) code buffer to hold all the new data
  CodeBuffer cb(name(), new_total_cap, 0);
  if (cb.blob() == NULL) {
    // Failed to allocate in code cache.
    free_blob();
    return;
  }

  // Create an old code buffer to remember which addresses used to go where.
  // This will be useful when we do final assembly into the code cache,
  // because we will need to know how to warp any internal address that
  // has been created at any time in this CodeBuffer's past.
  CodeBuffer* bxp = new CodeBuffer(_total_start, _total_size);
  bxp->take_over_code_from(this);  // remember the old undersized blob
  bxp->_before_expand = this->_before_expand;
  this->_before_expand = bxp;

  // Give each section its required (expanded) capacity.
  for (int n = (int)SECT_LIMIT-1; n >= SECT_FIRST; n--) {
    CodeSection* cb_sect   = cb.code_section(n);
    CodeSection* this_sect = code_section(n);
    if (new_capacity[n] == 0)  continue;  // already nulled out
    if (n != SECT_INSTS) {
      cb.initialize_section_size(cb_sect, new_capacity[n]);
    }
    address cb_start = cb_sect->start();
    cb_sect->set_end(cb_start + this_sect->size());
    if (this_sect->mark() == NULL) {
      cb_sect->clear_mark();
    } else {
      cb_sect->set_mark(cb_start + this_sect->mark_off());
    }
  }

  // Needs to be initialized when calling fix_relocation_after_move.
  cb.blob()->set_ctable_begin(cb.consts()->start());

  // Move all the code and relocations to the new blob:
  relocate_code_to(&cb);

  // Copy the temporary code buffer into the current code buffer.
  // Basically, do {*this = cb}, except for some control information.
  this->take_over_code_from(&cb);
  cb.set_blob(NULL);

  _decode_begin = NULL;  // sanity

  // Make certain that the new sections are all snugly inside the new blob.
  verify_section_allocation();
}

void CodeBuffer::take_over_code_from(CodeBuffer* cb) {
  // Take the new blob away from cb.
  set_blob(cb->blob());
  // Take over all the section pointers.
  for (int n = 0; n < (int)SECT_LIMIT; n++) {
    CodeSection* cb_sect   = cb->code_section(n);
    CodeSection* this_sect = code_section(n);
    this_sect->take_over_code_from(cb_sect);
  }
  _overflow_arena = cb->_overflow_arena;
}

void CodeBuffer::verify_section_allocation() {
  address tstart = _total_start;
  if (tstart == badAddress)  return;  // smashed by set_blob(NULL)
  address tend   = tstart + _total_size;
  if (_blob != NULL) {

    guarantee(tstart >= _blob->content_begin(), "sanity");
    guarantee(tend   <= _blob->content_end(),   "sanity");
  }
  // Verify disjointness.
  for (int n = (int) SECT_FIRST; n < (int) SECT_LIMIT; n++) {
    CodeSection* sect = code_section(n);
    if (!sect->is_allocated() || sect->is_empty())  continue;
    guarantee((intptr_t)sect->start() % sect->alignment() == 0 || sect->is_empty() || _blob == NULL, "start is aligned");
    for (int m = (int) SECT_FIRST; m < (int) SECT_LIMIT; m++) {
      CodeSection* other = code_section(m);
      if (!other->is_allocated() || other == sect)  continue;
      guarantee(!other->contains(sect->start()    ), "sanity");
      // limit is an exclusive address and can be the start of another
      // section.
      guarantee(!other->contains(sect->limit() - 1), "sanity");
    }
    guarantee(sect->end() <= tend, "sanity");
    guarantee(sect->end() <= sect->limit(), "sanity");
  }
}

void CodeBuffer::log_section_sizes(const char* name) {
  if (xtty != NULL) {
    ttyLocker ttyl;
    // log info about buffer usage
    xtty->print_cr("<blob name='%s' size='%d'>", name, _total_size);
    for (int n = (int) CodeBuffer::SECT_FIRST; n < (int) CodeBuffer::SECT_LIMIT; n++) {
      CodeSection* sect = code_section(n);
      if (!sect->is_allocated() || sect->is_empty())  continue;
      xtty->print_cr("<sect index='%d' size='" SIZE_FORMAT "' free='" SIZE_FORMAT "'/>", n, sect->limit() - sect->start(), sect->limit() - sect->end());
    }
    xtty->print_cr("</blob>");
  }
}

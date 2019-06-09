#include "precompiled.hpp"

#include "gc/shared/collectedHeap.hpp"
#include "memory/metaspace.hpp"
#include "memory/metaspace/chunkManager.hpp"
#include "memory/metaspace/metachunk.hpp"
#include "memory/metaspace/metaspaceCommon.hpp"
#include "memory/metaspace/spaceManager.hpp"
#include "memory/metaspace/virtualSpaceList.hpp"
#include "memory/universe.hpp"
#include "runtime/init.hpp"
#include "runtime/orderAccess.hpp"
#include "utilities/copy.hpp"
#include "utilities/debug.hpp"
#include "utilities/formatBuffer.hpp"
#include "utilities/globalDefinitions.hpp"

using namespace metaspace;

MetaWord* last_allocated = 0;

size_t Metaspace::_compressed_class_space_size;

static const char* space_type_name(Metaspace::MetaspaceType t) {
  const char* s = NULL;
  switch (t) {
    case Metaspace::StandardMetaspaceType: s = "Standard"; break;
    case Metaspace::BootMetaspaceType: s = "Boot"; break;
    case Metaspace::AnonymousMetaspaceType: s = "Anonymous"; break;
    case Metaspace::ReflectionMetaspaceType: s = "Reflection"; break;
    default: ShouldNotReachHere();
  }
  return s;
}

volatile size_t MetaspaceGC::_capacity_until_GC = 0;
uint MetaspaceGC::_shrink_factor = 0;
bool MetaspaceGC::_should_concurrent_collect = false;

// BlockFreelist methods

// VirtualSpaceNode methods

// MetaspaceGC methods

// VM_CollectForMetadataAllocation is the vm operation used to GC.
// Within the VM operation after the GC the attempt to allocate the metadata
// should succeed.  If the GC did not free enough space for the metaspace
// allocation, the HWM is increased so that another virtualspace will be
// allocated for the metadata.  With perm gen the increase in the perm
// gen had bounds, MinMetaspaceExpansion and MaxMetaspaceExpansion.  The
// metaspace policy uses those as the small and large steps for the HWM.
//
// After the GC the compute_new_size() for MetaspaceGC is called to
// resize the capacity of the metaspaces.  The current implementation
// is based on the flags MinMetaspaceFreeRatio and MaxMetaspaceFreeRatio used
// to resize the Java heap by some GC's.  New flags can be implemented
// if really needed.  MinMetaspaceFreeRatio is used to calculate how much
// free space is desirable in the metaspace capacity to decide how much
// to increase the HWM.  MaxMetaspaceFreeRatio is used to decide how much
// free space is desirable in the metaspace capacity before decreasing
// the HWM.

// Calculate the amount to increase the high water mark (HWM).
// Increase by a minimum amount (MinMetaspaceExpansion) so that
// another expansion is not requested too soon.  If that is not
// enough to satisfy the allocation, increase by MaxMetaspaceExpansion.
// If that is still not enough, expand by the size of the allocation
// plus some.
size_t MetaspaceGC::delta_capacity_until_GC(size_t bytes) {
  size_t min_delta = MinMetaspaceExpansion;
  size_t max_delta = MaxMetaspaceExpansion;
  size_t delta = align_up(bytes, Metaspace::commit_alignment());

  if (delta <= min_delta) {
    delta = min_delta;
  } else if (delta <= max_delta) {
    // Don't want to hit the high water mark on the next
    // allocation so make the delta greater than just enough
    // for this allocation.
    delta = max_delta;
  } else {
    // This allocation is large but the next ones are probably not
    // so increase by the minimum.
    delta = delta + min_delta;
  }

  return delta;
}

size_t MetaspaceGC::capacity_until_GC() {
  size_t value = OrderAccess::load_acquire(&_capacity_until_GC);
  return value;
}

// Try to increase the _capacity_until_GC limit counter by v bytes.
// Returns true if it succeeded. It may fail if either another thread
// concurrently increased the limit or the new limit would be larger
// than MaxMetaspaceSize.
// On success, optionally returns new and old metaspace capacity in
// new_cap_until_GC and old_cap_until_GC respectively.
// On error, optionally sets can_retry to indicate whether if there is
// actually enough space remaining to satisfy the request.
bool MetaspaceGC::inc_capacity_until_GC(size_t v, size_t* new_cap_until_GC, size_t* old_cap_until_GC, bool* can_retry) {
  size_t old_capacity_until_GC = _capacity_until_GC;
  size_t new_value = old_capacity_until_GC + v;

  if (new_value < old_capacity_until_GC) {
    // The addition wrapped around, set new_value to aligned max value.
    new_value = align_down(max_uintx, Metaspace::commit_alignment());
  }

  if (new_value > MaxMetaspaceSize) {
    if (can_retry != NULL) {
      *can_retry = false;
    }
    return false;
  }

  if (can_retry != NULL) {
    *can_retry = true;
  }
  size_t prev_value = Atomic::cmpxchg(new_value, &_capacity_until_GC, old_capacity_until_GC);

  if (old_capacity_until_GC != prev_value) {
    return false;
  }

  if (new_cap_until_GC != NULL) {
    *new_cap_until_GC = new_value;
  }
  if (old_cap_until_GC != NULL) {
    *old_cap_until_GC = old_capacity_until_GC;
  }
  return true;
}

size_t MetaspaceGC::dec_capacity_until_GC(size_t v) {
  return Atomic::sub(v, &_capacity_until_GC);
}

void MetaspaceGC::initialize() {
  // Set the high-water mark to MaxMetapaceSize during VM initializaton since
  // we can't do a GC during initialization.
  _capacity_until_GC = MaxMetaspaceSize;
}

void MetaspaceGC::post_initialize() {
  // Reset the high-water mark once the VM initialization is done.
  _capacity_until_GC = MAX2(MetaspaceUtils::committed_bytes(), MetaspaceSize);
}

bool MetaspaceGC::can_expand(size_t word_size, bool is_class) {
  // Check if the compressed class space is full.
  if (is_class && Metaspace::using_class_space()) {
    size_t class_committed = MetaspaceUtils::committed_bytes(Metaspace::ClassType);
    if (class_committed + word_size * BytesPerWord > CompressedClassSpaceSize) {
      return false;
    }
  }

  // Check if the user has imposed a limit on the metaspace memory.
  size_t committed_bytes = MetaspaceUtils::committed_bytes();
  if (committed_bytes + word_size * BytesPerWord > MaxMetaspaceSize) {
    return false;
  }

  return true;
}

size_t MetaspaceGC::allowed_expansion() {
  size_t committed_bytes = MetaspaceUtils::committed_bytes();
  size_t capacity_until_gc = capacity_until_GC();

  size_t left_until_max  = MaxMetaspaceSize - committed_bytes;
  size_t left_until_GC = capacity_until_gc - committed_bytes;
  size_t left_to_commit = MIN2(left_until_GC, left_until_max);

  return left_to_commit / BytesPerWord;
}

void MetaspaceGC::compute_new_size() {
  uint current_shrink_factor = _shrink_factor;
  _shrink_factor = 0;

  // Using committed_bytes() for used_after_gc is an overestimation, since the
  // chunk free lists are included in committed_bytes() and the memory in an
  // un-fragmented chunk free list is available for future allocations.
  // However, if the chunk free lists becomes fragmented, then the memory may
  // not be available for future allocations and the memory is therefore "in use".
  // Including the chunk free lists in the definition of "in use" is therefore
  // necessary. Not including the chunk free lists can cause capacity_until_GC to
  // shrink below committed_bytes() and this has caused serious bugs in the past.
  const size_t used_after_gc = MetaspaceUtils::committed_bytes();
  const size_t capacity_until_GC = MetaspaceGC::capacity_until_GC();

  const double minimum_free_percentage = MinMetaspaceFreeRatio / 100.0;
  const double maximum_used_percentage = 1.0 - minimum_free_percentage;

  const double min_tmp = used_after_gc / maximum_used_percentage;
  size_t minimum_desired_capacity = (size_t)MIN2(min_tmp, double(MaxMetaspaceSize));
  // Don't shrink less than the initial generation size
  minimum_desired_capacity = MAX2(minimum_desired_capacity, MetaspaceSize);

  size_t shrink_bytes = 0;
  if (capacity_until_GC < minimum_desired_capacity) {
    // If we have less capacity below the metaspace HWM, then
    // increment the HWM.
    size_t expand_bytes = minimum_desired_capacity - capacity_until_GC;
    expand_bytes = align_up(expand_bytes, Metaspace::commit_alignment());
    // Don't expand unless it's significant
    if (expand_bytes >= MinMetaspaceExpansion) {
      size_t new_capacity_until_GC = 0;
      bool succeeded = MetaspaceGC::inc_capacity_until_GC(expand_bytes, &new_capacity_until_GC);
    }
    return;
  }

  size_t max_shrink_bytes = capacity_until_GC - minimum_desired_capacity;

  // Should shrinking be considered?
  if (MaxMetaspaceFreeRatio < 100) {
    const double maximum_free_percentage = MaxMetaspaceFreeRatio / 100.0;
    const double minimum_used_percentage = 1.0 - maximum_free_percentage;
    const double max_tmp = used_after_gc / minimum_used_percentage;
    size_t maximum_desired_capacity = (size_t)MIN2(max_tmp, double(MaxMetaspaceSize));
    maximum_desired_capacity = MAX2(maximum_desired_capacity, MetaspaceSize);

    if (capacity_until_GC > maximum_desired_capacity) {
      // Capacity too large, compute shrinking size
      shrink_bytes = capacity_until_GC - maximum_desired_capacity;
      // We don't want shrink all the way back to initSize if people call
      // System.gc(), because some programs do that between "phases" and then
      // we'd just have to grow the heap up again for the next phase.  So we
      // damp the shrinking: 0% on the first call, 10% on the second call, 40%
      // on the third call, and 100% by the fourth call.  But if we recompute
      // size without shrinking, it goes back to 0%.
      shrink_bytes = shrink_bytes / 100 * current_shrink_factor;

      shrink_bytes = align_down(shrink_bytes, Metaspace::commit_alignment());

      if (current_shrink_factor == 0) {
        _shrink_factor = 10;
      } else {
        _shrink_factor = MIN2(current_shrink_factor * 4, (uint) 100);
      }
    }
  }

  // Don't shrink unless it's significant
  if (shrink_bytes >= MinMetaspaceExpansion && ((capacity_until_GC - shrink_bytes) >= MetaspaceSize)) {
    size_t new_capacity_until_GC = MetaspaceGC::dec_capacity_until_GC(shrink_bytes);
  }
}

// MetaspaceUtils
size_t MetaspaceUtils::_capacity_words [Metaspace::MetadataTypeCount] = { 0, 0 };
size_t MetaspaceUtils::_overhead_words [Metaspace::MetadataTypeCount] = { 0, 0 };
volatile size_t MetaspaceUtils::_used_words [Metaspace::MetadataTypeCount] = { 0, 0 };

// Collect used metaspace statistics. This involves walking the CLDG. The resulting
// output will be the accumulated values for all live metaspaces.
// Note: method does not do any locking.
void MetaspaceUtils::collect_statistics(ClassLoaderMetaspaceStatistics* out) {
  out->reset();
  ClassLoaderDataGraphMetaspaceIterator iter;
   while (iter.repeat()) {
     ClassLoaderMetaspace* msp = iter.get_next();
     if (msp != NULL) {
       msp->add_to_statistics(out);
     }
   }
}

size_t MetaspaceUtils::free_in_vs_bytes(Metaspace::MetadataType mdtype) {
  VirtualSpaceList* list = Metaspace::get_space_list(mdtype);
  return list == NULL ? 0 : list->free_bytes();
}

size_t MetaspaceUtils::free_in_vs_bytes() {
  return free_in_vs_bytes(Metaspace::ClassType) + free_in_vs_bytes(Metaspace::NonClassType);
}

static void inc_stat_nonatomically(size_t* pstat, size_t words) {
  (*pstat) += words;
}

static void dec_stat_nonatomically(size_t* pstat, size_t words) {
  const size_t size_now = *pstat;
  *pstat = size_now - words;
}

static void inc_stat_atomically(volatile size_t* pstat, size_t words) {
  Atomic::add(words, pstat);
}

static void dec_stat_atomically(volatile size_t* pstat, size_t words) {
  const size_t size_now = *pstat;
  Atomic::sub(words, pstat);
}

void MetaspaceUtils::dec_capacity(Metaspace::MetadataType mdtype, size_t words) {
  dec_stat_nonatomically(&_capacity_words[mdtype], words);
}
void MetaspaceUtils::inc_capacity(Metaspace::MetadataType mdtype, size_t words) {
  inc_stat_nonatomically(&_capacity_words[mdtype], words);
}
void MetaspaceUtils::dec_used(Metaspace::MetadataType mdtype, size_t words) {
  dec_stat_atomically(&_used_words[mdtype], words);
}
void MetaspaceUtils::inc_used(Metaspace::MetadataType mdtype, size_t words) {
  inc_stat_atomically(&_used_words[mdtype], words);
}
void MetaspaceUtils::dec_overhead(Metaspace::MetadataType mdtype, size_t words) {
  dec_stat_nonatomically(&_overhead_words[mdtype], words);
}
void MetaspaceUtils::inc_overhead(Metaspace::MetadataType mdtype, size_t words) {
  inc_stat_nonatomically(&_overhead_words[mdtype], words);
}

size_t MetaspaceUtils::reserved_bytes(Metaspace::MetadataType mdtype) {
  VirtualSpaceList* list = Metaspace::get_space_list(mdtype);
  return list == NULL ? 0 : list->reserved_bytes();
}

size_t MetaspaceUtils::committed_bytes(Metaspace::MetadataType mdtype) {
  VirtualSpaceList* list = Metaspace::get_space_list(mdtype);
  return list == NULL ? 0 : list->committed_bytes();
}

size_t MetaspaceUtils::min_chunk_size_words() { return Metaspace::first_chunk_word_size(); }

size_t MetaspaceUtils::free_chunks_total_words(Metaspace::MetadataType mdtype) {
  ChunkManager* chunk_manager = Metaspace::get_chunk_manager(mdtype);
  if (chunk_manager == NULL) {
    return 0;
  }
  return chunk_manager->free_chunks_total_words();
}

size_t MetaspaceUtils::free_chunks_total_bytes(Metaspace::MetadataType mdtype) {
  return free_chunks_total_words(mdtype) * BytesPerWord;
}

size_t MetaspaceUtils::free_chunks_total_words() {
  return free_chunks_total_words(Metaspace::ClassType) + free_chunks_total_words(Metaspace::NonClassType);
}

size_t MetaspaceUtils::free_chunks_total_bytes() {
  return free_chunks_total_words() * BytesPerWord;
}

bool MetaspaceUtils::has_chunk_free_list(Metaspace::MetadataType mdtype) {
  return Metaspace::get_chunk_manager(mdtype) != NULL;
}

MetaspaceChunkFreeListSummary MetaspaceUtils::chunk_free_list_summary(Metaspace::MetadataType mdtype) {
  if (!has_chunk_free_list(mdtype)) {
    return MetaspaceChunkFreeListSummary();
  }

  const ChunkManager* cm = Metaspace::get_chunk_manager(mdtype);
  return cm->chunk_free_list_summary();
}

void MetaspaceUtils::print_metaspace_change(size_t prev_metadata_used) { }

void MetaspaceUtils::print_on(outputStream* out) {
  Metaspace::MetadataType nct = Metaspace::NonClassType;

  out->print_cr(" Metaspace       used "      SIZE_FORMAT "K, capacity "  SIZE_FORMAT "K, committed " SIZE_FORMAT "K, reserved "  SIZE_FORMAT "K",
                used_bytes()/K, capacity_bytes()/K, committed_bytes()/K, reserved_bytes()/K);

  if (Metaspace::using_class_space()) {
    Metaspace::MetadataType ct = Metaspace::ClassType;
    out->print_cr("  class space    used "      SIZE_FORMAT "K, capacity "  SIZE_FORMAT "K, committed " SIZE_FORMAT "K, reserved "  SIZE_FORMAT "K",
                  used_bytes(ct)/K, capacity_bytes(ct)/K, committed_bytes(ct)/K, reserved_bytes(ct)/K);
  }
}

void MetaspaceUtils::print_vs(outputStream* out, size_t scale) {
  const size_t reserved_nonclass_words = reserved_bytes(Metaspace::NonClassType) / sizeof(MetaWord);
  const size_t committed_nonclass_words = committed_bytes(Metaspace::NonClassType) / sizeof(MetaWord);
  {
    if (Metaspace::using_class_space()) {
      out->print("  Non-class space:  ");
    }
    print_scaled_words(out, reserved_nonclass_words, scale, 7);
    out->print(" reserved, ");
    print_scaled_words_and_percentage(out, committed_nonclass_words, reserved_nonclass_words, scale, 7);
    out->print_cr(" committed ");

    if (Metaspace::using_class_space()) {
      const size_t reserved_class_words = reserved_bytes(Metaspace::ClassType) / sizeof(MetaWord);
      const size_t committed_class_words = committed_bytes(Metaspace::ClassType) / sizeof(MetaWord);
      out->print("      Class space:  ");
      print_scaled_words(out, reserved_class_words, scale, 7);
      out->print(" reserved, ");
      print_scaled_words_and_percentage(out, committed_class_words, reserved_class_words, scale, 7);
      out->print_cr(" committed ");

      const size_t reserved_words = reserved_nonclass_words + reserved_class_words;
      const size_t committed_words = committed_nonclass_words + committed_class_words;
      out->print("             Both:  ");
      print_scaled_words(out, reserved_words, scale, 7);
      out->print(" reserved, ");
      print_scaled_words_and_percentage(out, committed_words, reserved_words, scale, 7);
      out->print_cr(" committed ");
    }
  }
}

// Prints an ASCII representation of the given space.
void MetaspaceUtils::print_metaspace_map(outputStream* out, Metaspace::MetadataType mdtype) {
  MutexLockerEx cl(MetaspaceExpand_lock, Mutex::_no_safepoint_check_flag);
  const bool for_class = mdtype == Metaspace::ClassType ? true : false;
  VirtualSpaceList* const vsl = for_class ? Metaspace::class_space_list() : Metaspace::space_list();
  if (vsl != NULL) {
    if (for_class) {
      if (!Metaspace::using_class_space()) {
        out->print_cr("No Class Space.");
        return;
      }
      out->print_raw("---- Metaspace Map (Class Space) ----");
    } else {
      out->print_raw("---- Metaspace Map (Non-Class Space) ----");
    }
    // Print legend:
    out->cr();
    out->print_cr("Chunk Types (uppercase chunks are in use): x-specialized, s-small, m-medium, h-humongous.");
    out->cr();
    VirtualSpaceList* const vsl = for_class ? Metaspace::class_space_list() : Metaspace::space_list();
    vsl->print_map(out);
    out->cr();
  }
}

void MetaspaceUtils::verify_free_chunks() { }
void MetaspaceUtils::verify_metrics() { }

// Utils to check if a pointer or range is part of a committed metaspace region.
metaspace::VirtualSpaceNode* MetaspaceUtils::find_enclosing_virtual_space(const void* p) {
  VirtualSpaceNode* vsn = Metaspace::space_list()->find_enclosing_space(p);
  if (Metaspace::using_class_space() && vsn == NULL) {
    vsn = Metaspace::class_space_list()->find_enclosing_space(p);
  }
  return vsn;
}

bool MetaspaceUtils::is_in_committed(const void* p) {
  return find_enclosing_virtual_space(p) != NULL;
}

bool MetaspaceUtils::is_range_in_committed(const void* from, const void* to) {
  VirtualSpaceNode* vsn = find_enclosing_virtual_space(from);
  return (vsn != NULL) && vsn->contains(to);
}

// Metaspace methods

size_t Metaspace::_first_chunk_word_size = 0;
size_t Metaspace::_first_class_chunk_word_size = 0;

size_t Metaspace::_commit_alignment = 0;
size_t Metaspace::_reserve_alignment = 0;

VirtualSpaceList* Metaspace::_space_list = NULL;
VirtualSpaceList* Metaspace::_class_space_list = NULL;

ChunkManager* Metaspace::_chunk_manager_metadata = NULL;
ChunkManager* Metaspace::_chunk_manager_class = NULL;

#define VIRTUALSPACEMULTIPLIER 2

static const uint64_t UnscaledClassSpaceMax = (uint64_t(max_juint) + 1);

void Metaspace::set_narrow_klass_base_and_shift(address metaspace_base, address cds_base) {
  // Figure out the narrow_klass_base and the narrow_klass_shift.  The
  // narrow_klass_base is the lower of the metaspace base and the cds base
  // (if cds is enabled).  The narrow_klass_shift depends on the distance
  // between the lower base and higher address.
  address lower_base;
  address higher_address;
  {
    higher_address = metaspace_base + compressed_class_space_size();
    lower_base = metaspace_base;

    uint64_t klass_encoding_max = UnscaledClassSpaceMax << LogKlassAlignmentInBytes;
    // If compressed class space fits in lower 32G, we don't need a base.
    if (higher_address <= (address)klass_encoding_max) {
      lower_base = 0; // Effectively lower base is zero.
    }
  }

  Universe::set_narrow_klass_base(lower_base);

  // CDS uses LogKlassAlignmentInBytes for narrow_klass_shift. Although,
  // CDS can work with zero-shift mode also, to be consistent with AOT it uses
  // LogKlassAlignmentInBytes for klass shift so archived java heap objects
  // can be used at same time as AOT code.
  if ((uint64_t)(higher_address - lower_base) <= UnscaledClassSpaceMax) {
    Universe::set_narrow_klass_shift(0);
  } else {
    Universe::set_narrow_klass_shift(LogKlassAlignmentInBytes);
  }
}

// Try to allocate the metaspace at the requested addr.
void Metaspace::allocate_metaspace_compressed_klass_ptrs(char* requested_addr, address cds_base) {
  // Don't use large pages for the class space.
  bool large_pages = false;

#if !defined(AARCH64)
  ReservedSpace metaspace_rs = ReservedSpace(compressed_class_space_size(), _reserve_alignment, large_pages, requested_addr);
#else
  ReservedSpace metaspace_rs;

  // Our compressed klass pointers may fit nicely into the lower 32
  // bits.
  if ((uint64_t)requested_addr + compressed_class_space_size() < 4*G) {
    metaspace_rs = ReservedSpace(compressed_class_space_size(), _reserve_alignment, large_pages, requested_addr);
  }

  if (!metaspace_rs.is_reserved()) {
    // Aarch64: Try to align metaspace so that we can decode a compressed
    // klass with a single MOVK instruction.  We can do this iff the
    // compressed class base is a multiple of 4G.
    // Aix: Search for a place where we can find memory. If we need to load
    // the base, 4G alignment is helpful, too.
    size_t increment = AARCH64_ONLY(4*)G;
    for (char *a = align_up(requested_addr, increment); a < (char*)(1024*G); a += increment) {
      if (a == (char *)(32*G)) {
        // Go faster from here on. Zero-based is no longer possible.
        increment = 4*G;
      }

      metaspace_rs = ReservedSpace(compressed_class_space_size(), _reserve_alignment, large_pages, a);
      if (metaspace_rs.is_reserved())
        break;
    }
  }

#endif

  if (!metaspace_rs.is_reserved()) {
    // If no successful allocation then try to allocate the space anywhere.  If
    // that fails then OOM doom.  At this point we cannot try allocating the
    // metaspace as if UseCompressedClassPointers is off because too much
    // initialization has happened that depends on UseCompressedClassPointers.
    // So, UseCompressedClassPointers cannot be turned off at this point.
    if (!metaspace_rs.is_reserved()) {
      metaspace_rs = ReservedSpace(compressed_class_space_size(), _reserve_alignment, large_pages);
      if (!metaspace_rs.is_reserved()) {
        vm_exit_during_initialization(err_msg("Could not allocate metaspace: " SIZE_FORMAT " bytes", compressed_class_space_size()));
      }
    }
  }

  set_narrow_klass_base_and_shift((address)metaspace_rs.base(), 0);

  initialize_class_space(metaspace_rs);
}

void Metaspace::print_compressed_class_space(outputStream* st, const char* requested_addr) {
  st->print_cr("Narrow klass base: " PTR_FORMAT ", Narrow klass shift: %d", p2i(Universe::narrow_klass_base()), Universe::narrow_klass_shift());
  if (_class_space_list != NULL) {
    address base = (address)_class_space_list->current_virtual_space()->bottom();
    st->print("Compressed class space size: " SIZE_FORMAT " Address: " PTR_FORMAT, compressed_class_space_size(), p2i(base));
    if (requested_addr != 0) {
      st->print(" Req Addr: " PTR_FORMAT, p2i(requested_addr));
    }
    st->cr();
  }
}

// For UseCompressedClassPointers the class space is reserved above the top of
// the Java heap.  The argument passed in is at the base of the compressed space.
void Metaspace::initialize_class_space(ReservedSpace rs) {
  _class_space_list = new VirtualSpaceList(rs);
  _chunk_manager_class = new ChunkManager(true/*is_class*/);

  if (!_class_space_list->initialization_succeeded()) {
    vm_exit_during_initialization("Failed to setup compressed class space virtual space list.");
  }
}

void Metaspace::ergo_initialize() {
  size_t page_size = os::vm_page_size();
  if (UseLargePages && UseLargePagesInMetaspace) {
    page_size = os::large_page_size();
  }

  _commit_alignment  = page_size;
  _reserve_alignment = MAX2(page_size, (size_t)os::vm_allocation_granularity());

  // Do not use FLAG_SET_ERGO to update MaxMetaspaceSize, since this will
  // override if MaxMetaspaceSize was set on the command line or not.
  // This information is needed later to conform to the specification of the
  // java.lang.management.MemoryUsage API.
  //
  // Ideally, we would be able to set the default value of MaxMetaspaceSize in
  // globals.hpp to the aligned value, but this is not possible, since the
  // alignment depends on other flags being parsed.
  MaxMetaspaceSize = align_down_bounded(MaxMetaspaceSize, _reserve_alignment);

  if (MetaspaceSize > MaxMetaspaceSize) {
    MetaspaceSize = MaxMetaspaceSize;
  }

  MetaspaceSize = align_down_bounded(MetaspaceSize, _commit_alignment);

  MinMetaspaceExpansion = align_down_bounded(MinMetaspaceExpansion, _commit_alignment);
  MaxMetaspaceExpansion = align_down_bounded(MaxMetaspaceExpansion, _commit_alignment);

  CompressedClassSpaceSize = align_down_bounded(CompressedClassSpaceSize, _reserve_alignment);

  // Initial virtual space size will be calculated at global_initialize()
  size_t min_metaspace_sz = VIRTUALSPACEMULTIPLIER * InitialBootClassLoaderMetaspaceSize;
  if (UseCompressedClassPointers) {
    if ((min_metaspace_sz + CompressedClassSpaceSize) > MaxMetaspaceSize) {
      if (min_metaspace_sz >= MaxMetaspaceSize) {
        vm_exit_during_initialization("MaxMetaspaceSize is too small.");
      } else {
        FLAG_SET_ERGO(size_t, CompressedClassSpaceSize,
                      MaxMetaspaceSize - min_metaspace_sz);
      }
    }
  } else if (min_metaspace_sz >= MaxMetaspaceSize) {
    FLAG_SET_ERGO(size_t, InitialBootClassLoaderMetaspaceSize,
                  min_metaspace_sz);
  }

  set_compressed_class_space_size(CompressedClassSpaceSize);
}

void Metaspace::global_initialize() {
  MetaspaceGC::initialize();

  {
    if (using_class_space()) {
      char* base = (char*)align_up(Universe::heap()->reserved_region().end(), _reserve_alignment);
      allocate_metaspace_compressed_klass_ptrs(base, 0);
    }
  }

  // Initialize these before initializing the VirtualSpaceList
  _first_chunk_word_size = InitialBootClassLoaderMetaspaceSize / BytesPerWord;
  _first_chunk_word_size = align_word_size_up(_first_chunk_word_size);
  // Make the first class chunk bigger than a medium chunk so it's not put
  // on the medium chunk list.   The next chunk will be small and progress
  // from there.  This size calculated by -version.
  _first_class_chunk_word_size = MIN2((size_t)MediumChunk*6,
                                     (CompressedClassSpaceSize/BytesPerWord)*2);
  _first_class_chunk_word_size = align_word_size_up(_first_class_chunk_word_size);
  // Arbitrarily set the initial virtual space to a multiple
  // of the boot class loader size.
  size_t word_size = VIRTUALSPACEMULTIPLIER * _first_chunk_word_size;
  word_size = align_up(word_size, Metaspace::reserve_alignment_words());

  // Initialize the list of virtual spaces.
  _space_list = new VirtualSpaceList(word_size);
  _chunk_manager_metadata = new ChunkManager(false/*metaspace*/);

  if (!_space_list->initialization_succeeded()) {
    vm_exit_during_initialization("Unable to setup metadata virtual space list.", NULL);
  }
}

void Metaspace::post_initialize() {
  MetaspaceGC::post_initialize();
}

void Metaspace::verify_global_initialization() {
  if (using_class_space()) {
  }
}

size_t Metaspace::align_word_size_up(size_t word_size) {
  size_t byte_size = word_size * wordSize;
  return ReservedSpace::allocation_align_size_up(byte_size) / wordSize;
}

MetaWord* Metaspace::allocate(ClassLoaderData* loader_data, size_t word_size, MetaspaceObj::Type type, TRAPS) {
  if (HAS_PENDING_EXCEPTION) {
    ShouldNotReachHere();
    return NULL;  // caller does a CHECK_NULL too
  }

  MetadataType mdtype = (type == MetaspaceObj::ClassType) ? ClassType : NonClassType;

  // Try to allocate metadata.
  MetaWord* result = loader_data->metaspace_non_null()->allocate(word_size, mdtype);

  if (result == NULL) {
    // Allocation failed.
    if (is_init_completed()) {
      // Only start a GC if the bootstrapping has completed.
      // Also, we cannot GC if we are at the end of the CDS dumping stage which runs inside
      // the VM thread.

      // Try to clean out some memory and retry.
      result = Universe::heap()->satisfy_failed_metadata_allocation(loader_data, word_size, mdtype);
    }
  }

  if (result == NULL) {
    report_metadata_oome(loader_data, word_size, type, mdtype, THREAD);
    return NULL;
  }

  // Zero initialize.
  Copy::fill_to_words((HeapWord*)result, word_size, 0);

  return result;
}

void Metaspace::report_metadata_oome(ClassLoaderData* loader_data, size_t word_size, MetaspaceObj::Type type, MetadataType mdtype, TRAPS) {
  bool out_of_compressed_class_space = false;
  if (is_class_space_allocation(mdtype)) {
    ClassLoaderMetaspace* metaspace = loader_data->metaspace_non_null();
    out_of_compressed_class_space = MetaspaceUtils::committed_bytes(Metaspace::ClassType) + (metaspace->class_chunk_size(word_size) * BytesPerWord) > CompressedClassSpaceSize;
  }

  // -XX:OnOutOfMemoryError support
  const char* space_string = out_of_compressed_class_space ? "Compressed class space" : "Metaspace";

  report_java_out_of_memory(space_string);

  if (!is_init_completed()) {
    vm_exit_during_initialization("OutOfMemoryError", space_string);
  }

  if (out_of_compressed_class_space) {
    THROW_OOP(Universe::out_of_memory_error_class_metaspace());
  } else {
    THROW_OOP(Universe::out_of_memory_error_metaspace());
  }
}

const char* Metaspace::metadata_type_name(Metaspace::MetadataType mdtype) {
  switch (mdtype) {
    case Metaspace::ClassType: return "Class";
    case Metaspace::NonClassType: return "Metadata";
    default:
      ShouldNotReachHere();
      return NULL;
  }
}

void Metaspace::purge(MetadataType mdtype) {
  get_space_list(mdtype)->purge(get_chunk_manager(mdtype));
}

void Metaspace::purge() {
  MutexLockerEx cl(MetaspaceExpand_lock, Mutex::_no_safepoint_check_flag);
  purge(NonClassType);
  if (using_class_space()) {
    purge(ClassType);
  }
}

bool Metaspace::contains(const void* ptr) {
  return contains_non_shared(ptr);
}

bool Metaspace::contains_non_shared(const void* ptr) {
  if (using_class_space() && get_space_list(ClassType)->contains(ptr)) {
     return true;
  }

  return get_space_list(NonClassType)->contains(ptr);
}

// ClassLoaderMetaspace

ClassLoaderMetaspace::ClassLoaderMetaspace(Mutex* lock, Metaspace::MetaspaceType type)
  : _lock(lock)
  , _space_type(type)
  , _vsm(NULL)
  , _class_vsm(NULL)
{
  initialize(lock, type);
}

ClassLoaderMetaspace::~ClassLoaderMetaspace() {
  delete _vsm;
  if (Metaspace::using_class_space()) {
    delete _class_vsm;
  }
}

void ClassLoaderMetaspace::initialize_first_chunk(Metaspace::MetaspaceType type, Metaspace::MetadataType mdtype) {
  Metachunk* chunk = get_initialization_chunk(type, mdtype);
  if (chunk != NULL) {
    // Add to this manager's list of chunks in use and make it the current_chunk().
    get_space_manager(mdtype)->add_chunk(chunk, true);
  }
}

Metachunk* ClassLoaderMetaspace::get_initialization_chunk(Metaspace::MetaspaceType type, Metaspace::MetadataType mdtype) {
  size_t chunk_word_size = get_space_manager(mdtype)->get_initial_chunk_size(type);

  // Get a chunk from the chunk freelist
  Metachunk* chunk = Metaspace::get_chunk_manager(mdtype)->chunk_freelist_allocate(chunk_word_size);

  if (chunk == NULL) {
    chunk = Metaspace::get_space_list(mdtype)->get_new_chunk(chunk_word_size,
                                                  get_space_manager(mdtype)->medium_chunk_bunch());
  }

  return chunk;
}

void ClassLoaderMetaspace::initialize(Mutex* lock, Metaspace::MetaspaceType type) {
  Metaspace::verify_global_initialization();

  // Allocate SpaceManager for metadata objects.
  _vsm = new SpaceManager(Metaspace::NonClassType, type, lock);

  if (Metaspace::using_class_space()) {
    // Allocate SpaceManager for classes.
    _class_vsm = new SpaceManager(Metaspace::ClassType, type, lock);
  }

  MutexLockerEx cl(MetaspaceExpand_lock, Mutex::_no_safepoint_check_flag);

  // Allocate chunk for metadata objects
  initialize_first_chunk(type, Metaspace::NonClassType);

  // Allocate chunk for class metadata objects
  if (Metaspace::using_class_space()) {
    initialize_first_chunk(type, Metaspace::ClassType);
  }
}

MetaWord* ClassLoaderMetaspace::allocate(size_t word_size, Metaspace::MetadataType mdtype) {
  Metaspace::assert_not_frozen();

  // Don't use class_vsm() unless UseCompressedClassPointers is true.
  if (Metaspace::is_class_space_allocation(mdtype)) {
    return class_vsm()->allocate(word_size);
  } else {
    return vsm()->allocate(word_size);
  }
}

MetaWord* ClassLoaderMetaspace::expand_and_allocate(size_t word_size, Metaspace::MetadataType mdtype) {
  Metaspace::assert_not_frozen();
  size_t delta_bytes = MetaspaceGC::delta_capacity_until_GC(word_size * BytesPerWord);

  size_t before = 0;
  size_t after = 0;
  bool can_retry = true;
  MetaWord* res;
  bool incremented;

  // Each thread increments the HWM at most once. Even if the thread fails to increment
  // the HWM, an allocation is still attempted. This is because another thread must then
  // have incremented the HWM and therefore the allocation might still succeed.
  do {
    incremented = MetaspaceGC::inc_capacity_until_GC(delta_bytes, &after, &before, &can_retry);
    res = allocate(word_size, mdtype);
  } while (!incremented && res == NULL && can_retry);

  return res;
}

size_t ClassLoaderMetaspace::allocated_blocks_bytes() const {
  return (vsm()->used_words() + (Metaspace::using_class_space() ? class_vsm()->used_words() : 0)) * BytesPerWord;
}

size_t ClassLoaderMetaspace::allocated_chunks_bytes() const {
  return (vsm()->capacity_words() + (Metaspace::using_class_space() ? class_vsm()->capacity_words() : 0)) * BytesPerWord;
}

void ClassLoaderMetaspace::deallocate(MetaWord* ptr, size_t word_size, bool is_class) {
  Metaspace::assert_not_frozen();

  MutexLockerEx ml(vsm()->lock(), Mutex::_no_safepoint_check_flag);

  if (is_class && Metaspace::using_class_space()) {
    class_vsm()->deallocate(ptr, word_size);
  } else {
    vsm()->deallocate(ptr, word_size);
  }
}

size_t ClassLoaderMetaspace::class_chunk_size(size_t word_size) {
  return class_vsm()->calc_chunk_size(word_size);
}

void ClassLoaderMetaspace::print_on(outputStream* out) const {
  // Print both class virtual space counts and metaspace.
  if (Verbose) {
    vsm()->print_on(out);
    if (Metaspace::using_class_space()) {
      class_vsm()->print_on(out);
    }
  }
}

void ClassLoaderMetaspace::add_to_statistics_locked(ClassLoaderMetaspaceStatistics* out) const {
  vsm()->add_to_statistics_locked(&out->nonclass_sm_stats());
  if (Metaspace::using_class_space()) {
    class_vsm()->add_to_statistics_locked(&out->class_sm_stats());
  }
}

void ClassLoaderMetaspace::add_to_statistics(ClassLoaderMetaspaceStatistics* out) const {
  MutexLockerEx cl(lock(), Mutex::_no_safepoint_check_flag);
  add_to_statistics_locked(out);
}

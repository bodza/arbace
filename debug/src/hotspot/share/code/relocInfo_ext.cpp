#include "precompiled.hpp"

#include "code/codeCache.hpp"
#include "code/relocInfo.hpp"
#include "code/relocInfo_ext.hpp"
#include "gc/shared/cardTable.hpp"
#include "gc/shared/cardTableBarrierSet.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "memory/universe.hpp"
#include "runtime/os.hpp"
#include "utilities/debug.hpp"
#include "c1/c1_globals.hpp"

address symbolic_Relocation::symbolic_value(symbolic_Relocation::symbolic_reference t) {
  if (Universe::heap() == NULL) {
    // the symbolic values are not needed so early
    // (and most of them lead to errors if asked too early)
    return NULL;
  }
  switch (t) {
  case symbolic_Relocation::polling_page_reference: {
    return os::get_polling_page();
  }
  case symbolic_Relocation::eden_top_reference: {
    if (!Universe::heap()->supports_inline_contig_alloc()) {
      return NULL;
    }
    return (address)Universe::heap()->top_addr();
  }
  case symbolic_Relocation::heap_end_reference: {
    if (!Universe::heap()->supports_inline_contig_alloc()) {
      return NULL;
    }
    return (address)Universe::heap()->end_addr();
  }
  case symbolic_Relocation::card_table_reference: {
    BarrierSet* bs = BarrierSet::barrier_set();
    CardTableBarrierSet* ctbs = barrier_set_cast<CardTableBarrierSet>(bs);
    CardTable* ct = ctbs->card_table();
    return (address)ct->byte_map_base();
  }
  case symbolic_Relocation::mark_bits_reference: {
    return (address)Universe::verify_mark_bits();
  }
  case symbolic_Relocation::mark_mask_reference: {
    return (address)Universe::verify_mark_mask();
  }
  case symbolic_Relocation::oop_bits_reference: {
    return (address)Universe::verify_oop_bits();
  }
  case symbolic_Relocation::oop_mask_reference: {
    return (address)Universe::verify_oop_mask();
  }
  case symbolic_Relocation::debug_string_reference: {
    return (address)"<Lost debug string>";
  }
  default: {
    // missing declaration
    ShouldNotReachHere();
    return NULL;
  }
  }
}

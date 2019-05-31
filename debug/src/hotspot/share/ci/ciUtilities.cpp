#include "precompiled.hpp"
#include "ci/ciUtilities.hpp"
#include "gc/shared/cardTableBarrierSet.hpp"
#include "gc/shared/cardTable.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "memory/universe.hpp"

// ciUtilities
//
// Miscellaneous internal compiler interface routines.

// ------------------------------------------------------------------
// basictype_to_str
const char* basictype_to_str(BasicType t) {
  const char* str = type2name(t);
  if (str == NULL) return "illegal";
  return str;
}

// ------------------------------------------------------------------
// basictype_to_char
const char basictype_to_char(BasicType t) {
  char c = type2char(t);
  return c ? c : 'X';
}

// ------------------------------------------------------------------
// card_table_base
jbyte *ci_card_table_address() {
  BarrierSet* bs = BarrierSet::barrier_set();
  CardTableBarrierSet* ctbs = barrier_set_cast<CardTableBarrierSet>(bs);
  CardTable* ct = ctbs->card_table();
  return ct->byte_map_base();
}

#include "precompiled.hpp"

#include "gc/g1/heapRegion.hpp"
#include "gc/g1/heapRegionBounds.inline.hpp"
#include "gc/g1/heapRegionRemSet.hpp"
#include "gc/g1/sparsePRT.hpp"
#include "gc/shared/cardTableBarrierSet.hpp"
#include "gc/shared/space.inline.hpp"
#include "memory/allocation.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/mutexLocker.hpp"

void SparsePRTEntry::init(RegionIdx_t region_ind) {
  // Check that the card array element type can represent all cards in the region.
  // Choose a large SparsePRTEntry::card_elem_t (e.g. CardIdx_t) if required.
  _region_ind = region_ind;
  _next_index = RSHashTable::NullEntry;
  _next_null = 0;
}

bool SparsePRTEntry::contains_card(CardIdx_t card_index) const {
  for (int i = 0; i < num_valid_cards(); i++) {
    if (card(i) == card_index) {
      return true;
    }
  }
  return false;
}

SparsePRTEntry::AddCardResult SparsePRTEntry::add_card(CardIdx_t card_index) {
  for (int i = 0; i < num_valid_cards(); i++) {
    if (card(i) == card_index) {
      return found;
    }
  }
  if (num_valid_cards() < cards_num() - 1) {
    _cards[_next_null] = (card_elem_t)card_index;
    _next_null++;
    return added;
   }
  // Otherwise, we're full.
  return overflow;
}

void SparsePRTEntry::copy_cards(card_elem_t* cards) const {
  memcpy(cards, _cards, cards_num() * sizeof(card_elem_t));
}

void SparsePRTEntry::copy_cards(SparsePRTEntry* e) const {
  copy_cards(e->_cards);
  e->_next_null = _next_null;
}

// ----------------------------------------------------------------------

float RSHashTable::TableOccupancyFactor = 0.5f;

RSHashTable::RSHashTable(size_t capacity) :
  _capacity(capacity), _capacity_mask(capacity-1),
  _occupied_entries(0), _occupied_cards(0),
  _entries(NULL),
  _buckets(NEW_C_HEAP_ARRAY(int, capacity, mtGC)),
  _free_list(NullEntry), _free_region(0)
{
  _num_entries = (capacity * TableOccupancyFactor) + 1;
  _entries = (SparsePRTEntry*)NEW_C_HEAP_ARRAY(char, _num_entries * SparsePRTEntry::size(), mtGC);
  clear();
}

RSHashTable::~RSHashTable() {
  if (_entries != NULL) {
    FREE_C_HEAP_ARRAY(SparsePRTEntry, _entries);
    _entries = NULL;
  }
  if (_buckets != NULL) {
    FREE_C_HEAP_ARRAY(int, _buckets);
    _buckets = NULL;
  }
}

void RSHashTable::clear() {
  _occupied_entries = 0;
  _occupied_cards = 0;
  guarantee(_entries != NULL, "INV");
  guarantee(_buckets != NULL, "INV");

  guarantee(_capacity <= ((size_t)1 << (sizeof(int)*BitsPerByte-1)) - 1, "_capacity too large");

  // This will put -1 == NullEntry in the key field of all entries.
  memset((void*)_entries, NullEntry, _num_entries * SparsePRTEntry::size());
  memset((void*)_buckets, NullEntry, _capacity * sizeof(int));
  _free_list = NullEntry;
  _free_region = 0;
}

bool RSHashTable::add_card(RegionIdx_t region_ind, CardIdx_t card_index) {
  SparsePRTEntry* e = entry_for_region_ind_create(region_ind);
  SparsePRTEntry::AddCardResult res = e->add_card(card_index);
  if (res == SparsePRTEntry::added) _occupied_cards++;
  return res != SparsePRTEntry::overflow;
}

SparsePRTEntry* RSHashTable::get_entry(RegionIdx_t region_ind) const {
  int ind = (int) (region_ind & capacity_mask());
  int cur_ind = _buckets[ind];
  SparsePRTEntry* cur;
  while (cur_ind != NullEntry && (cur = entry(cur_ind))->r_ind() != region_ind) {
    cur_ind = cur->next_index();
  }

  if (cur_ind == NullEntry) return NULL;
  return cur;
}

bool RSHashTable::delete_entry(RegionIdx_t region_ind) {
  int ind = (int) (region_ind & capacity_mask());
  int* prev_loc = &_buckets[ind];
  int cur_ind = *prev_loc;
  SparsePRTEntry* cur;
  while (cur_ind != NullEntry && (cur = entry(cur_ind))->r_ind() != region_ind) {
    prev_loc = cur->next_index_addr();
    cur_ind = *prev_loc;
  }

  if (cur_ind == NullEntry) return false;
  // Otherwise, splice out "cur".
  *prev_loc = cur->next_index();
  _occupied_cards -= cur->num_valid_cards();
  free_entry(cur_ind);
  _occupied_entries--;
  return true;
}

SparsePRTEntry*
RSHashTable::entry_for_region_ind_create(RegionIdx_t region_ind) {
  SparsePRTEntry* res = get_entry(region_ind);
  if (res == NULL) {
    int new_ind = alloc_entry();
    res = entry(new_ind);
    res->init(region_ind);
    // Insert at front.
    int ind = (int) (region_ind & capacity_mask());
    res->set_next_index(_buckets[ind]);
    _buckets[ind] = new_ind;
    _occupied_entries++;
  }
  return res;
}

int RSHashTable::alloc_entry() {
  int res;
  if (_free_list != NullEntry) {
    res = _free_list;
    _free_list = entry(res)->next_index();
    return res;
  } else if ((size_t)_free_region < _num_entries) {
    res = _free_region;
    _free_region++;
    return res;
  } else {
    return NullEntry;
  }
}

void RSHashTable::free_entry(int fi) {
  entry(fi)->set_next_index(_free_list);
  _free_list = fi;
}

void RSHashTable::add_entry(SparsePRTEntry* e) {
  SparsePRTEntry* e2 = entry_for_region_ind_create(e->r_ind());
  e->copy_cards(e2);
  _occupied_cards += e2->num_valid_cards();
}

CardIdx_t RSHashTableIter::find_first_card_in_list() {
  while (_bl_ind != RSHashTable::NullEntry) {
    SparsePRTEntry* sparse_entry = _rsht->entry(_bl_ind);
    if (sparse_entry->num_valid_cards() > 0) {
      return sparse_entry->card(0);
    } else {
      _bl_ind = sparse_entry->next_index();
    }
  }
  // Otherwise, none found:
  return NoCardFound;
}

size_t RSHashTableIter::compute_card_ind(CardIdx_t ci) {
  return (_rsht->entry(_bl_ind)->r_ind() * HeapRegion::CardsPerRegion) + ci;
}

bool RSHashTableIter::has_next(size_t& card_index) {
  _card_ind++;
  if (_bl_ind >= 0) {
    SparsePRTEntry* e = _rsht->entry(_bl_ind);
    if (_card_ind < e->num_valid_cards()) {
      CardIdx_t ci = e->card(_card_ind);
      card_index = compute_card_ind(ci);
      return true;
    }
  }

  // Otherwise, must find the next valid entry.
  _card_ind = 0;

  if (_bl_ind != RSHashTable::NullEntry) {
      _bl_ind = _rsht->entry(_bl_ind)->next_index();
      CardIdx_t ci = find_first_card_in_list();
      if (ci != NoCardFound) {
        card_index = compute_card_ind(ci);
        return true;
      }
  }
  // If we didn't return above, must go to the next non-null table index.
  _tbl_ind++;
  while ((size_t)_tbl_ind < _rsht->capacity()) {
    _bl_ind = _rsht->_buckets[_tbl_ind];
    CardIdx_t ci = find_first_card_in_list();
    if (ci != NoCardFound) {
      card_index = compute_card_ind(ci);
      return true;
    }
    // Otherwise, try next entry.
    _tbl_ind++;
  }
  // Otherwise, there were no entry.
  return false;
}

bool RSHashTable::contains_card(RegionIdx_t region_index, CardIdx_t card_index) const {
  SparsePRTEntry* e = get_entry(region_index);
  return (e != NULL && e->contains_card(card_index));
}

size_t RSHashTable::mem_size() const {
  return sizeof(RSHashTable) + _num_entries * (SparsePRTEntry::size() + sizeof(int));
}

// ----------------------------------------------------------------------

SparsePRT* volatile SparsePRT::_head_expanded_list = NULL;

void SparsePRT::add_to_expanded_list(SparsePRT* sprt) {
  // We could expand multiple times in a pause -- only put on list once.
  if (sprt->expanded()) return;
  sprt->set_expanded(true);
  SparsePRT* hd = _head_expanded_list;
  while (true) {
    sprt->_next_expanded = hd;
    SparsePRT* res = Atomic::cmpxchg(sprt, &_head_expanded_list, hd);
    if (res == hd) return;
    else hd = res;
  }
}

SparsePRT* SparsePRT::get_from_expanded_list() {
  SparsePRT* hd = _head_expanded_list;
  while (hd != NULL) {
    SparsePRT* next = hd->next_expanded();
    SparsePRT* res = Atomic::cmpxchg(next, &_head_expanded_list, hd);
    if (res == hd) {
      hd->set_next_expanded(NULL);
      return hd;
    } else {
      hd = res;
    }
  }
  return NULL;
}

void SparsePRT::reset_for_cleanup_tasks() {
  _head_expanded_list = NULL;
}

void SparsePRT::do_cleanup_work(SparsePRTCleanupTask* sprt_cleanup_task) {
  if (should_be_on_expanded_list()) {
    sprt_cleanup_task->add(this);
  }
}

void SparsePRT::finish_cleanup_task(SparsePRTCleanupTask* sprt_cleanup_task) {
  SparsePRT* head = sprt_cleanup_task->head();
  SparsePRT* tail = sprt_cleanup_task->tail();
  if (head != NULL) {
    tail->set_next_expanded(_head_expanded_list);
    _head_expanded_list = head;
  }
}

bool SparsePRT::should_be_on_expanded_list() {
  return expanded();
}

void SparsePRT::cleanup_all() {
  // First clean up all expanded tables so they agree on next and cur.
  SparsePRT* sprt = get_from_expanded_list();
  while (sprt != NULL) {
    sprt->cleanup();
    sprt = get_from_expanded_list();
  }
}

SparsePRT::SparsePRT(HeapRegion* hr) : _hr(hr), _expanded(false), _next_expanded(NULL) {
  _cur = new RSHashTable(InitialCapacity);
  _next = _cur;
}

SparsePRT::~SparsePRT() {
  if (_cur != _next) { delete _cur; }
  delete _next;
}

size_t SparsePRT::mem_size() const {
  // We ignore "_cur" here, because it either = _next, or else it is
  // on the deleted list.
  return sizeof(SparsePRT) + _next->mem_size();
}

bool SparsePRT::add_card(RegionIdx_t region_id, CardIdx_t card_index) {
  if (_next->should_expand()) {
    expand();
  }
  return _next->add_card(region_id, card_index);
}

SparsePRTEntry* SparsePRT::get_entry(RegionIdx_t region_id) {
  return _next->get_entry(region_id);
}

bool SparsePRT::delete_entry(RegionIdx_t region_id) {
  return _next->delete_entry(region_id);
}

void SparsePRT::clear() {
  // If they differ, _next is bigger then cur, so next has no chance of
  // being the initial size.
  if (_next != _cur) {
    delete _next;
  }

  if (_cur->capacity() != InitialCapacity) {
    delete _cur;
    _cur = new RSHashTable(InitialCapacity);
  } else {
    _cur->clear();
  }
  _next = _cur;
  _expanded = false;
}

void SparsePRT::cleanup() {
  // Make sure that the current and next tables agree.
  if (_cur != _next) {
    delete _cur;
  }
  _cur = _next;
  set_expanded(false);
}

void SparsePRT::expand() {
  RSHashTable* last = _next;
  _next = new RSHashTable(last->capacity() * 2);
  for (size_t i = 0; i < last->num_entries(); i++) {
    SparsePRTEntry* e = last->entry((int)i);
    if (e->valid_entry()) {
      _next->add_entry(e);
    }
  }
  if (last != _cur) {
    delete last;
  }
  add_to_expanded_list(this);
}

void SparsePRTCleanupTask::add(SparsePRT* sprt) {
  sprt->set_next_expanded(NULL);
  if (_tail != NULL) {
    _tail->set_next_expanded(sprt);
  } else {
    _head = sprt;
  }
  _tail = sprt;
}

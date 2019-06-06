#ifndef SHARE_MEMORY_FREELIST_INLINE_HPP
#define SHARE_MEMORY_FREELIST_INLINE_HPP

#include "gc/shared/collectedHeap.hpp"
#include "memory/freeList.hpp"
#include "runtime/globals.hpp"
#include "runtime/mutex.hpp"
#include "runtime/vmThread.hpp"
#include "utilities/macros.hpp"

// Free list.  A FreeList is used to access a linked list of chunks
// of space in the heap.  The head and tail are maintained so that
// items can be (as in the current implementation) added at the
// at the tail of the list and removed from the head of the list to
// maintain a FIFO queue.

template <class Chunk>
FreeList<Chunk>::FreeList() : _head(NULL), _tail(NULL) {
  _size         = 0;
  _count        = 0;
}

template <class Chunk>
void FreeList<Chunk>::link_head(Chunk* v) {
  assert_proper_lock_protection();
  set_head(v);
  // If this method is not used (just set the head instead),
  // this check can be avoided.
  if (v != NULL) {
    v->link_prev(NULL);
  }
}

template <class Chunk>
void FreeList<Chunk>::reset() {
  // Don't set the _size to 0 because this method is
  // used with a existing list that has a size but which has
  // been emptied.
  // Don't clear the _protecting_lock of an existing list.
  set_count(0);
  set_head(NULL);
  set_tail(NULL);
}

template <class Chunk>
void FreeList<Chunk>::initialize() {
  reset();
  set_size(0);
}

template <class Chunk_t>
Chunk_t* FreeList<Chunk_t>::get_chunk_at_head() {
  assert_proper_lock_protection();
  Chunk_t* fc = head();
  if (fc != NULL) {
    Chunk_t* nextFC = fc->next();
    if (nextFC != NULL) {
      // The chunk fc being removed has a "next".  Set the "next" to the
      // "prev" of fc.
      nextFC->link_prev(NULL);
    } else { // removed tail of list
      link_tail(NULL);
    }
    link_head(nextFC);
    decrement_count();
  }
  return fc;
}

template <class Chunk>
void FreeList<Chunk>::getFirstNChunksFromList(size_t n, FreeList<Chunk>* fl) {
  assert_proper_lock_protection();
  if (count() > 0) {
    int k = 1;
    fl->set_head(head()); n--;
    Chunk* tl = head();
    while (tl->next() != NULL && n > 0) {
      tl = tl->next(); n--; k++;
    }

    // First, fix up the list we took from.
    Chunk* new_head = tl->next();
    set_head(new_head);
    set_count(count() - k);
    if (new_head == NULL) {
      set_tail(NULL);
    } else {
      new_head->link_prev(NULL);
    }
    // Now we can fix up the tail.
    tl->link_next(NULL);
    // And return the result.
    fl->set_tail(tl);
    fl->set_count(k);
  }
}

// Remove this chunk from the list
template <class Chunk>
void FreeList<Chunk>::remove_chunk(Chunk*fc) {
   assert_proper_lock_protection();

   Chunk* prevFC = fc->prev();
   Chunk* nextFC = fc->next();
   if (nextFC != NULL) {
     // The chunk fc being removed has a "next".  Set the "next" to the
     // "prev" of fc.
     nextFC->link_prev(prevFC);
   } else { // removed tail of list
     link_tail(prevFC);
   }
   if (prevFC == NULL) { // removed head of list
     link_head(nextFC);
   } else {
     prevFC->link_next(nextFC);
   }
   decrement_count();
}

// Add this chunk at the head of the list.
template <class Chunk>
void FreeList<Chunk>::return_chunk_at_head(Chunk* chunk, bool record_return) {
  assert_proper_lock_protection();

  Chunk* oldHead = head();
  chunk->link_after(oldHead);
  link_head(chunk);
  if (oldHead == NULL) { // only chunk in list
    link_tail(chunk);
  }
  increment_count(); // of # of chunks in list
}

template <class Chunk>
void FreeList<Chunk>::return_chunk_at_head(Chunk* chunk) {
  assert_proper_lock_protection();
  return_chunk_at_head(chunk, true);
}

// Add this chunk at the tail of the list.
template <class Chunk>
void FreeList<Chunk>::return_chunk_at_tail(Chunk* chunk, bool record_return) {
  assert_proper_lock_protection();

  Chunk* oldTail = tail();
  if (oldTail != NULL) {
    oldTail->link_after(chunk);
  } else { // only chunk in list
    link_head(chunk);
  }
  link_tail(chunk);
  increment_count();  // of # of chunks in list
}

template <class Chunk>
void FreeList<Chunk>::return_chunk_at_tail(Chunk* chunk) {
  return_chunk_at_tail(chunk, true);
}

template <class Chunk>
void FreeList<Chunk>::prepend(FreeList<Chunk>* fl) {
  assert_proper_lock_protection();
  if (fl->count() > 0) {
    if (count() == 0) {
      set_head(fl->head());
      set_tail(fl->tail());
      set_count(fl->count());
    } else {
      // Both are non-empty.
      Chunk* fl_tail = fl->tail();
      Chunk* this_head = head();
      fl_tail->link_next(this_head);
      this_head->link_prev(fl_tail);
      set_head(fl->head());
      set_count(count() + fl->count());
    }
    fl->set_head(NULL);
    fl->set_tail(NULL);
    fl->set_count(0);
  }
}

// verify_chunk_in_free_lists() is used to verify that an item is in this free list.
// It is used as a debugging aid.
template <class Chunk>
bool FreeList<Chunk>::verify_chunk_in_free_list(Chunk* fc) const {
  // This is an internal consistency check, not part of the check that the
  // chunk is in the free lists.
  guarantee(fc->size() == size(), "Wrong list is being searched");
  Chunk* curFC = head();
  while (curFC) {
    // This is an internal consistency check.
    guarantee(size() == curFC->size(), "Chunk is in wrong list.");
    if (fc == curFC) {
      return true;
    }
    curFC = curFC->next();
  }
  return false;
}

// Print the "label line" for free list stats.
template <class Chunk>
void FreeList<Chunk>::print_labels_on(outputStream* st, const char* c) {
  st->print("%16s\t", c);
  st->print("%14s\t"    "%14s\t"    "%14s\t"    "%14s\t"    "%14s\t"
            "%14s\t"    "%14s\t"    "%14s\t"    "%14s\t"    "%14s\t"    "\n",
            "bfrsurp", "surplus", "desired", "prvSwep", "bfrSwep",
            "count",   "cBirths", "cDeaths", "sBirths", "sDeaths");
}

// Print the AllocationStats for the given free list. If the second argument
// to the call is a non-null string, it is printed in the first column;
// otherwise, if the argument is null (the default), then the size of the
// (free list) block is printed in the first column.
template <class Chunk_t>
void FreeList<Chunk_t>::print_on(outputStream* st, const char* c) const {
  if (c != NULL) {
    st->print("%16s", c);
  } else {
    st->print(SIZE_FORMAT_W(16), size());
  }
}

#endif

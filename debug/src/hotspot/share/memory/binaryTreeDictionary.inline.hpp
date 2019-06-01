#ifndef SHARE_VM_MEMORY_BINARYTREEDICTIONARY_INLINE_HPP
#define SHARE_VM_MEMORY_BINARYTREEDICTIONARY_INLINE_HPP

#include "gc/shared/spaceDecorator.hpp"
#include "logging/log.hpp"
#include "logging/logStream.hpp"
#include "memory/binaryTreeDictionary.hpp"
#include "memory/freeList.inline.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/mutex.hpp"
#include "runtime/globals.hpp"
#include "utilities/macros.hpp"
#include "utilities/ostream.hpp"

////////////////////////////////////////////////////////////////////////////////
// A binary tree based search structure for free blocks.
// This is currently used in the Concurrent Mark&Sweep implementation.
////////////////////////////////////////////////////////////////////////////////

template <class Chunk_t, class FreeList_t>
TreeChunk<Chunk_t, FreeList_t>* TreeChunk<Chunk_t, FreeList_t>::as_TreeChunk(Chunk_t* fc) {
  // Do some assertion checking here.
  return (TreeChunk<Chunk_t, FreeList_t>*) fc;
}

template <class Chunk_t, class FreeList_t>
void TreeChunk<Chunk_t, FreeList_t>::verify_tree_chunk_list() const {
  TreeChunk<Chunk_t, FreeList_t>* nextTC = (TreeChunk<Chunk_t, FreeList_t>*)next();
  if (prev() != NULL) { // interior list node shouldn't have tree fields
    guarantee(embedded_list()->parent() == NULL && embedded_list()->left() == NULL && embedded_list()->right()  == NULL, "should be clear");
  }
  if (nextTC != NULL) {
    guarantee(as_TreeChunk(nextTC->prev()) == this, "broken chain");
    guarantee(nextTC->size() == size(), "wrong size");
    nextTC->verify_tree_chunk_list();
  }
}

template <class Chunk_t, class FreeList_t>
TreeList<Chunk_t, FreeList_t>::TreeList() : _parent(NULL), _left(NULL), _right(NULL) { }

template <class Chunk_t, class FreeList_t>
TreeList<Chunk_t, FreeList_t>*
TreeList<Chunk_t, FreeList_t>::as_TreeList(TreeChunk<Chunk_t,FreeList_t>* tc) {
  TreeList<Chunk_t, FreeList_t>* tl = tc->embedded_list();
  tl->initialize();
  tc->set_list(tl);
  tl->set_size(tc->size());
  tl->link_head(tc);
  tl->link_tail(tc);
  tl->set_count(1);
  return tl;
}

template <class Chunk_t, class FreeList_t>
TreeList<Chunk_t, FreeList_t>*
TreeList<Chunk_t, FreeList_t>::as_TreeList(HeapWord* addr, size_t size) {
  TreeChunk<Chunk_t, FreeList_t>* tc = (TreeChunk<Chunk_t, FreeList_t>*) addr;
  // The space will have been mangled initially but
  // is not remangled when a Chunk_t is returned to the free list
  // (since it is used to maintain the chunk on the free list).
  tc->assert_is_mangled();
  tc->set_size(size);
  tc->link_prev(NULL);
  tc->link_next(NULL);
  TreeList<Chunk_t, FreeList_t>* tl = TreeList<Chunk_t, FreeList_t>::as_TreeList(tc);
  return tl;
}

template <class Chunk_t, class FreeList_t>
TreeList<Chunk_t, FreeList_t>*
TreeList<Chunk_t, FreeList_t>::get_better_list(BinaryTreeDictionary<Chunk_t, FreeList_t>* dictionary) {
  return this;
}

template <class Chunk_t, class FreeList_t>
TreeList<Chunk_t, FreeList_t>* TreeList<Chunk_t, FreeList_t>::remove_chunk_replace_if_needed(TreeChunk<Chunk_t, FreeList_t>* tc) {

  TreeList<Chunk_t, FreeList_t>* retTL = this;
  Chunk_t* list = head();

  Chunk_t* prevFC = tc->prev();
  TreeChunk<Chunk_t, FreeList_t>* nextTC = TreeChunk<Chunk_t, FreeList_t>::as_TreeChunk(tc->next());

  // Is this the first item on the list?
  if (tc == list) {
    // The "getChunk..." functions for a TreeList<Chunk_t, FreeList_t> will not return the
    // first chunk in the list unless it is the last chunk in the list
    // because the first chunk is also acting as the tree node.
    // When coalescing happens, however, the first chunk in the a tree
    // list can be the start of a free range.  Free ranges are removed
    // from the free lists so that they are not available to be
    // allocated when the sweeper yields (giving up the free list lock)
    // to allow mutator activity.  If this chunk is the first in the
    // list and is not the last in the list, do the work to copy the
    // TreeList<Chunk_t, FreeList_t> from the first chunk to the next chunk and update all
    // the TreeList<Chunk_t, FreeList_t> pointers in the chunks in the list.
    if (nextTC == NULL) {
      set_tail(NULL);
      set_head(NULL);
    } else {
      // copy embedded list.
      nextTC->set_embedded_list(tc->embedded_list());
      retTL = nextTC->embedded_list();
      // Fix the pointer to the list in each chunk in the list.
      // This can be slow for a long list.  Consider having
      // an option that does not allow the first chunk on the
      // list to be coalesced.
      for (TreeChunk<Chunk_t, FreeList_t>* curTC = nextTC; curTC != NULL; curTC = TreeChunk<Chunk_t, FreeList_t>::as_TreeChunk(curTC->next())) {
        curTC->set_list(retTL);
      }
      // Fix the parent to point to the new TreeList<Chunk_t, FreeList_t>.
      if (retTL->parent() != NULL) {
        if (this == retTL->parent()->left()) {
          retTL->parent()->set_left(retTL);
        } else {
          retTL->parent()->set_right(retTL);
        }
      }
      // Fix the children's parent pointers to point to the new list.
      if (retTL->right() != NULL) {
        retTL->right()->set_parent(retTL);
      }
      if (retTL->left() != NULL) {
        retTL->left()->set_parent(retTL);
      }
      retTL->link_head(nextTC);
    }
  } else {
    if (nextTC == NULL) {
      // Removing chunk at tail of list
      this->link_tail(prevFC);
    }
    // Chunk is interior to the list
    prevFC->link_after(nextTC);
  }

  // Below this point the embedded TreeList<Chunk_t, FreeList_t> being used for the
  // tree node may have changed. Don't use "this"
  // TreeList<Chunk_t, FreeList_t>*.
  // chunk should still be a free chunk (bit set in _prev)
  retTL->decrement_count();

  return retTL;
}

template <class Chunk_t, class FreeList_t>
void TreeList<Chunk_t, FreeList_t>::return_chunk_at_tail(TreeChunk<Chunk_t, FreeList_t>* chunk) {
  Chunk_t* fc = tail();
  fc->link_after(chunk);
  this->link_tail(chunk);

  FreeList_t::increment_count();
}

// Add this chunk at the head of the list.  "At the head of the list"
// is defined to be after the chunk pointer to by head().  This is
// because the TreeList<Chunk_t, FreeList_t> is embedded in the first TreeChunk<Chunk_t, FreeList_t> in the
// list.  See the definition of TreeChunk<Chunk_t, FreeList_t>.
template <class Chunk_t, class FreeList_t>
void TreeList<Chunk_t, FreeList_t>::return_chunk_at_head(TreeChunk<Chunk_t, FreeList_t>* chunk) {

  Chunk_t* fc = head()->next();
  if (fc != NULL) {
    chunk->link_after(fc);
  } else {
    this->link_tail(chunk);
  }
  head()->link_after(chunk);
  FreeList_t::increment_count();
}

template <class Chunk_t, class FreeList_t>
void TreeChunk<Chunk_t, FreeList_t>::assert_is_mangled() const { }

template <class Chunk_t, class FreeList_t>
TreeChunk<Chunk_t, FreeList_t>* TreeList<Chunk_t, FreeList_t>::head_as_TreeChunk() {
  return TreeChunk<Chunk_t, FreeList_t>::as_TreeChunk(head());
}

template <class Chunk_t, class FreeList_t>
TreeChunk<Chunk_t, FreeList_t>* TreeList<Chunk_t, FreeList_t>::first_available() {
  Chunk_t* fc = head()->next();
  TreeChunk<Chunk_t, FreeList_t>* retTC;
  if (fc == NULL) {
    retTC = head_as_TreeChunk();
  } else {
    retTC = TreeChunk<Chunk_t, FreeList_t>::as_TreeChunk(fc);
  }
  return retTC;
}

// Returns the block with the largest heap address amongst
// those in the list for this size; potentially slow and expensive,
// use with caution!
template <class Chunk_t, class FreeList_t>
TreeChunk<Chunk_t, FreeList_t>* TreeList<Chunk_t, FreeList_t>::largest_address() {
  Chunk_t* fc = head()->next();
  TreeChunk<Chunk_t, FreeList_t>* retTC;
  if (fc == NULL) {
    retTC = head_as_TreeChunk();
  } else {
    // walk down the list and return the one with the highest
    // heap address among chunks of this size.
    Chunk_t* last = fc;
    while (fc->next() != NULL) {
      if ((HeapWord*)last < (HeapWord*)fc) {
        last = fc;
      }
      fc = fc->next();
    }
    retTC = TreeChunk<Chunk_t, FreeList_t>::as_TreeChunk(last);
  }
  return retTC;
}

template <class Chunk_t, class FreeList_t>
BinaryTreeDictionary<Chunk_t, FreeList_t>::BinaryTreeDictionary(MemRegion mr) {

  reset(mr);
}

template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::inc_total_size(size_t inc) {
  _total_size = _total_size + inc;
}

template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::dec_total_size(size_t dec) {
  _total_size = _total_size - dec;
}

template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::reset(MemRegion mr) {
  set_root(TreeList<Chunk_t, FreeList_t>::as_TreeList(mr.start(), mr.word_size()));
  set_total_size(mr.word_size());
  set_total_free_blocks(1);
}

template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::reset(HeapWord* addr, size_t byte_size) {
  MemRegion mr(addr, heap_word_size(byte_size));
  reset(mr);
}

template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::reset() {
  set_root(NULL);
  set_total_size(0);
  set_total_free_blocks(0);
}

// Get a free block of size at least size from tree, or NULL.
template <class Chunk_t, class FreeList_t>
TreeChunk<Chunk_t, FreeList_t>*
BinaryTreeDictionary<Chunk_t, FreeList_t>::get_chunk_from_tree(size_t size)
{
  TreeList<Chunk_t, FreeList_t> *curTL, *prevTL;
  TreeChunk<Chunk_t, FreeList_t>* retTC = NULL;

  if (FLSVerifyDictionary) {
    verify_tree();
  }
  // starting at the root, work downwards trying to find match.
  // Remember the last node of size too great or too small.
  for (prevTL = curTL = root(); curTL != NULL;) {
    if (curTL->size() == size) {        // exact match
      break;
    }
    prevTL = curTL;
    if (curTL->size() < size) {        // proceed to right sub-tree
      curTL = curTL->right();
    } else {                           // proceed to left sub-tree
      curTL = curTL->left();
    }
  }
  if (curTL == NULL) { // couldn't find exact match

    // try and find the next larger size by walking back up the search path
    for (curTL = prevTL; curTL != NULL;) {
      if (curTL->size() >= size) break;
      else curTL = curTL->parent();
    }
  }
  if (curTL != NULL) {

    curTL = curTL->get_better_list(this);

    retTC = curTL->first_available();
    remove_chunk_from_tree(retTC);
  }

  if (FLSVerifyDictionary) {
    verify();
  }
  return retTC;
}

template <class Chunk_t, class FreeList_t>
TreeList<Chunk_t, FreeList_t>* BinaryTreeDictionary<Chunk_t, FreeList_t>::find_list(size_t size) const {
  TreeList<Chunk_t, FreeList_t>* curTL;
  for (curTL = root(); curTL != NULL;) {
    if (curTL->size() == size) {        // exact match
      break;
    }

    if (curTL->size() < size) {        // proceed to right sub-tree
      curTL = curTL->right();
    } else {                           // proceed to left sub-tree
      curTL = curTL->left();
    }
  }
  return curTL;
}

template <class Chunk_t, class FreeList_t>
bool BinaryTreeDictionary<Chunk_t, FreeList_t>::verify_chunk_in_free_list(Chunk_t* tc) const {
  size_t size = tc->size();
  TreeList<Chunk_t, FreeList_t>* tl = find_list(size);
  if (tl == NULL) {
    return false;
  } else {
    return tl->verify_chunk_in_free_list(tc);
  }
}

template <class Chunk_t, class FreeList_t>
Chunk_t* BinaryTreeDictionary<Chunk_t, FreeList_t>::find_largest_dict() const {
  TreeList<Chunk_t, FreeList_t> *curTL = root();
  if (curTL != NULL) {
    while (curTL->right() != NULL) curTL = curTL->right();
    return curTL->largest_address();
  } else {
    return NULL;
  }
}

// Remove the current chunk from the tree.  If it is not the last
// chunk in a list on a tree node, just unlink it.
// If it is the last chunk in the list (the next link is NULL),
// remove the node and repair the tree.
template <class Chunk_t, class FreeList_t>
TreeChunk<Chunk_t, FreeList_t>*
BinaryTreeDictionary<Chunk_t, FreeList_t>::remove_chunk_from_tree(TreeChunk<Chunk_t, FreeList_t>* tc) {

  TreeList<Chunk_t, FreeList_t> *newTL, *parentTL;
  TreeChunk<Chunk_t, FreeList_t>* retTC;
  TreeList<Chunk_t, FreeList_t>* tl = tc->list();

  bool complicated_splice = false;

  retTC = tc;
  // Removing this chunk can have the side effect of changing the node
  // (TreeList<Chunk_t, FreeList_t>*) in the tree.  If the node is the root, update it.
  TreeList<Chunk_t, FreeList_t>* replacementTL = tl->remove_chunk_replace_if_needed(tc);
  if (tl == root()) {
    set_root(replacementTL);
  }

  // Does the tree need to be repaired?
  if (replacementTL->count() == 0) {
    // Find the replacement node for the (soon to be empty) node being removed.
    // if we have a single (or no) child, splice child in our stead
    if (replacementTL->left() == NULL) {
      // left is NULL so pick right.  right may also be NULL.
      newTL = replacementTL->right();
    } else if (replacementTL->right() == NULL) {
      // right is NULL
      newTL = replacementTL->left();
    } else {  // we have both children, so, by patriarchal convention,
              // my replacement is least node in right sub-tree
      complicated_splice = true;
      newTL = remove_tree_minimum(replacementTL->right());
    }
    // newTL is the replacement for the (soon to be empty) node.
    // newTL may be NULL.
    // should verify; we just cleanly excised our replacement
    if (FLSVerifyDictionary) {
      verify_tree();
    }
    // first make newTL my parent's child
    if ((parentTL = replacementTL->parent()) == NULL) {
      // newTL should be root
      set_root(newTL);
      if (newTL != NULL) {
        newTL->clear_parent();
      }
    } else if (parentTL->right() == replacementTL) {
      // replacementTL is a right child
      parentTL->set_right(newTL);
    } else {                                // replacementTL is a left child
      parentTL->set_left(newTL);
    }
    if (complicated_splice) {  // we need newTL to get replacementTL's two children
      newTL->set_left(replacementTL->left());
      newTL->set_right(replacementTL->right());
    }
  }

  dec_total_size(retTC->size());     // size book-keeping
  set_total_free_blocks(total_free_blocks() - 1);

  if (FLSVerifyDictionary) {
    verify_tree();
  }
  return TreeChunk<Chunk_t, FreeList_t>::as_TreeChunk(retTC);
}

// Remove the leftmost node (lm) in the tree and return it.
// If lm has a right child, link it to the left node of
// the parent of lm.
template <class Chunk_t, class FreeList_t>
TreeList<Chunk_t, FreeList_t>* BinaryTreeDictionary<Chunk_t, FreeList_t>::remove_tree_minimum(TreeList<Chunk_t, FreeList_t>* tl) {
  // locate the subtree minimum by walking down left branches
  TreeList<Chunk_t, FreeList_t>* curTL = tl;
  for (; curTL->left() != NULL; curTL = curTL->left())
    ;
  // obviously curTL now has at most one child, a right child
  if (curTL != root()) {  // Should this test just be removed?
    TreeList<Chunk_t, FreeList_t>* parentTL = curTL->parent();
    if (parentTL->left() == curTL) { // curTL is a left child
      parentTL->set_left(curTL->right());
    } else {
      // If the list tl has no left child, then curTL may be
      // the right child of parentTL.
      parentTL->set_right(curTL->right());
    }
  } else {
    // The only use of this method would not pass the root of the
    // tree (as indicated by the assertion above that the tree list
    // has a parent) but the specification does not explicitly exclude the
    // passing of the root so accommodate it.
    set_root(NULL);
  }
  // we just excised a (non-root) node, we should still verify all tree invariants
  if (FLSVerifyDictionary) {
    verify_tree();
  }
  return curTL;
}

template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::insert_chunk_in_tree(Chunk_t* fc) {
  TreeList<Chunk_t, FreeList_t> *curTL, *prevTL;
  size_t size = fc->size();

  if (FLSVerifyDictionary) {
    verify_tree();
  }

  fc->clear_next();
  fc->link_prev(NULL);

  // work down from the _root, looking for insertion point
  for (prevTL = curTL = root(); curTL != NULL;) {
    if (curTL->size() == size)  // exact match
      break;
    prevTL = curTL;
    if (curTL->size() > size) { // follow left branch
      curTL = curTL->left();
    } else {                    // follow right branch
      curTL = curTL->right();
    }
  }
  TreeChunk<Chunk_t, FreeList_t>* tc = TreeChunk<Chunk_t, FreeList_t>::as_TreeChunk(fc);
  // This chunk is being returned to the binary tree.  Its embedded
  // TreeList<Chunk_t, FreeList_t> should be unused at this point.
  tc->initialize();
  if (curTL != NULL) {          // exact match
    tc->set_list(curTL);
    curTL->return_chunk_at_tail(tc);
  } else {                     // need a new node in tree
    tc->clear_next();
    tc->link_prev(NULL);
    TreeList<Chunk_t, FreeList_t>* newTL = TreeList<Chunk_t, FreeList_t>::as_TreeList(tc);
    if (prevTL == NULL) {      // we are the only tree node
      set_root(newTL);
    } else {                   // insert under prevTL ...
      if (prevTL->size() < size) {   // am right child
        prevTL->set_right(newTL);
      } else {                       // am left child
        prevTL->set_left(newTL);
      }
    }
  }

  inc_total_size(size);
  // Method 'total_size_in_tree' walks through the every block in the
  // tree, so it can cause significant performance loss if there are
  // many blocks in the tree
  set_total_free_blocks(total_free_blocks() + 1);
  if (FLSVerifyDictionary) {
    verify_tree();
  }
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::max_chunk_size() const {
  verify_par_locked();
  TreeList<Chunk_t, FreeList_t>* tc = root();
  if (tc == NULL) return 0;
  for (; tc->right() != NULL; tc = tc->right());
  return tc->size();
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::total_list_length(TreeList<Chunk_t, FreeList_t>* tl) const {
  size_t res;
  res = tl->count();
  return res;
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::total_size_in_tree(TreeList<Chunk_t, FreeList_t>* tl) const {
  if (tl == NULL)
    return 0;
  return (tl->size() * total_list_length(tl)) + total_size_in_tree(tl->left())    + total_size_in_tree(tl->right());
}

template <class Chunk_t, class FreeList_t>
double BinaryTreeDictionary<Chunk_t, FreeList_t>::sum_of_squared_block_sizes(TreeList<Chunk_t, FreeList_t>* const tl) const {
  if (tl == NULL) {
    return 0.0;
  }
  double size = (double)(tl->size());
  double curr = size * size * total_list_length(tl);
  curr += sum_of_squared_block_sizes(tl->left());
  curr += sum_of_squared_block_sizes(tl->right());
  return curr;
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::total_free_blocks_in_tree(TreeList<Chunk_t, FreeList_t>* tl) const {
  if (tl == NULL)
    return 0;
  return total_list_length(tl) + total_free_blocks_in_tree(tl->left()) + total_free_blocks_in_tree(tl->right());
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::num_free_blocks() const {
  return total_free_blocks();
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::tree_height_helper(TreeList<Chunk_t, FreeList_t>* tl) const {
  if (tl == NULL)
    return 0;
  return 1 + MAX2(tree_height_helper(tl->left()),
                  tree_height_helper(tl->right()));
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::tree_height() const {
  return tree_height_helper(root());
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::total_nodes_helper(TreeList<Chunk_t, FreeList_t>* tl) const {
  if (tl == NULL) {
    return 0;
  }
  return 1 + total_nodes_helper(tl->left()) + total_nodes_helper(tl->right());
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::total_nodes_in_tree(TreeList<Chunk_t, FreeList_t>* tl) const {
  return total_nodes_helper(root());
}

// Searches the tree for a chunk that ends at the
// specified address.
template <class Chunk_t, class FreeList_t>
class EndTreeSearchClosure : public DescendTreeSearchClosure<Chunk_t, FreeList_t> {
  HeapWord* _target;
  Chunk_t* _found;

 public:
  EndTreeSearchClosure(HeapWord* target) : _target(target), _found(NULL) { }
  bool do_list(FreeList_t* fl) {
    Chunk_t* item = fl->head();
    while (item != NULL) {
      if (item->end() == (uintptr_t*) _target) {
        _found = item;
        return true;
      }
      item = item->next();
    }
    return false;
  }
  Chunk_t* found() { return _found; }
};

template <class Chunk_t, class FreeList_t>
Chunk_t* BinaryTreeDictionary<Chunk_t, FreeList_t>::find_chunk_ends_at(HeapWord* target) const {
  EndTreeSearchClosure<Chunk_t, FreeList_t> etsc(target);
  bool found_target = etsc.do_tree(root());
  return etsc.found();
}

// Print summary statistics
template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::report_statistics(outputStream* st) const {
  verify_par_locked();
  st->print_cr("Statistics for BinaryTreeDictionary:");
  st->print_cr("------------------------------------");
  size_t total_size = total_chunk_size();
  size_t free_blocks = num_free_blocks();
  st->print_cr("Total Free Space: " SIZE_FORMAT, total_size);
  st->print_cr("Max   Chunk Size: " SIZE_FORMAT, max_chunk_size());
  st->print_cr("Number of Blocks: " SIZE_FORMAT, free_blocks);
  if (free_blocks > 0) {
    st->print_cr("Av.  Block  Size: " SIZE_FORMAT, total_size/free_blocks);
  }
  st->print_cr("Tree      Height: " SIZE_FORMAT, tree_height());
}

template <class Chunk_t, class FreeList_t>
class PrintFreeListsClosure : public AscendTreeCensusClosure<Chunk_t, FreeList_t> {
  outputStream* _st;
  int _print_line;

 public:
  PrintFreeListsClosure(outputStream* st) {
    _st = st;
    _print_line = 0;
  }
  void do_list(FreeList_t* fl) {
    if (++_print_line >= 40) {
      FreeList_t::print_labels_on(_st, "size");
      _print_line = 0;
    }
    fl->print_on(_st);
    size_t sz = fl->size();
    for (Chunk_t* fc = fl->head(); fc != NULL; fc = fc->next()) {
      _st->print_cr("\t[" PTR_FORMAT "," PTR_FORMAT ")  %s", p2i(fc), p2i((HeapWord*)fc + sz), fc->cantCoalesce() ? "\t CC" : "");
    }
  }
};

template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::print_free_lists(outputStream* st) const {

  FreeList_t::print_labels_on(st, "size");
  PrintFreeListsClosure<Chunk_t, FreeList_t> pflc(st);
  pflc.do_tree(root());
}

// Verify the following tree invariants:
// . _root has no parent
// . parent and child point to each other
// . each node's key correctly related to that of its child(ren)
template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::verify_tree() const {
  guarantee(root() == NULL || total_free_blocks() == 0 || total_size() != 0, "_total_size shouldn't be 0?");
  guarantee(root() == NULL || root()->parent() == NULL, "_root shouldn't have parent");
  verify_tree_helper(root());
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::verify_prev_free_ptrs(TreeList<Chunk_t, FreeList_t>* tl) {
  size_t ct = 0;
  for (Chunk_t* curFC = tl->head(); curFC != NULL; curFC = curFC->next()) {
    ct++;
  }
  return ct;
}

// Note: this helper is recursive rather than iterative, so use with
// caution on very deep trees; and watch out for stack overflow errors;
// In general, to be used only for debugging.
template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::verify_tree_helper(TreeList<Chunk_t, FreeList_t>* tl) const {
  if (tl == NULL)
    return;
  guarantee(tl->size() != 0, "A list must has a size");
  guarantee(tl->left()  == NULL || tl->left()->parent()  == tl, "parent<-/->left");
  guarantee(tl->right() == NULL || tl->right()->parent() == tl, "parent<-/->right");
  guarantee(tl->left() == NULL  || tl->left()->size()    <  tl->size(), "parent !> left");
  guarantee(tl->right() == NULL || tl->right()->size()   >  tl->size(), "parent !< left");
  guarantee(tl->head() == NULL || tl->head()->is_free(), "!Free");
  guarantee(tl->head() == NULL || tl->head_as_TreeChunk()->list() == tl, "list inconsistency");
  guarantee(tl->count() > 0 || (tl->head() == NULL && tl->tail() == NULL), "list count is inconsistent");
  guarantee(tl->count() > 1 || tl->head() == tl->tail(), "list is incorrectly constructed");
  size_t count = verify_prev_free_ptrs(tl);
  guarantee(count == (size_t)tl->count(), "Node count is incorrect");
  if (tl->head() != NULL) {
    tl->head_as_TreeChunk()->verify_tree_chunk_list();
  }
  verify_tree_helper(tl->left());
  verify_tree_helper(tl->right());
}

template <class Chunk_t, class FreeList_t>
void BinaryTreeDictionary<Chunk_t, FreeList_t>::verify() const {
  verify_tree();
  guarantee(total_size() == total_size_in_tree(root()), "Total Size inconsistency");
}

template <class Chunk_t, class FreeList_t>
size_t BinaryTreeDictionary<Chunk_t, FreeList_t>::total_chunk_size() const {
  return total_size();
}

#endif

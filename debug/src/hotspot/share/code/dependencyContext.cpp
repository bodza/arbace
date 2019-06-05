#include "precompiled.hpp"

#include "code/nmethod.hpp"
#include "code/dependencies.hpp"
#include "code/dependencyContext.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/atomic.hpp"
#include "utilities/exceptions.hpp"

void dependencyContext_init() {
  DependencyContext::init();
}

void DependencyContext::init() { }

//
// Walk the list of dependent nmethods searching for nmethods which
// are dependent on the changes that were passed in and mark them for
// deoptimization.  Returns the number of nmethods found.
//
int DependencyContext::mark_dependent_nmethods(DepChange& changes) {
  int found = 0;
  for (nmethodBucket* b = dependencies(); b != NULL; b = b->next()) {
    nmethod* nm = b->get_nmethod();
    // since dependencies aren't removed until an nmethod becomes a zombie,
    // the dependency list may contain nmethods which aren't alive.
    if (b->count() > 0 && nm->is_alive() && !nm->is_marked_for_deoptimization() && nm->check_dependency_on(changes)) {
      changes.mark_for_deoptimization(nm);
      found++;
    }
  }
  return found;
}

//
// Add an nmethod to the dependency context.
// It's possible that an nmethod has multiple dependencies on a klass
// so a count is kept for each bucket to guarantee that creation and
// deletion of dependencies is consistent.
//
void DependencyContext::add_dependent_nmethod(nmethod* nm, bool expunge) {
  for (nmethodBucket* b = dependencies(); b != NULL; b = b->next()) {
    if (nm == b->get_nmethod()) {
      b->increment();
      return;
    }
  }
  set_dependencies(new nmethodBucket(nm, dependencies()));
  if (expunge) {
    // Remove stale entries from the list.
    expunge_stale_entries();
  }
}

//
// Remove an nmethod dependency from the context.
// Decrement count of the nmethod in the dependency list and, optionally, remove
// the bucket completely when the count goes to 0.  This method must find
// a corresponding bucket otherwise there's a bug in the recording of dependencies.
// Can be called concurrently by parallel GC threads.
//
void DependencyContext::remove_dependent_nmethod(nmethod* nm, bool expunge) {
  nmethodBucket* first = dependencies();
  nmethodBucket* last = NULL;
  for (nmethodBucket* b = first; b != NULL; b = b->next()) {
    if (nm == b->get_nmethod()) {
      int val = b->decrement();
      guarantee(val >= 0, "Underflow: %d", val);
      if (val == 0) {
        if (expunge) {
          if (last == NULL) {
            set_dependencies(b->next());
          } else {
            last->set_next(b->next());
          }
          delete b;
        } else {
          // Mark the context as having stale entries, since it is not safe to
          // expunge the list right now.
          set_has_stale_entries(true);
        }
      }
      if (expunge) {
        // Remove stale entries from the list.
        expunge_stale_entries();
      }
      return;
    }
    last = b;
  }
  ShouldNotReachHere();
}

//
// Reclaim all unused buckets.
//
void DependencyContext::expunge_stale_entries() {
  if (!has_stale_entries()) {
    return;
  }
  nmethodBucket* first = dependencies();
  nmethodBucket* last = NULL;
  int removed = 0;
  for (nmethodBucket* b = first; b != NULL;) {
    nmethodBucket* next = b->next();
    if (b->count() == 0) {
      if (last == NULL) {
        first = next;
      } else {
        last->set_next(next);
      }
      removed++;
      delete b;
      // last stays the same.
    } else {
      last = b;
    }
    b = next;
  }
  set_dependencies(first);
  set_has_stale_entries(false);
}

//
// Invalidate all dependencies in the context
int DependencyContext::remove_all_dependents() {
  nmethodBucket* b = dependencies();
  set_dependencies(NULL);
  int marked = 0;
  int removed = 0;
  while (b != NULL) {
    nmethod* nm = b->get_nmethod();
    if (b->count() > 0 && nm->is_alive() && !nm->is_marked_for_deoptimization()) {
      nm->mark_for_deoptimization();
      marked++;
    }
    nmethodBucket* next = b->next();
    removed++;
    delete b;
    b = next;
  }
  set_has_stale_entries(false);
  return marked;
}

void DependencyContext::wipe() {
  nmethodBucket* b = dependencies();
  set_dependencies(NULL);
  set_has_stale_entries(false);
  while (b != NULL) {
    nmethodBucket* next = b->next();
    delete b;
    b = next;
  }
}

int nmethodBucket::decrement() {
  return Atomic::sub(1, &_count);
}

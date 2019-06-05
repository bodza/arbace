#ifndef SHARE_VM_GC_SHARED_AGETABLE_HPP
#define SHARE_VM_GC_SHARED_AGETABLE_HPP

#include "oops/markOop.hpp"
#include "oops/oop.hpp"

// Age table for adaptive feedback-mediated tenuring (scavenging)
//
// Note: all sizes are in oops

class AgeTable {
  friend class VMStructs;

 public:
  // constants
  enum { table_size = markOopDesc::max_age + 1 };

  // instance variables
  size_t sizes[table_size];

  // constructor.  "global" indicates that this is the global age table
  // (as opposed to gc-thread-local)
  AgeTable(bool global = true);

  // clear table
  void clear();

  // add entry
  inline void add(oop p, size_t oop_size);

  void add(uint age, size_t oop_size) {
    sizes[age] += oop_size;
  }

  // Merge another age table with the current one.  Used
  // for parallel young generation gc.
  void merge(AgeTable* subTable);

  // Calculate new tenuring threshold based on age information.
  uint compute_tenuring_threshold(size_t desired_survivor_size);
};

#endif

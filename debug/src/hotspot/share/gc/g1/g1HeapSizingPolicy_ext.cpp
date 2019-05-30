#include "precompiled.hpp"
#include "gc/g1/g1HeapSizingPolicy.hpp"
#include "memory/allocation.inline.hpp"

G1HeapSizingPolicy* G1HeapSizingPolicy::create(const G1CollectedHeap* g1h, const G1Analytics* analytics) {
  return new G1HeapSizingPolicy(g1h, analytics);
}

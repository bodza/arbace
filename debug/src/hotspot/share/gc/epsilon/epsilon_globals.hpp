#ifndef SHARE_VM_GC_EPSILON_GLOBALS_HPP
#define SHARE_VM_GC_EPSILON_GLOBALS_HPP

#include "runtime/globals.hpp"
//
// Defines all globals flags used by the Epsilon GC.
//

#define GC_EPSILON_FLAGS(develop, \
                    develop_pd, \
                    product, \
                    product_pd, \
                    diagnostic, \
                    diagnostic_pd, \
                    experimental, \
                    notproduct, \
                    manageable, \
                    product_rw, \
                    lp64_product, \
                    range, \
                    constraint, \
                    writeable) \
 \
  experimental(size_t, EpsilonPrintHeapSteps, 20, \
          "Print heap occupancy stats with this number of steps. " \
          "0 turns the printing off.") \
          range(0, max_intx) \
 \
  experimental(size_t, EpsilonUpdateCountersStep, 1 * M, \
          "Update heap occupancy counters after allocating this much " \
          "memory. Higher values would make allocations faster at " \
          "the expense of lower resolution in heap counters.") \
          range(1, max_intx) \
 \
  experimental(size_t, EpsilonMaxTLABSize, 4 * M, \
          "Max TLAB size to use with Epsilon GC. Larger value improves " \
          "performance at the expense of per-thread memory waste. This " \
          "asks TLAB machinery to cap TLAB sizes at this value.") \
          range(1, max_intx) \
 \
  experimental(bool, EpsilonElasticTLAB, true, \
          "Use elastic policy to manage TLAB sizes. This conserves memory " \
          "for non-actively allocating threads, even when they request " \
          "large TLABs for themselves. Active threads would experience " \
          "smaller TLABs until policy catches up.") \
 \
  experimental(bool, EpsilonElasticTLABDecay, true, \
          "Use timed decays to shrik TLAB sizes. This conserves memory " \
          "for the threads that allocate in bursts of different sizes, " \
          "for example the small/rare allocations coming after the initial " \
          "large burst.") \
 \
  experimental(double, EpsilonTLABElasticity, 1.1, \
          "Multiplier to use when deciding on next TLAB size. Larger value " \
          "improves performance at the expense of per-thread memory waste. " \
          "Lower value improves memory footprint, but penalizes actively " \
          "allocating threads.") \
          range(1.0, DBL_MAX) \
 \
  experimental(size_t, EpsilonTLABDecayTime, 1000, \
          "TLAB sizing policy decays to initial size after thread had not " \
          "allocated for this long. Time is in milliseconds. Lower value " \
          "improves memory footprint, but penalizes actively allocating " \
          "threads.") \
          range(1, max_intx) \
 \
  experimental(size_t, EpsilonMinHeapExpand, 128 * M, \
          "Min expansion step for heap. Larger value improves performance " \
          "at the potential expense of memory waste.") \
          range(1, max_intx)

#endif

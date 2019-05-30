#ifndef SHARE_VM_GC_G1_G1REMSETSUMMARY_HPP
#define SHARE_VM_GC_G1_G1REMSETSUMMARY_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/ostream.hpp"

class G1RemSet;

// A G1RemSetSummary manages statistical information about the G1RemSet

class G1RemSetSummary {
private:
  friend class GetRSThreadVTimeClosure;

  G1RemSet* _rem_set;

  size_t _num_conc_refined_cards;
  size_t _num_processed_buf_mutator;
  size_t _num_processed_buf_rs_threads;

  size_t _num_coarsenings;

  size_t _num_vtimes;
  double* _rs_threads_vtimes;

  double _sampling_thread_vtime;

  void set_rs_thread_vtime(uint thread, double value);
  void set_sampling_thread_vtime(double value) {
    _sampling_thread_vtime = value;
  }

  // update this summary with current data from various places
  void update();

public:
  G1RemSetSummary();
  G1RemSetSummary(G1RemSet* remset);

  ~G1RemSetSummary();

  // set the counters in this summary to the values of the others
  void set(G1RemSetSummary* other);
  // subtract all counters from the other summary, and set them in the current
  void subtract_from(G1RemSetSummary* other);

  void print_on(outputStream* out);

  double rs_thread_vtime(uint thread) const;

  double sampling_thread_vtime() const {
    return _sampling_thread_vtime;
  }

  size_t num_conc_refined_cards() const {
    return _num_conc_refined_cards;
  }

  size_t num_processed_buf_mutator() const {
    return _num_processed_buf_mutator;
  }

  size_t num_processed_buf_rs_threads() const {
    return _num_processed_buf_rs_threads;
  }

  size_t num_processed_buf_total() const {
    return num_processed_buf_mutator() + num_processed_buf_rs_threads();
  }

  size_t num_coarsenings() const {
    return _num_coarsenings;
  }
};

#endif

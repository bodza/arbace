#ifndef SHARE_VM_GC_G1_G1PREDICTIONS_HPP
#define SHARE_VM_GC_G1_G1PREDICTIONS_HPP

#include "utilities/numberSeq.hpp"

// Utility class containing various helper methods for prediction.
class G1Predictions {
 private:
  double _sigma;

  // This function is used to estimate the stddev of sample sets. There is some
  // special consideration of small sample sets: the actual stddev for them is
  // not very useful, so we calculate some value based on the sample average.
  // Five or more samples yields zero (at that point we use the stddev); fewer
  // scale the sample set average linearly from two times the average to 0.5 times
  // it.
  double stddev_estimate(TruncatedSeq const* seq) const {
    double estimate = seq->dsd();
    int const samples = seq->num();
    if (samples < 5) {
      estimate = MAX2(seq->davg() * (5 - samples) / 2.0, estimate);
    }
    return estimate;
  }
 public:
  G1Predictions(double sigma) : _sigma(sigma) { }

  // Confidence factor.
  double sigma() const { return _sigma; }

  double get_new_prediction(TruncatedSeq const* seq) const {
    return seq->davg() + _sigma * stddev_estimate(seq);
  }
};

#endif

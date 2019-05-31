#include "precompiled.hpp"
#include "gc/shared/gcStats.hpp"
#include "gc/shared/gcUtil.inline.hpp"

GCStats::GCStats() {
    _avg_promoted = new AdaptivePaddedNoZeroDevAverage(AdaptiveSizePolicyWeight, PromotedPadding);
}

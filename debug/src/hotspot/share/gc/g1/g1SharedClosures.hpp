#include "gc/g1/g1CodeBlobClosure.hpp"
#include "gc/g1/g1OopClosures.hpp"
#include "memory/iterator.hpp"

class G1CollectedHeap;
class G1ParScanThreadState;

// Simple holder object for a complete set of closures used by the G1 evacuation code.
template <G1Mark Mark>
class G1SharedClosures {
public:
  G1ParCopyClosure<G1BarrierNone, Mark> _oops;
  G1ParCopyClosure<G1BarrierCLD,  Mark> _oops_in_cld;

  G1CLDScanClosure                _clds;
  G1CodeBlobClosure               _codeblobs;

  G1SharedClosures(G1CollectedHeap* g1h, G1ParScanThreadState* pss, bool process_only_dirty, bool must_claim_cld) :
    _oops(g1h, pss),
    _oops_in_cld(g1h, pss),
    _clds(&_oops_in_cld, process_only_dirty, must_claim_cld),
    _codeblobs(&_oops) {}
};

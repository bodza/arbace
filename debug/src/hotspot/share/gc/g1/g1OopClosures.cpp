#include "precompiled.hpp"

#include "gc/g1/g1CollectedHeap.inline.hpp"
#include "gc/g1/g1OopClosures.inline.hpp"
#include "gc/g1/g1ParScanThreadState.hpp"
#include "memory/iterator.inline.hpp"
#include "utilities/stack.inline.hpp"

G1ParCopyHelper::G1ParCopyHelper(G1CollectedHeap* g1h,  G1ParScanThreadState* par_scan_state) :
  _g1h(g1h),
  _par_scan_state(par_scan_state),
  _worker_id(par_scan_state->worker_id()),
  _scanned_cld(NULL),
  _cm(_g1h->concurrent_mark())
{ }

G1ScanClosureBase::G1ScanClosureBase(G1CollectedHeap* g1h, G1ParScanThreadState* par_scan_state) :
  _g1h(g1h), _par_scan_state(par_scan_state), _from(NULL)
{ }

void G1CLDScanClosure::do_cld(ClassLoaderData* cld) {
  // If the class loader data has not been dirtied we know that there's
  // no references into  the young gen and we can skip it.
  if (!_process_only_dirty || cld->has_modified_oops()) {
    // Tell the closure that this class loader data is the CLD to scavenge
    // and is the one to dirty if oops are left pointing into the young gen.
    _closure->set_scanned_cld(cld);

    // Clean the cld since we're going to scavenge all the metadata.
    // Clear modified oops only if this cld is claimed.
    cld->oops_do(_closure, _must_claim, /*clear_modified_oops*/true);

    _closure->set_scanned_cld(NULL);

    _closure->trim_queue_partially();
  }
  _count++;
}

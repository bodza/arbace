#ifndef SHARE_VM_GC_G1_G1HEAPVERIFIER_HPP
#define SHARE_VM_GC_G1_G1HEAPVERIFIER_HPP

#include "gc/g1/heapRegionSet.hpp"
#include "memory/allocation.hpp"
#include "memory/universe.hpp"
#include "utilities/macros.hpp"

class G1CollectedHeap;

class G1HeapVerifier : public CHeapObj<mtGC> {
private:
  static int _enabled_verification_types;

  G1CollectedHeap* _g1h;

  void verify_region_sets();

public:
  enum G1VerifyType {
    G1VerifyYoungNormal     =  1, // -XX:VerifyGCType=young-normal
    G1VerifyConcurrentStart =  2, // -XX:VerifyGCType=concurrent-start
    G1VerifyMixed           =  4, // -XX:VerifyGCType=mixed
    G1VerifyRemark          =  8, // -XX:VerifyGCType=remark
    G1VerifyCleanup         = 16, // -XX:VerifyGCType=cleanup
    G1VerifyFull            = 32, // -XX:VerifyGCType=full
    G1VerifyAll             = -1
  };

  G1HeapVerifier(G1CollectedHeap* heap) : _g1h(heap) {}

  static void enable_verification_type(G1VerifyType type);
  static bool should_verify(G1VerifyType type);

  // Perform verification.

  // vo == UsePrevMarking -> use "prev" marking information,
  // vo == UseNextMarking -> use "next" marking information
  // vo == UseFullMarking -> use "next" marking bitmap but no TAMS
  //
  // NOTE: Only the "prev" marking information is guaranteed to be
  // consistent most of the time, so most calls to this should use
  // vo == UsePrevMarking.
  // Currently, there is only one case where this is called with
  // vo == UseNextMarking, which is to verify the "next" marking
  // information at the end of remark.
  // Currently there is only one place where this is called with
  // vo == UseFullMarking, which is to verify the marking during a
  // full GC.
  void verify(VerifyOption vo);

  // verify_region_sets_optional() is planted in the code for
  // list verification in debug builds.
  void verify_region_sets_optional() { }

  void prepare_for_verify();
  double verify(G1VerifyType type, VerifyOption vo, const char* msg);
  void verify_before_gc(G1VerifyType type);
  void verify_after_gc(G1VerifyType type);

  // If G1VerifyBitmaps is set, verify that the marking bitmaps for
  // the given region do not have any spurious marks. If errors are
  // detected, print appropriate error messages and crash.
  void check_bitmaps(const char* caller, HeapRegion* hr) {};

  // If G1VerifyBitmaps is set, verify that the marking bitmaps do not
  // have any spurious marks. If errors are detected, print
  // appropriate error messages and crash.
  void check_bitmaps(const char* caller) {};

  // Do sanity check on the contents of the in-cset fast test table.
  bool check_cset_fast_test() { return true; };

  void verify_card_table_cleanup() {};

  void verify_not_dirty_region(HeapRegion* hr) {};
  void verify_dirty_region(HeapRegion* hr) {};
  void verify_dirty_young_regions() {};

  static void verify_archive_regions();
};

#endif

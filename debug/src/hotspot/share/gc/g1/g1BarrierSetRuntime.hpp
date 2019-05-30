#ifndef SHARE_GC_G1_G1BARRIERSETRUNTIME_HPP
#define SHARE_GC_G1_G1BARRIERSETRUNTIME_HPP

#include "memory/allocation.hpp"
#include "oops/oopsHierarchy.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

class oopDesc;
class JavaThread;

class G1BarrierSetRuntime: public AllStatic {
public:
  // Arraycopy stub generator
  static void write_ref_array_pre_oop_entry(oop* dst, size_t length);
  static void write_ref_array_pre_narrow_oop_entry(narrowOop* dst, size_t length);
  static void write_ref_array_post_entry(HeapWord* dst, size_t length);

  // C2 slow-path runtime calls.
  static void write_ref_field_pre_entry(oopDesc* orig, JavaThread *thread);
  static void write_ref_field_post_entry(void* card_addr, JavaThread* thread);
};

#endif

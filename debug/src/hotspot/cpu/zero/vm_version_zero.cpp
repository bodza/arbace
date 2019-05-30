#include "precompiled.hpp"
#include "assembler_zero.inline.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/java.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "vm_version_zero.hpp"

void VM_Version::initialize() {
  // This machine does not allow unaligned memory accesses
  if (! FLAG_IS_DEFAULT(UseUnalignedAccesses)) {
    warning("Unaligned memory access is not available on this CPU");
    FLAG_SET_DEFAULT(UseUnalignedAccesses, false);
  }
  // Disable prefetching for Zero
  if (! FLAG_IS_DEFAULT(AllocatePrefetchDistance)) {
    warning("Prefetching is not available for a Zero VM");
  }
  FLAG_SET_DEFAULT(AllocatePrefetchDistance, 0);
}

#ifndef SHARE_VM_JVMCI_VMSTRUCTS_JVMCI_HPP
#define SHARE_VM_JVMCI_VMSTRUCTS_JVMCI_HPP

#include "runtime/vmStructs.hpp"

class JVMCIVMStructs {
public:
  /**
   * The last entry has a NULL fieldName.
   */
  static VMStructEntry localHotSpotVMStructs[];

  /**
   * The last entry has a NULL typeName.
   */
  static VMTypeEntry localHotSpotVMTypes[];

  /**
   * Table of integer constants.
   * The last entry has a NULL typeName.
   */
  static VMIntConstantEntry localHotSpotVMIntConstants[];

  /**
   * Table of long constants.
   * The last entry has a NULL typeName.
   */
  static VMLongConstantEntry localHotSpotVMLongConstants[];

  /**
   * Table of addresses.
   */
  static VMAddressEntry localHotSpotVMAddresses[];

  static int localHotSpotVMStructs_count();
  static int localHotSpotVMTypes_count();
  static int localHotSpotVMIntConstants_count();
  static int localHotSpotVMLongConstants_count();
  static int localHotSpotVMAddresses_count();
};

#endif

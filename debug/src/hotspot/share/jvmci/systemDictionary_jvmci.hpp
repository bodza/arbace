#ifndef SHARE_VM_JVMCI_SYSTEMDICTIONARY_JVMCI_HPP
#define SHARE_VM_JVMCI_SYSTEMDICTIONARY_JVMCI_HPP

#define JVMCI_WK_KLASSES_DO(do_klass) \
  /* JVMCI classes. These are loaded on-demand. */ \
  do_klass(JVMCI_klass,                                  jdk_vm_ci_runtime_JVMCI,                               Jvmci) \
  do_klass(HotSpotForeignCallTarget_klass,               jdk_vm_ci_hotspot_HotSpotForeignCallTarget,            Jvmci) \
  do_klass(HotSpotResolvedJavaMethodImpl_klass,          jdk_vm_ci_hotspot_HotSpotResolvedJavaMethodImpl,       Jvmci) \
  do_klass(HotSpotResolvedObjectTypeImpl_klass,          jdk_vm_ci_hotspot_HotSpotResolvedObjectTypeImpl,       Jvmci) \
  do_klass(HotSpotConstantPool_klass,                    jdk_vm_ci_hotspot_HotSpotConstantPool,                 Jvmci) \
  do_klass(Value_klass,                                  jdk_vm_ci_meta_Value,                                  Jvmci)

#endif

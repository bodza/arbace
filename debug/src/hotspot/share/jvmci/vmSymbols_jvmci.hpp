#ifndef SHARE_VM_JVMCI_VMSYMBOLS_JVMCI_HPP
#define SHARE_VM_JVMCI_VMSYMBOLS_JVMCI_HPP

#define JVMCI_VM_SYMBOLS_DO(template, do_alias) \
  template(jdk_vm_ci_runtime_JVMCI,                               "jdk/vm/ci/runtime/JVMCI") \
  template(jdk_vm_ci_hotspot_HotSpotForeignCallTarget,            "jdk/vm/ci/hotspot/HotSpotForeignCallTarget") \
  template(jdk_vm_ci_hotspot_CompilerToVM,                        "jdk/vm/ci/hotspot/CompilerToVM") \
  template(jdk_vm_ci_hotspot_HotSpotResolvedJavaMethodImpl,       "jdk/vm/ci/hotspot/HotSpotResolvedJavaMethodImpl") \
  template(jdk_vm_ci_hotspot_HotSpotResolvedObjectTypeImpl,       "jdk/vm/ci/hotspot/HotSpotResolvedObjectTypeImpl") \
  template(jdk_vm_ci_hotspot_HotSpotConstantPool,                 "jdk/vm/ci/hotspot/HotSpotConstantPool") \
  template(jdk_vm_ci_meta_Value,                                  "jdk/vm/ci/meta/Value") \
  template(jdk_vm_ci_code_InvalidInstalledCodeException,          "jdk/vm/ci/code/InvalidInstalledCodeException") \
  template(jdk_vm_ci_common_JVMCIError,                           "jdk/vm/ci/common/JVMCIError") \
  template(visitFrame_name,                                       "visitFrame") \
  template(visitFrame_signature,                                  "(Ljdk/vm/ci/code/stack/InspectedFrame;)Ljava/lang/Object;") \
  template(adjustCompilationLevel_name,                           "adjustCompilationLevel") \
  template(adjustCompilationLevel_signature,                      "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/String;ZI)I") \
  template(compileMethod_name,                                    "compileMethod") \
  template(compileMethod_signature,                               "(Ljdk/vm/ci/hotspot/HotSpotResolvedJavaMethod;IJI)Ljdk/vm/ci/hotspot/HotSpotCompilationRequestResult;") \
  template(fromMetaspace_name,                                    "fromMetaspace") \
  template(method_fromMetaspace_signature,                        "(J)Ljdk/vm/ci/hotspot/HotSpotResolvedJavaMethod;") \
  template(constantPool_fromMetaspace_signature,                  "(J)Ljdk/vm/ci/hotspot/HotSpotConstantPool;") \
  template(klass_fromMetaspace_signature,                         "(Ljava/lang/Class;)Ljdk/vm/ci/hotspot/HotSpotResolvedObjectTypeImpl;")

#endif

package jdk.vm.ci.hotspot;

import static jdk.vm.ci.hotspot.HotSpotJVMCIRuntime.runtime;

import jdk.internal.misc.Unsafe;

/**
 * Used to access native configuration details.
 *
 * All non-static, public fields in this class are so that they can be compiled as constants.
 */
class HotSpotVMConfig extends HotSpotVMConfigAccess {
    /**
     * Gets the configuration associated with the singleton {@link HotSpotJVMCIRuntime}.
     */
    static HotSpotVMConfig config() {
        return runtime().getConfig();
    }

    HotSpotVMConfig(HotSpotVMConfigStore store) {
        super(store);
    }

    final int jvmAccFieldInternal = getConstant("JVM_ACC_FIELD_INTERNAL", Integer.class);
    final int jvmAccFieldStable = getConstant("JVM_ACC_FIELD_STABLE", Integer.class);

    // These modifiers are not public in Modifier so we get them via vmStructs.
    final int jvmAccSynthetic = getConstant("JVM_ACC_SYNTHETIC", Integer.class);
    final int jvmAccAnnotation = getConstant("JVM_ACC_ANNOTATION", Integer.class);
    final int jvmAccBridge = getConstant("JVM_ACC_BRIDGE", Integer.class);
    final int jvmAccVarargs = getConstant("JVM_ACC_VARARGS", Integer.class);
    final int jvmAccEnum = getConstant("JVM_ACC_ENUM", Integer.class);
}

package jdk.vm.ci.runtime;

import jdk.vm.ci.hotspot.HotSpotJVMCIRuntime;

public class JVMCI {
    private static final HotSpotJVMCIRuntime runtime;

    private static native HotSpotJVMCIRuntime initializeRuntime();

    public static void initialize() {
        // force static initializer
    }

    static {
        HotSpotJVMCIRuntime rt = null;
        try {
            rt = initializeRuntime();
        } catch (UnsatisfiedLinkError e) {
        }
        runtime = rt;
    }
}

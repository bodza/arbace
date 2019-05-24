package jdk.vm.ci.hotspot;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;

import jdk.internal.misc.Unsafe;
import jdk.vm.ci.code.CompiledCode;
import jdk.vm.ci.code.InstalledCode;
import jdk.vm.ci.meta.JavaKind;
import jdk.vm.ci.meta.JavaType;
import jdk.vm.ci.meta.UnresolvedJavaType;
import jdk.vm.ci.runtime.JVMCI;

/**
 * HotSpot implementation of a JVMCI runtime.
 */
public final class HotSpotJVMCIRuntime {
    static class DelayedInit {
        private static final HotSpotJVMCIRuntime instance = new HotSpotJVMCIRuntime();
    }

    /**
     * Gets the singleton {@link HotSpotJVMCIRuntime} object.
     */
    public static HotSpotJVMCIRuntime runtime() {
        JVMCI.initialize();
        return DelayedInit.instance;
    }

    protected final CompilerToVM compilerToVm;

    protected final HotSpotVMConfigStore configStore;
    protected final HotSpotVMConfig config;

    private HotSpotJVMCIRuntime() {
        compilerToVm = new CompilerToVM();

        configStore = new HotSpotVMConfigStore(compilerToVm);
        config = new HotSpotVMConfig(configStore);
    }

    public HotSpotVMConfigStore getConfigStore() {
        return configStore;
    }

    public HotSpotVMConfig getConfig() {
        return config;
    }

    public CompilerToVM getCompilerToVM() {
        return compilerToVm;
    }
}

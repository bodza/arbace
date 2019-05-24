package jdk.vm.ci.hotspot;

import jdk.vm.ci.code.StackSlot;
import jdk.vm.ci.code.site.Site;
import jdk.vm.ci.meta.ResolvedJavaMethod;

/**
 * {@link HotSpotCompiledCode} destined for installation as an nmethod.
 */
public final class HotSpotCompiledNmethod extends HotSpotCompiledCode {
    protected final HotSpotResolvedJavaMethod method;
    protected final int entryBCI;

    /**
     * Compilation identifier.
     */
    protected final int id;

    /**
     * Address of a native {@code JVMCIEnv} object or 0L if no such object exists.
     */
    protected final long jvmciEnv;

    protected final boolean hasUnsafeAccess;

    public HotSpotCompiledNmethod(String name, byte[] targetCode, int targetCodeSize, Site[] sites, ResolvedJavaMethod[] methods, boolean isImmutablePIC, int totalFrameSize, StackSlot deoptRescueSlot, HotSpotResolvedJavaMethod method, int entryBCI, int id, long jvmciEnv, boolean hasUnsafeAccess) {
        super(name, targetCode, targetCodeSize, sites, methods, isImmutablePIC, totalFrameSize, deoptRescueSlot);
        this.method = method;
        this.entryBCI = entryBCI;
        this.id = id;
        this.jvmciEnv = jvmciEnv;
        this.hasUnsafeAccess = hasUnsafeAccess;
    }
}

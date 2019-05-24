package jdk.vm.ci.hotspot;

import jdk.vm.ci.code.CompiledCode;
import jdk.vm.ci.code.StackSlot;
import jdk.vm.ci.code.site.Infopoint;
import jdk.vm.ci.code.site.Site;
import jdk.vm.ci.meta.ResolvedJavaMethod;

/**
 * A {@link CompiledCode} with additional HotSpot-specific information required for installing the
 * code in HotSpot's code cache.
 */
public class HotSpotCompiledCode implements CompiledCode {
    /**
     * The name of this compilation unit.
     */
    protected final String name;

    /**
     * The buffer containing the emitted machine code.
     */
    protected final byte[] targetCode;

    /**
     * The leading number of bytes in {@link #targetCode} containing the emitted machine code.
     */
    protected final int targetCodeSize;

    /**
     * A list of code annotations describing special sites in {@link #targetCode}.
     */
    protected final Site[] sites;

    /**
     * The list of the methods whose bytecodes were used as input to the compilation. If
     * {@code null}, then the compilation did not record method dependencies. Otherwise, the first
     * element of this array is the root method of the compilation.
     */
    protected final ResolvedJavaMethod[] methods;

    /**
     * A flag determining whether this code is immutable and position independent.
     */
    protected final boolean isImmutablePIC;

    /**
     * The total size of the stack frame of this compiled method.
     */
    protected final int totalFrameSize;

    /**
     * The deopt rescue slot. Must be non-null if there is a safepoint in the method.
     */
    protected final StackSlot deoptRescueSlot;

    public HotSpotCompiledCode(String name, byte[] targetCode, int targetCodeSize, Site[] sites, ResolvedJavaMethod[] methods, boolean isImmutablePIC, int totalFrameSize, StackSlot deoptRescueSlot) {
        this.name = name;
        this.targetCode = targetCode;
        this.targetCodeSize = targetCodeSize;
        this.sites = sites;
        this.methods = methods;

        this.isImmutablePIC = isImmutablePIC;
        this.totalFrameSize = totalFrameSize;
        this.deoptRescueSlot = deoptRescueSlot;
    }

    public String getName() {
        return name;
    }
}

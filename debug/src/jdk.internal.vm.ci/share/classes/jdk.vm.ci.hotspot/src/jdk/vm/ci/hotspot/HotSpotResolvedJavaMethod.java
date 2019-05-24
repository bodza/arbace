package jdk.vm.ci.hotspot;

import java.lang.reflect.Modifier;

import jdk.vm.ci.meta.JavaMethod;
import jdk.vm.ci.meta.ResolvedJavaMethod;

/**
 * Implementation of {@link JavaMethod} for resolved HotSpot methods.
 */
public interface HotSpotResolvedJavaMethod extends ResolvedJavaMethod {
    /**
     * Returns true if this method has a {@code CallerSensitive} annotation.
     *
     * @return true if CallerSensitive annotation present, false otherwise
     */
    boolean isCallerSensitive();

    /**
     * Allocates a compile id for this method by asking the VM for one.
     *
     * @param entryBCI entry bci
     * @return compile id
     */
    int allocateCompileId(int entryBCI);
}

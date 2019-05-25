package jdk.vm.ci.hotspot;

import java.lang.reflect.Modifier;

import jdk.vm.ci.meta.JavaMethod;
import jdk.vm.ci.meta.ResolvedJavaMethod;

/**
 * Implementation of {@link JavaMethod} for resolved HotSpot methods.
 */
public interface HotSpotResolvedJavaMethod extends ResolvedJavaMethod {
    /**
     * Allocates a compile id for this method by asking the VM for one.
     *
     * @param entryBCI entry bci
     * @return compile id
     */
    int allocateCompileId(int entryBCI);
}

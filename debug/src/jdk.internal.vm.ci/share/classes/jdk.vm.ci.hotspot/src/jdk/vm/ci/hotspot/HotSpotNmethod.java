package jdk.vm.ci.hotspot;

import static jdk.vm.ci.hotspot.CompilerToVM.compilerToVM;
import jdk.vm.ci.code.InstalledCode;
import jdk.vm.ci.code.InvalidInstalledCodeException;
import jdk.vm.ci.meta.JavaKind;
import jdk.vm.ci.meta.JavaType;
import jdk.vm.ci.meta.ResolvedJavaMethod;

/**
 * Implementation of {@link InstalledCode} for code installed as an nmethod.
 */
public class HotSpotNmethod extends HotSpotInstalledCode {
    /**
     * This (indirect) Method* reference is safe since class redefinition preserves all methods
     * associated with nmethods in the code cache.
     */
    private final HotSpotResolvedJavaMethod method;

    public HotSpotNmethod(HotSpotResolvedJavaMethod method, String name) {
        super(name);
        this.method = method;
    }

    public ResolvedJavaMethod getMethod() {
        return method;
    }

    @Override
    public void invalidate() {
        compilerToVM().invalidateInstalledCode(this);
    }

    @Override
    public Object executeVarargs(Object... args) throws InvalidInstalledCodeException {
        return compilerToVM().executeInstalledCode(args, this);
    }

    @Override
    public long getStart() {
        return isValid() ? super.getStart() : 0;
    }
}

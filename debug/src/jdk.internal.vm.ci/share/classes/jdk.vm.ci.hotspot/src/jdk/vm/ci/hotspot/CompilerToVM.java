package jdk.vm.ci.hotspot;

import static jdk.vm.ci.hotspot.HotSpotJVMCIRuntime.runtime;

import java.lang.reflect.Executable;

import jdk.vm.ci.code.InstalledCode;
import jdk.vm.ci.code.InvalidInstalledCodeException;
import jdk.vm.ci.meta.JavaType;
import jdk.vm.ci.meta.ResolvedJavaMethod;

/**
 * Calls from Java into HotSpot. The behavior of all the methods in this class that take a native
 * pointer as an argument (e.g., {@link #getSymbol(long)}) is undefined if the argument does not
 * denote a valid native object.
 */
final class CompilerToVM {
    /**
     * Initializes the native part of the JVMCI runtime.
     */
    private static native void registerNatives();

    static {
        initialize();
    }

    private static void initialize() {
        registerNatives();
    }

    /**
     * Gets the {@link CompilerToVM} instance associated with the singleton
     * {@link HotSpotJVMCIRuntime} instance.
     */
    public static CompilerToVM compilerToVM() {
        return runtime().getCompilerToVM();
    }

    /**
     * Installs the result of a compilation into the code cache.
     *
     * @param compiledCode the result of a compilation
     * @param code the details of the installed CodeBlob are written to this object
     * @return the outcome of the installation which will be one of
     *         {@link HotSpotVMConfig#codeInstallResultOk},
     *         {@link HotSpotVMConfig#codeInstallResultCacheFull},
     *         {@link HotSpotVMConfig#codeInstallResultCodeTooLarge},
     *         {@link HotSpotVMConfig#codeInstallResultDependenciesFailed} or
     *         {@link HotSpotVMConfig#codeInstallResultDependenciesInvalid}.
     */
    native int installCode(HotSpotCompiledCode compiledCode, InstalledCode code);

    /**
     * Reads the database of VM info. The return value encodes the info in a nested object array
     * that is described by the pseudo Java object {@code info} below:
     *
     * <pre>
     *     info = [
     *         VMField[] vmFields,
     *         [String name, Long size, ...] vmTypeSizes,
     *         [String name, Long value, ...] vmConstants,
     *         [String name, Long value, ...] vmAddresses,
     *         VMFlag[] vmFlags
     *         VMIntrinsicMethod[] vmIntrinsics
     *     ]
     * </pre>
     *
     * @return VM info as encoded above
     */
    native Object[] readConfiguration();

    /**
     * Gets the method corresponding to {@code executable}.
     */
    native ResolvedJavaMethod asResolvedJavaMethod(Executable executable);

    /**
     * Gets a textual disassembly of {@code codeBlob}.
     *
     * @return a non-zero length string containing a disassembly of {@code codeBlob} or null if
     *         {@code codeBlob} could not be disassembled for some reason
     */
    // The HotSpot disassembler seems not to be thread safe so it's better to synchronize its usage
    synchronized native String disassembleCodeBlob(InstalledCode installedCode);

    /**
     * Executes some {@code installedCode} with arguments {@code args}.
     *
     * @return the result of executing {@code installedCode}
     * @throws InvalidInstalledCodeException if {@code installedCode} has been invalidated
     */
    native Object executeInstalledCode(Object[] args, InstalledCode installedCode) throws InvalidInstalledCodeException;

    /**
     * Invalidates {@code installedCode} such that {@link InvalidInstalledCodeException} will be
     * raised the next time {@code installedCode} is executed.
     */
    native void invalidateInstalledCode(InstalledCode installedCode);

    /**
     * Generate a unique id to identify the result of the compile.
     */
    native int allocateCompileId(ResolvedJavaMethod method, int entryBCI);

    /**
     * Gets the value of {@code metaspaceSymbol} as a String.
     */
    native String getSymbol(long metaspaceSymbol);

    /**
     * Gets the value of the VM flag named {@code name}.
     *
     * @param name name of a VM option
     * @return {@code this} if the named VM option doesn't exist, a {@link String} or {@code null}
     *         if its type is {@code ccstr} or {@code ccstrlist}, a {@link Double} if its type is
     *         {@code double}, a {@link Boolean} if its type is {@code bool} otherwise a
     *         {@link Long}
     */
    native Object getFlagValue(String name);
}

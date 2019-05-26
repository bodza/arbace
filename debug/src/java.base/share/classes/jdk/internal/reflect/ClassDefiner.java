package jdk.internal.reflect;

import jdk.internal.misc.Unsafe;

/**
 * Utility class which assists in calling Unsafe.defineClass() by
 * creating a new class loader which delegates to the one needed in
 * order for proper resolution of the given bytecodes to occur.
 */
class ClassDefiner {
    static final Unsafe unsafe = Unsafe.getUnsafe();

    /**
     * We define generated code into a new class loader which
     * delegates to the defining loader of the target class. It is
     * necessary for the VM to be able to resolve references to the
     * target class from the generated bytecodes, which could not occur
     * if the generated code was loaded into the bootstrap class
     * loader.
     *
     * There are two primary reasons for creating a new loader
     * instead of defining these bytecodes directly into the defining
     * loader of the target class: first, it avoids any possible
     * security risk of having these bytecodes in the same loader.
     * Second, it allows the generated bytecodes to be unloaded earlier
     * than would otherwise be possible, decreasing run-time
     * footprint.
     */
    static Class<?> defineClass(String name, byte[] bytes, int off, int len, final ClassLoader parentClassLoader) {
        return unsafe.defineClass(name, bytes, off, len, new DelegatingClassLoader(parentClassLoader));
    }
}

// NOTE: this class's name and presence are known to the virtual
// machine as of the fix for 4474172.
class DelegatingClassLoader extends ClassLoader {
    DelegatingClassLoader(ClassLoader parent) {
        super(parent);
    }
}

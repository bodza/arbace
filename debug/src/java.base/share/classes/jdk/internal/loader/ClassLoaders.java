package jdk.internal.loader;

import java.io.IOException;
import java.net.URL;

import jdk.internal.misc.VM;

/**
 * Creates and provides access to the built-in platform and application class
 * loaders. It also creates the class loader that is used to locate resources
 * in modules defined to the boot class loader.
 */
public class ClassLoaders {
    private ClassLoaders() { }

    // the built-in class loaders
    private static final BootClassLoader BOOT_LOADER = new BootClassLoader();
    private static final PlatformClassLoader PLATFORM_LOADER = new PlatformClassLoader(BOOT_LOADER);
    private static final AppClassLoader APP_LOADER = new AppClassLoader(PLATFORM_LOADER);

    /**
     * Returns the class loader that is used to find resources in modules
     * defined to the boot class loader.
     *
     * @apiNote This method is not public, it should instead be used via
     * the BootLoader class that provides a restricted API to this class
     * loader.
     */
    static BuiltinClassLoader bootLoader() {
        return BOOT_LOADER;
    }

    /**
     * Returns the platform class loader.
     */
    public static ClassLoader platformClassLoader() {
        return PLATFORM_LOADER;
    }

    /**
     * Returns the application class loader.
     */
    public static ClassLoader appClassLoader() {
        return APP_LOADER;
    }

    /**
     * The class loader that is used to find resources in modules defined to
     * the boot class loader. It is not used for class loading.
     */
    private static class BootClassLoader extends BuiltinClassLoader {
        BootClassLoader() {
            super(null, null);
        }

        @Override
        protected Class<?> loadClassOrNull(String cn) {
            return this.findBootstrapClassOrNull(cn);
        }
    };

    /**
     * The platform class loader, a unique type to make it easier to distinguish
     * from the application class loader.
     */
    private static class PlatformClassLoader extends BuiltinClassLoader {
        static {
            if (!ClassLoader.registerAsParallelCapable())
                throw new InternalError();
        }

        PlatformClassLoader(BootClassLoader parent) {
            super("platform", parent);
        }

        /**
         * Called by the VM to support define package for AppCDS.
         *
         * Shared classes are returned in ClassLoader::findLoadedClass
         * that bypass the defineClass call.
         */
        /* oops! private Package definePackage(String pn, Module module) {
            return this.definePackage(pn, module);
        } */
    }

    /**
     * The application class loader that is a {@code BuiltinClassLoader} with
     * customizations to be compatible with long standing behavior.
     */
    private static class AppClassLoader extends BuiltinClassLoader {
        static {
            if (!ClassLoader.registerAsParallelCapable())
                throw new InternalError();
        }

        AppClassLoader(PlatformClassLoader parent) {
            super("app", parent);
        }

        @Override
        protected Class<?> loadClass(String cn, boolean resolve) throws ClassNotFoundException {
            return super.loadClass(cn, resolve);
        }

        /**
         * Called by the VM to support define package for AppCDS
         *
         * Shared classes are returned in ClassLoader::findLoadedClass
         * that bypass the defineClass call.
         */
        /* oops! private Package definePackage(String pn, Module module) {
            return this.definePackage(pn, module);
        } */

        /**
         * Called by the VM to support define package for AppCDS
         */
        /* oops! protected Package defineOrCheckPackage(String pn) {
            return super.defineOrCheckPackage(pn);
        } */
    }
}

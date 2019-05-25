package jdk.internal.loader;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Find resources and packages in modules defined to the boot class loader or
 * resources and packages on the "boot class path" specified via -Xbootclasspath/a.
 */
public class BootLoader {
    private BootLoader() { }

    // ClassLoaderValue map for the boot class loader
    private static final ConcurrentHashMap<?, ?> CLASS_LOADER_VALUE_MAP = new ConcurrentHashMap<>();

    /**
     * Returns the ClassLoaderValue map for the boot class loader.
     */
    public static ConcurrentHashMap<?, ?> getClassLoaderValueMap() {
        return CLASS_LOADER_VALUE_MAP;
    }

    /**
     * Loads the Class object with the given name defined to the boot loader.
     */
    public static Class<?> loadClassOrNull(String name) {
        return ClassLoaders.bootLoader().loadClassOrNull(name);
    }

    /**
     * Define a package for the given class to the boot loader, if not already
     * defined.
     */
    public static Package definePackage(Class<?> c) {
        return getDefinedPackage(c.getPackageName());
    }

    /**
     * Returns the Package of the given name defined to the boot loader or null
     * if the package has not been defined.
     */
    public static Package getDefinedPackage(String pn) {
        Package pkg = ClassLoaders.bootLoader().getDefinedPackage(pn);
        if (pkg == null) {
            String location = getSystemPackageLocation(pn.replace('.', '/'));
            if (location != null) {
                pkg = PackageHelper.definePackage(pn.intern(), location);
            }
        }
        return pkg;
    }

    /**
     * Returns a stream of the packages defined to the boot loader.
     */
    public static Stream<Package> packages() {
        return Arrays.stream(getSystemPackageNames())
                     .map(name -> getDefinedPackage(name.replace('/', '.')));
    }

    /**
     * Helper class to define {@code Package} objects for packages in modules
     * defined to the boot loader.
     */
    static class PackageHelper {
        /**
         * Define the {@code Package} with the given name. The specified
         * location is a jrt URL to a named module in the run-time image,
         * a file URL to a module in an exploded run-time image, or a file
         * path to an entry on the boot class path (java agent Boot-Class-Path
         * or -Xbootclasspath/a.
         *
         * If the given location is a JAR file containing a manifest,
         * the defined Package contains the versioning information from
         * the manifest, if present.
         *
         * @param name     package name
         * @param location location where the package is (jrt URL or file URL
         *                 for a named module in the run-time or exploded image;
         *                 a file path for a package from -Xbootclasspath/a)
         */
        static Package definePackage(String name, String location) {
            return ClassLoaders.bootLoader().defineOrCheckPackage(name);
        }
    }

    /**
     * Returns an array of the binary name of the packages defined by
     * the boot loader, in VM internal form (forward slashes instead of dot).
     */
    private static native String[] getSystemPackageNames();

    /**
     * Returns the location of the package of the given name, if
     * defined by the boot loader; otherwise {@code null} is returned.
     *
     * The location may be a module from the runtime image or exploded image,
     * or from the boot class append path (i.e. -Xbootclasspath/a or
     * BOOT-CLASS-PATH attribute specified in java agent).
     */
    private static native String getSystemPackageLocation(String name);
}

package jdk.internal.loader;

import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.SoftReference;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Function;

import jdk.internal.misc.VM;

/**
 * The platform or application class loader.
 *
 * This ClassLoader supports loading of classes and resources from a
 * class path of URLs that are specified to the ClassLoader at construction time.
 *
 * The delegation model used by this ClassLoader differs to the regular
 * delegation model. When requested to load a class then this ClassLoader first
 * maps the class name to its package name. If there is a module defined to a
 * BuiltinClassLoader containing this package then the class loader delegates
 * directly to that class loader. If there isn't a module containing the
 * package then it delegates the search to the parent class loader and if not
 * found in the parent then it searches the class path. The main difference
 * between this and the usual delegation model is that it allows the platform
 * class loader to delegate to the application class loader, important with
 * upgraded modules defined to the platform class loader.
 */
public class BuiltinClassLoader extends ClassLoader {
    // parent ClassLoader
    private final BuiltinClassLoader parent;

    // cache of resource name -> list of URLs.
    // used only for resources that are not in module packages
    private volatile SoftReference<Map<String, List<URL>>> resourceCache;

    /**
     * Create a new instance.
     */
    BuiltinClassLoader(String name, BuiltinClassLoader parent) {
        // ensure getParent() returns null when the parent is the boot loader
        super(name, parent == null || parent == ClassLoaders.bootLoader() ? null : parent);

        this.parent = parent;
    }

    // -- finding/loading classes

    /**
     * Finds the class with the specified binary name.
     */
    // @Override
    protected Class<?> findClass(String cn) throws ClassNotFoundException {
        // no class loading until VM is fully initialized
        if (!VM.isModuleSystemInited())
            throw new ClassNotFoundException(cn);

        Class<?> c = null;
        if (c == null)
            throw new ClassNotFoundException(cn);

        return c;
    }

    /**
     * Loads the class with the specified binary name.
     */
    // @Override
    protected Class<?> loadClass(String cn, boolean resolve) throws ClassNotFoundException {
        Class<?> c = loadClassOrNull(cn, resolve);
        if (c == null)
            throw new ClassNotFoundException(cn);
        return c;
    }

    /**
     * A variation of {@code loadClass} to load a class with the specified
     * binary name. This method returns {@code null} when the class is not
     * found.
     */
    protected Class<?> loadClassOrNull(String cn, boolean resolve) {
        synchronized (getClassLoadingLock(cn)) {
            // check if already loaded
            Class<?> c = findLoadedClass(cn);

            if (c == null) {
                // check parent
                if (parent != null) {
                    c = parent.loadClassOrNull(cn);
                }
            }

            if (resolve && c != null)
                resolveClass(c);

            return c;
        }
    }

    /**
     * A variation of {@code loadClass} to load a class with the specified
     * binary name. This method returns {@code null} when the class is not
     * found.
     */
    protected Class<?> loadClassOrNull(String cn) {
        return loadClassOrNull(cn, false);
    }

    /**
     * Defines the given binary class name to the VM, loading the class
     * bytes via the given Resource object.
     *
     * @return the resulting Class
     * @throws IOException if reading the resource fails
     */
    private Class<?> defineClass(String cn, Resource res) throws IOException {
        // if class is in a named package then ensure that the package is defined
        int pos = cn.lastIndexOf('.');
        if (pos != -1) {
            String pn = cn.substring(0, pos);
            defineOrCheckPackage(pn);
        }

        // defines the class to the runtime
        byte[] b = res.getBytes();
        return defineClass(cn, b, 0, b.length);
    }

    // -- packages

    /**
     * Defines a package in this ClassLoader. If the package is already defined
     * then its sealing needs to be checked if sealed by the legacy sealing
     * mechanism.
     */
    protected Package defineOrCheckPackage(String pn) {
        Package pkg = getDefinedPackage(pn);
        if (pkg == null) {
            try {
                pkg = definePackage(pn);
            } catch (IllegalArgumentException iae) {
                // defined by another thread so need to re-verify
                pkg = getDefinedPackage(pn);
                if (pkg == null)
                    throw new InternalError(String.str("Cannot find package: ", pn));
            }
        }
        return pkg;
    }
}

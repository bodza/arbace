package java.lang;

import java.net.URI;

/**
 * A NamedPackage represents a package by name in a specific module.
 *
 * A class loader will automatically create NamedPackage for each
 * package when a class is defined.  Package object is lazily
 * defined until Class::getPackage or ClassLoader::getDefinedPackage(s)
 * method is called.
 *
 * NamedPackage allows ClassLoader to keep track of the runtime
 * packages with minimal footprint and avoid constructing Package
 * object.
 */
class NamedPackage {
    private final String name;

    NamedPackage(String pn) {
        this.name = pn.intern();
    }

    /**
     * Returns the name of this package.
     */
    String packageName() {
        return name;
    }

    /**
     * Returns the location of the module if this named package is in
     * a named module; otherwise, returns null.
     */
    URI location() {
        return null;
    }

    /**
     * Creates a Package object of the given name and module.
     */
    static Package toPackage(String name) {
        return new Package(name);
    }
}

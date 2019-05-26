package java.lang.reflect;

import jdk.internal.misc.VM;
import jdk.internal.reflect.Reflection;
import jdk.internal.reflect.ReflectionFactory;

/**
 * The {@code AccessibleObject} class is the base class for {@code Field},
 * {@code Method}, and {@code Constructor} objects (known as <em>reflected
 * objects</em>). It provides the ability to flag a reflected object as
 * suppressing checks for Java language access control when it is used. This
 * permits sophisticated applications with sufficient privilege, such as Java
 * Object Serialization or other persistence mechanisms, to manipulate objects
 * in a manner that would normally be prohibited.
 *
 * Java language access control prevents use of private members outside
 * their top-level class; package access members outside their package; protected members
 * outside their package or subclasses; and public members outside their
 * module unless they are declared in an {@link Module#isExported(String,Module)
 * exported} package and the user {@link Module#canRead reads} their module. By
 * default, Java language access control is enforced (with one variation) when
 * {@code Field}s, {@code Method}s, or {@code Constructor}s are used to get or
 * set fields, to invoke methods, or to create and initialize new instances of
 * classes, respectively. Every reflected object checks that the code using it
 * is in an appropriate class, package, or module.
 *
 * The one variation from Java language access control is that the checks
 * by reflected objects assume readability. That is, the module containing
 * the use of a reflected object is assumed to read the module in which
 * the underlying field, method, or constructor is declared.
 */
public class AccessibleObject {
    // Reflection factory used by subclasses for creating field,
    // method, and constructor accessors. Note that this is called
    // very early in the bootstrapping process.
    static final ReflectionFactory reflectionFactory = ReflectionFactory.getReflectionFactory();

    /**
     * Constructor: only used by the Java Virtual Machine.
     */
    protected AccessibleObject() {}

    /**
     * Returns the root AccessibleObject; or null if this object is the root.
     *
     * All subclasses override this method.
     */
    AccessibleObject getRoot() {
        throw new InternalError();
    }
}

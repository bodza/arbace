package java.lang;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.Objects;

import jdk.internal.loader.BootLoader;
import jdk.internal.reflect.CallerSensitive;
import jdk.internal.reflect.Reflection;

/**
 * Represents metadata about a run-time package associated with a class loader.
 * Metadata includes annotations, versioning, and sealing.
 *
 * Annotations for the run-time package are read from {@code package-info.class}
 * at the same code source as classes in the run-time package.
 *
 * A {@code Package} may be explicitly defined with
 * the {@link ClassLoader#definePackage(String)} method.
 * If a {@code Package} is not explicitly defined for a run-time package when
 * a class in that run-time package is defined, then a {@code Package} is
 * automatically defined by the class's defining class loader, as follows.
 *
 * A {@code Package} automatically defined for classes in a named module has
 * the following properties:
 * <ul>
 * <li>The name of the package is derived from the {@linkplain Class#getName() binary names}
 *     of the classes. Since classes in a named module must be in a named package,
 *     the derived name is never empty.</li>
 * <li>The package is sealed with the {@linkplain ModuleReference#location()
 *     module location} as the code source, if known.</li>
 * <li>The specification and implementation titles, versions, and vendors
 *     are unspecified.</li>
 * <li>Any annotations on the package are read from {@code package-info.class}
 *     as specified above.</li>
 * </ul>
 *
 * A {@code Package} automatically defined for classes in an unnamed module
 * has the following properties:
 * <ul>
 * <li>The name of the package is either {@code ""} (for classes in an unnamed package)
 *     or derived from the {@linkplain Class#getName() binary names} of the classes
 *     (for classes in a named package).</li>
 * <li>The package is not sealed.</li>
 * <li>The specification and implementation titles, versions, and vendors
 *     are unspecified.</li>
 * <li>Any annotations on the package are read from {@code package-info.class}
 *     as specified above.</li>
 * </ul>
 *
 * A {@code Package} can be obtained with the {@link ClassLoader#getDefinedPackage
 * ClassLoader.getDefinedPackage(String)} method.
 * Every {@code Package} defined by a class loader can be obtained
 * with the {@link Package#getPackages Package.getPackages()} and
 * {@link ClassLoader#getDefinedPackages} methods.
 *
 * @implNote
 * The <a href="ClassLoader.html#builtinLoaders">builtin class loaders</a>
 * do not explicitly define {@code Package} objects for packages in
 * <em>named modules</em>.  Instead those packages are automatically defined
 * and have no specification and implementation versioning information.
 */
public class Package extends NamedPackage implements java.lang.reflect.AnnotatedElement {
    /**
     * Return the name of this package.
     *
     * @return The fully-qualified name of this package as defined in section 6.5.3 of
     *          <cite>The Java&trade; Language Specification</cite>,
     *          for example, {@code java.lang}
     */
    public String getName() {
        return packageName();
    }

    /**
     * Returns all of the {@code Package}s defined by the caller's class loader
     * and its ancestors.  The returned array may contain more than one
     * {@code Package} object of the same package name, each defined by
     * a different class loader in the class loader hierarchy.
     *
     * Calling this method is equivalent to calling {@link ClassLoader#getPackages}
     * on a {@code ClassLoader} instance which is the caller's class loader.
     *
     * @return The array of {@code Package} objects defined by this
     *          class loader and its ancestors
     */
    @CallerSensitive
    public static Package[] getPackages() {
        ClassLoader cl = ClassLoader.getClassLoader(Reflection.getCallerClass());
        return cl != null ? cl.getPackages() : BootLoader.packages().toArray(Package[]::new);
    }

    /**
     * Return the hash code computed from the package name.
     * @return the hash code computed from the package name.
     */
    @Override
    public int hashCode() {
        return packageName().hashCode();
    }

    /**
     * Returns the string representation of this Package.
     * Its value is the string "package " and the package name.
     * If the package title is defined it is appended.
     * If the package version is defined it is appended.
     * @return the string representation of the package.
     */
    @Override
    public String toString() {
        return "package " + packageName();
    }

    private Class<?> getPackageInfo() {
        if (packageInfo == null) {
            // find package-info.class defined by loader
            String cn = packageName() + ".package-info";
            Class<?> c = BootLoader.loadClass(cn);
            if (c != null) {
                packageInfo = c;
            } else {
                // store a proxy for the package info that has no annotations
                class PackageInfoProxy {}
                packageInfo = PackageInfoProxy.class;
            }
        }
        return packageInfo;
    }

    /**
     * @throws NullPointerException {@inheritDoc}
     */
    public <A extends Annotation> A getAnnotation(Class<A> annotationClass) {
        return getPackageInfo().getAnnotation(annotationClass);
    }

    /**
     * {@inheritDoc}
     * @throws NullPointerException {@inheritDoc}
     */
    @Override
    public boolean isAnnotationPresent(Class<? extends Annotation> annotationClass) {
        return AnnotatedElement.super.isAnnotationPresent(annotationClass);
    }

    /**
     * @throws NullPointerException {@inheritDoc}
     */
    @Override
    public  <A extends Annotation> A[] getAnnotationsByType(Class<A> annotationClass) {
        return getPackageInfo().getAnnotationsByType(annotationClass);
    }

    public Annotation[] getAnnotations() {
        return getPackageInfo().getAnnotations();
    }

    /**
     * @throws NullPointerException {@inheritDoc}
     */
    @Override
    public <A extends Annotation> A getDeclaredAnnotation(Class<A> annotationClass) {
        return getPackageInfo().getDeclaredAnnotation(annotationClass);
    }

    /**
     * @throws NullPointerException {@inheritDoc}
     */
    @Override
    public <A extends Annotation> A[] getDeclaredAnnotationsByType(Class<A> annotationClass) {
        return getPackageInfo().getDeclaredAnnotationsByType(annotationClass);
    }

    public Annotation[] getDeclaredAnnotations() {
        return getPackageInfo().getDeclaredAnnotations();
    }

    /**
     * Construct a package instance for an unnamed module
     * with the specified version information.
     *
     * @apiNote
     * This method should not be called to define a Package for named module.
     *
     * @param name the name of the package
     * @param loader defining class loader
     */
    Package(String name, ClassLoader loader)
    {
        super(Objects.requireNonNull(name));
    }

    Package(String name) {
        super(name);
    }

    private Class<?> packageInfo;
}

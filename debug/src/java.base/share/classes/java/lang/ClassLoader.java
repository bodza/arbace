package java.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Vector;

import jdk.internal.perf.PerfCounter;
import jdk.internal.loader.ClassLoaders;
import jdk.internal.misc.Unsafe;
import jdk.internal.misc.VM;

/**
 * A class loader is an object that is responsible for loading classes. The
 * class {@code ClassLoader} is an abstract class.  Given the <a
 * href="#binary-name">binary name</a> of a class, a class loader should attempt to
 * locate or generate data that constitutes a definition for the class.  A
 * typical strategy is to transform the name into a file name and then read a
 * "class file" of that name from a file system.
 *
 * Every {@link java.lang.Class Class} object contains a {@link
 * Class#getClassLoader() reference} to the {@code ClassLoader} that defined it.
 *
 * {@code Class} objects for array classes are not created by class
 * loaders, but are created automatically as required by the Java runtime.
 * The class loader for an array class, as returned by {@link
 * Class#getClassLoader()} is the same as the class loader for its element
 * type; if the element type is a primitive type, then the array class has no
 * class loader.
 *
 * Applications implement subclasses of {@code ClassLoader} in order to
 * extend the manner in which the Java virtual machine dynamically loads
 * classes.
 *
 * Class loaders may typically be used by security managers to indicate
 * security domains.
 *
 * In addition to loading classes, a class loader is also responsible for
 * locating resources. A resource is some data (a "{@code .class}" file,
 * configuration data, or an image for example) that is identified with an
 * abstract '/'-separated path name. Resources are typically packaged with an
 * application or library so that they can be located by code in the
 * application or library. In some cases, the resources are included so that
 * they can be located by other libraries.
 *
 * The {@code ClassLoader} class uses a delegation model to search for
 * classes and resources.  Each instance of {@code ClassLoader} has an
 * associated parent class loader. When requested to find a class or
 * resource, a {@code ClassLoader} instance will usually delegate the search
 * for the class or resource to its parent class loader before attempting to
 * find the class or resource itself.
 *
 * <h3><a id="builtinLoaders">Run-time Built-in Class Loaders</a></h3>
 *
 * The Java run-time has the following built-in class loaders:
 *
 * <ul>
 * <li>Bootstrap class loader.
 *     It is the virtual machine's built-in class loader, typically represented
 *     as {@code null}, and does not have a parent.</li>
 * <li>{@linkplain #getPlatformClassLoader() Platform class loader}.
 *     All <em>platform classes</em> are visible to the platform class loader
 *     that can be used as the parent of a {@code ClassLoader} instance.
 *     Platform classes include Java SE platform APIs, their implementation
 *     classes and JDK-specific run-time classes that are defined by the
 *     platform class loader or its ancestors.
 *
 *     To allow for upgrading/overriding of modules defined to the platform
 *     class loader, and where upgraded modules read modules defined to class
 *     loaders other than the platform class loader and its ancestors, then
 *     the platform class loader may have to delegate to other class loaders,
 *     the application class loader for example.
 *     In other words, classes in named modules defined to class loaders
 *     other than the platform class loader and its ancestors may be visible
 *     to the platform class loader.</li>
 * <li>{@linkplain #getSystemClassLoader() System class loader}.
 *     It is also known as <em>application class loader</em> and is distinct
 *     from the platform class loader.
 *     The system class loader is typically used to define classes on the
 *     application class path, module path, and JDK-specific tools.
 *     The platform class loader is a parent or an ancestor of the system class
 *     loader that all platform classes are visible to it.</li>
 * </ul>
 *
 * Normally, the Java virtual machine loads classes from the local file
 * system in a platform-dependent manner.
 * However, some classes may not originate from a file; they may originate
 * from other sources, such as the network, or they could be constructed by an
 * application.  The method {@link #defineClass(String, byte[], int, int)
 * defineClass} converts an array of bytes into an instance of class
 * {@code Class}. Instances of this newly defined class can be created using
 * {@link Class#newInstance Class.newInstance}.
 *
 * The methods and constructors of objects created by a class loader may
 * reference other classes.  To determine the class(es) referred to, the Java
 * virtual machine invokes the {@link #loadClass loadClass} method of
 * the class loader that originally created the class.
 *
 * For example, an application could create a network class loader to
 * download class files from a server.  Sample code might look like:
 *
 * <blockquote><pre>
 *   ClassLoader loader&nbsp;= new NetworkClassLoader(host,&nbsp;port);
 *   Object main&nbsp;= loader.loadClass("Main", true).newInstance();
 *       &nbsp;.&nbsp;.&nbsp;.
 * </pre></blockquote>
 *
 * The network class loader subclass must define the methods {@link
 * #findClass findClass} and {@code loadClassData} to load a class
 * from the network.  Once it has downloaded the bytes that make up the class,
 * it should use the method {@link #defineClass defineClass} to
 * create a class instance.  A sample implementation is:
 *
 * <blockquote><pre>
 *     class NetworkClassLoader extends ClassLoader {
 *         String host;
 *         int port;
 *
 *         public Class findClass(String name) {
 *             byte[] b = loadClassData(name);
 *             return defineClass(name, b, 0, b.length);
 *         }
 *
 *         private byte[] loadClassData(String name) {
 *             // load the class data from the connection
 *             &nbsp;.&nbsp;.&nbsp;.
 *         }
 *     }
 * </pre></blockquote>
 *
 * <h3><a id="binary-name">Binary names</a></h3>
 *
 * Any class name provided as a {@code String} parameter to methods in
 * {@code ClassLoader} must be a binary name as defined by
 * <cite>The Java&trade; Language Specification</cite>.
 *
 * Examples of valid class names include:
 * <blockquote><pre>
 *   "java.lang.String"
 *   "javax.swing.JSpinner$DefaultEditor"
 *   "java.security.KeyStore$Builder$FileBuilder$1"
 *   "java.net.URLClassLoader$3$1"
 * </pre></blockquote>
 *
 * Any package name provided as a {@code String} parameter to methods in
 * {@code ClassLoader} must be either the empty string (denoting an unnamed package)
 * or a fully qualified name as defined by
 * <cite>The Java&trade; Language Specification</cite>.
 */
public abstract class ClassLoader {
    private static native void registerNatives();
    static {
        registerNatives();
    }

    // The parent class loader for delegation
    // Note: VM hardcoded the offset of this field, thus all new fields
    // must be added *after* it.
    private final ClassLoader parent;

    // class loader name
    private final String name;

    // The classes loaded by this class loader. The only purpose of this table
    // is to keep the classes from being GC'ed until the loader is GC'ed.
    private final Vector<Class<?>> classes = new Vector<>();

    // Invoked by the VM to record every loaded class with this loader.
    void addClass(Class<?> c) {
        classes.addElement(c);
    }

    // The packages defined in this class loader.  Each package name is
    // mapped to its corresponding NamedPackage object.
    //
    // The value is a Package object if ClassLoader::definePackage,
    // Class::getPackage or ClassLoader::getDefinedPackage(s)
    // method is called to define it.
    // Otherwise, the value is a NamedPackage object.
    private final Map<String, NamedPackage> packages = Collections.synchronizedMap(new HashMap<>());

    /*
     * Returns a named package for the given module.
     */
    private NamedPackage getNamedPackage(String pn) {
        NamedPackage p = packages.get(pn);
        if (p == null) {
            p = new NamedPackage(pn);

            NamedPackage value = packages.putIfAbsent(pn, p);
            if (value != null) {
                // Package object already be defined for the named package
                p = value;
            }
        }
        return p;
    }

    private static Void checkCreateClassLoader() {
        return checkCreateClassLoader(null);
    }

    private static Void checkCreateClassLoader(String name) {
        if (name != null && name.isEmpty()) {
            throw new IllegalArgumentException("name must be non-empty or null");
        }

        return null;
    }

    private ClassLoader(Void unused, String name, ClassLoader parent) {
        this.name = name;
        this.parent = parent;
    }

    /**
     * Creates a new class loader of the specified name and using the
     * specified parent class loader for delegation.
     *
     * @apiNote If the parent is specified as {@code null} (for the
     * bootstrap class loader) then there is no guarantee that all platform
     * classes are visible.
     *
     * @param name   class loader name; or {@code null} if not named
     * @param parent the parent class loader
     *
     * @throws IllegalArgumentException if the given name is empty.
     */
    protected ClassLoader(String name, ClassLoader parent) {
        this(checkCreateClassLoader(name), name, parent);
    }

    /**
     * Creates a new class loader using the specified parent class loader for
     * delegation.
     *
     * @apiNote If the parent is specified as {@code null} (for the
     * bootstrap class loader) then there is no guarantee that all platform
     * classes are visible.
     *
     * @param parent
     *         The parent class loader
     */
    protected ClassLoader(ClassLoader parent) {
        this(checkCreateClassLoader(), null, parent);
    }

    /**
     * Creates a new class loader using the {@code ClassLoader} returned by
     * the method {@link #getSystemClassLoader()
     * getSystemClassLoader()} as the parent class loader.
     */
    protected ClassLoader() {
        this(checkCreateClassLoader(), null, getSystemClassLoader());
    }

    /**
     * Returns the name of this class loader or {@code null} if
     * this class loader is not named.
     *
     * @apiNote This method is non-final for compatibility.  If this
     * method is overridden, this method must return the same name
     * as specified when this class loader was instantiated.
     *
     * @return name of this class loader; or {@code null} if
     * this class loader is not named.
     */
    public String getName() {
        return name;
    }

    // -- Class --

    /**
     * Loads the class with the specified <a href="#binary-name">binary name</a>.
     * This method searches for classes in the same manner as the {@link
     * #loadClass(String, boolean)} method.  It is invoked by the Java virtual
     * machine to resolve class references.  Invoking this method is equivalent
     * to invoking {@link #loadClass(String, boolean) loadClass(name,
     * false)}.
     *
     * @param name
     *         The <a href="#binary-name">binary name</a> of the class
     *
     * @return The resulting {@code Class} object
     *
     * @throws ClassNotFoundException
     *          If the class was not found
     */
    public Class<?> loadClass(String name) throws ClassNotFoundException {
        return loadClass(name, false);
    }

    /**
     * Loads the class with the specified <a href="#binary-name">binary name</a>.  The
     * default implementation of this method searches for classes in the
     * following order:
     *
     * <ol>
     *   <li>Invoke {@link #findLoadedClass(String)} to check if the class
     *   has already been loaded.</li>
     *
     *   <li>Invoke the {@link #loadClass(String) loadClass} method
     *   on the parent class loader.  If the parent is {@code null} the class
     *   loader built into the virtual machine is used, instead.</li>
     *
     *   <li>Invoke the {@link #findClass(String)} method to find the
     *   class.</li>
     * </ol>
     *
     * If the class was found using the above steps, and the
     * {@code resolve} flag is true, this method will then invoke the {@link
     * #resolveClass(Class)} method on the resulting {@code Class} object.
     *
     * Subclasses of {@code ClassLoader} are encouraged to override {@link
     * #findClass(String)}, rather than this method.
     *
     * Unless overridden, this method synchronizes on the result of
     * {@link #getClassLoadingLock getClassLoadingLock} method
     * during the entire class loading process.
     *
     * @param name
     *         The <a href="#binary-name">binary name</a> of the class
     *
     * @param resolve
     *         If {@code true} then resolve the class
     *
     * @return The resulting {@code Class} object
     *
     * @throws ClassNotFoundException
     *          If the class could not be found
     */
    protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        synchronized (getClassLoadingLock(name)) {
            // First, check if the class has already been loaded
            Class<?> c = findLoadedClass(name);
            if (c == null) {
                long t0 = System.nanoTime();
                try {
                    if (parent != null) {
                        c = parent.loadClass(name, false);
                    } else {
                        c = findBootstrapClassOrNull(name);
                    }
                } catch (ClassNotFoundException e) {
                    // ClassNotFoundException thrown if class not found
                    // from the non-null parent class loader
                }

                if (c == null) {
                    // If still not found, then invoke findClass in order
                    // to find the class.
                    long t1 = System.nanoTime();
                    c = findClass(name);

                    // this is the defining class loader; record the stats
                    PerfCounter.getParentDelegationTime().addTime(t1 - t0);
                    PerfCounter.getFindClassTime().addElapsedTimeFrom(t1);
                    PerfCounter.getFindClasses().increment();
                }
            }
            if (resolve) {
                resolveClass(c);
            }
            return c;
        }
    }

    /**
     * Returns the lock object for class loading operations.
     * For backward compatibility, the default implementation of this method
     * behaves as follows. If this ClassLoader object is registered as
     * parallel capable, the method returns a dedicated object associated
     * with the specified class name. Otherwise, the method returns this
     * ClassLoader object.
     *
     * @param className
     *         The name of the to-be-loaded class
     *
     * @return the lock for class loading operations
     *
     * @throws NullPointerException
     *         If registered as parallel capable and {@code className} is null
     */
    protected Object getClassLoadingLock(String className) {
        return this;
    }

    /**
     * Finds the class with the specified <a href="#binary-name">binary name</a>.
     * This method should be overridden by class loader implementations that
     * follow the delegation model for loading classes, and will be invoked by
     * the {@link #loadClass loadClass} method after checking the
     * parent class loader for the requested class.
     *
     * @implSpec The default implementation throws {@code ClassNotFoundException}.
     *
     * @param name
     *         The <a href="#binary-name">binary name</a> of the class
     *
     * @return The resulting {@code Class} object
     *
     * @throws ClassNotFoundException
     *          If the class could not be found
     */
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        throw new ClassNotFoundException(name);
    }

    private void preDefineClass(String name) {
        if (!checkName(name)) {
            throw new NoClassDefFoundError(String.str("IllegalName: ", name));
        }
    }

    private void postDefineClass(Class<?> c) {
        // define a named package, if not present
        getNamedPackage(c.getPackageName());
    }

    /**
     * Converts an array of bytes into an instance of class {@code Class}.
     *
     * Before the class can be used it must be resolved.
     *
     * You should always pass in the <a href="#binary-name">binary name</a> of the
     * class you are defining as well as the bytes.  This ensures that the
     * class you are defining is indeed the class you think it is.
     *
     * If the specified {@code name} begins with "{@code java.}", it can
     * only be defined by the {@linkplain #getPlatformClassLoader()
     * platform class loader} or its ancestors; otherwise {@code SecurityException}
     * will be thrown.  If {@code name} is not {@code null}, it must be equal to
     * the <a href="#binary-name">binary name</a> of the class
     * specified by the byte array {@code b}, otherwise a {@link
     * NoClassDefFoundError NoClassDefFoundError} will be thrown.
     *
     * This method defines a package in this class loader corresponding to the
     * package of the {@code Class} (if such a package has not already been defined
     * in this class loader). The name of the defined package is derived from
     * the <a href="#binary-name">binary name</a> of the class specified by
     * the byte array {@code b}.
     * Other properties of the defined package are as specified by {@link Package}.
     *
     * @param name
     *         The expected <a href="#binary-name">binary name</a> of the class, or
     *         {@code null} if not known
     * @param b
     *         The bytes that make up the class data. The bytes in positions
     *         {@code off} through {@code off+len-1} should have the format
     *         of a valid class file as defined by
     *         <cite>The Java&trade; Virtual Machine Specification</cite>.
     * @param off
     *         The start offset in {@code b} of the class data
     * @param len
     *         The length of the class data
     *
     * @return The {@code Class} object created from the data.
     *
     * @throws ClassFormatError
     *          If the data did not contain a valid class
     * @throws NoClassDefFoundError
     *          If {@code name} is not {@code null} and not equal to the
     *          <a href="#binary-name">binary name</a> of the class specified by {@code b}
     * @throws IndexOutOfBoundsException
     *          If either {@code off} or {@code len} is negative, or if
     *          {@code off+len} is greater than {@code b.length}.
     */
    protected final Class<?> defineClass(String name, byte[] b, int off, int len) throws ClassFormatError {
        preDefineClass(name);
        Class<?> c = defineClass1(this, name, b, off, len/* oops! , null */, null);
        postDefineClass(c);
        return c;
    }

    /**
     * Converts a {@link java.nio.ByteBuffer ByteBuffer} into an instance
     * of class {@code Class}.
     *
     * Before the class can be used it must be resolved.
     *
     * An invocation of this method of the form
     * <i>cl</i>{@code .defineClass(}<i>name</i>{@code ,}
     * <i>bBuffer</i>{@code )} yields exactly the same
     * result as the statements
     *
     *<code>
     * ...<br>
     * byte[] temp = new byte[bBuffer.{@link java.nio.ByteBuffer#remaining remaining}()];<br>
     *     bBuffer.{@link java.nio.ByteBuffer#get(byte[]) get}(temp);<br>
     *     return {@link #defineClass(String, byte[], int, int) cl.defineClass}(name, temp, 0, temp.length);<br>
     * </code>
     *
     * @param name
     *         The expected <a href="#binary-name">binary name</a>. of the class, or
     *         {@code null} if not known
     * @param b
     *         The bytes that make up the class data. The bytes from positions
     *         {@code b.position()} through {@code b.position() + b.limit() -1
     *         } should have the format of a valid class file as defined by
     *         <cite>The Java&trade; Virtual Machine Specification</cite>.
     *
     * @return The {@code Class} object created from the data.
     *
     * @throws ClassFormatError
     *          If the data did not contain a valid class.
     * @throws NoClassDefFoundError
     *          If {@code name} is not {@code null} and not equal to the
     *          <a href="#binary-name">binary name</a> of the class specified by {@code b}
     */
    protected final Class<?> defineClass(String name, java.nio.ByteBuffer b) throws ClassFormatError {
        int len = b.remaining();

        // Use byte[] if not a direct ByteBuffer:
        if (!b.isDirect()) {
            if (b.hasArray()) {
                return defineClass(name, b.array(), b.position() + b.arrayOffset(), len);
            } else {
                // no array, or read-only array
                byte[] tb = new byte[len];
                b.get(tb); // get bytes out of byte buffer.
                return defineClass(name, tb, 0, len);
            }
        }

        preDefineClass(name);
        Class<?> c = defineClass2(this, name, b, b.position(), len/* oops! , null */, null);
        postDefineClass(c);
        return c;
    }

    /* oops! */public static native Class<?> defineClass1(ClassLoader loader, String name, byte[] b, int off, int len/* oops! , ProtectionDomain pd */, String source);

    static native Class<?> defineClass2(ClassLoader loader, String name, java.nio.ByteBuffer b, int off, int len/* oops! , ProtectionDomain pd */, String source);

    // true if the name is null or has the potential to be a valid binary name
    private boolean checkName(String name) {
        if ((name == null) || (name.length() == 0))
            return true;
        if ((name.indexOf('/') != -1) || (name.charAt(0) == '['))
            return false;
        return true;
    }

    /**
     * Links the specified class.  This (misleadingly named) method may be
     * used by a class loader to link a class.  If the class {@code c} has
     * already been linked, then this method simply returns. Otherwise, the
     * class is linked as described in the "Execution" chapter of
     * <cite>The Java&trade; Language Specification</cite>.
     *
     * @param c
     *         The class to link
     *
     * @throws NullPointerException
     *          If {@code c} is {@code null}.
     */
    protected final void resolveClass(Class<?> c) {
        if (c == null) {
            throw new NullPointerException();
        }
    }

    /**
     * Finds a class with the specified <a href="#binary-name">binary name</a>,
     * loading it if necessary.
     *
     * This method loads the class through the system class loader (see
     * {@link #getSystemClassLoader()}).  The {@code Class} object returned
     * might have more than one {@code ClassLoader} associated with it.
     * Subclasses of {@code ClassLoader} need not usually invoke this method,
     * because most class loaders need to override just {@link
     * #findClass(String)}.
     *
     * @param name
     *         The <a href="#binary-name">binary name</a> of the class
     *
     * @return The {@code Class} object for the specified {@code name}
     *
     * @throws ClassNotFoundException
     *          If the class could not be found
     */
    protected final Class<?> findSystemClass(String name) throws ClassNotFoundException {
        return getSystemClassLoader().loadClass(name);
    }

    /**
     * Returns a class loaded by the bootstrap class loader;
     * or return null if not found.
     */
    /* oops! */public Class<?> findBootstrapClassOrNull(String name) {
        if (!checkName(name))
            return null;

        return findBootstrapClass(name);
    }

    // return null if not found
    private native Class<?> findBootstrapClass(String name);

    /**
     * Returns the class with the given <a href="#binary-name">binary name</a> if this
     * loader has been recorded by the Java virtual machine as an initiating
     * loader of a class with that <a href="#binary-name">binary name</a>.  Otherwise
     * {@code null} is returned.
     *
     * @param name
     *         The <a href="#binary-name">binary name</a> of the class
     *
     * @return The {@code Class} object, or {@code null} if the class has
     *          not been loaded
     */
    protected final Class<?> findLoadedClass(String name) {
        if (!checkName(name))
            return null;
        return findLoadedClass0(name);
    }

    private final native Class<?> findLoadedClass0(String name);

    /**
     * Sets the signers of a class.  This should be invoked after defining a
     * class.
     *
     * @param c
     *         The {@code Class} object
     *
     * @param signers
     *         The signers for the class
     */
    protected final void setSigners(Class<?> c, Object[] signers) {
        c.setSigners(signers);
    }

    // -- Hierarchy --

    /**
     * Returns the parent class loader for delegation. Some implementations may
     * use {@code null} to represent the bootstrap class loader. This method
     * will return {@code null} in such implementations if this class loader's
     * parent is the bootstrap class loader.
     *
     * @return The parent {@code ClassLoader}
     */
    // @CallerSensitive
    public final ClassLoader getParent() {
        return parent;
    }

    /**
     * Returns the platform class loader.  All
     * <a href="#builtinLoaders">platform classes</a> are visible to
     * the platform class loader.
     *
     * @implNote The name of the builtin platform class loader is
     * {@code "platform"}.
     *
     * @return The platform {@code ClassLoader}.
     */
    // @CallerSensitive
    public static ClassLoader getPlatformClassLoader() {
        return getBuiltinPlatformClassLoader();
    }

    /**
     * Returns the system class loader.  This is the default
     * delegation parent for new {@code ClassLoader} instances, and is
     * typically the class loader used to start the application.
     *
     * This method is first invoked early in the runtime's startup
     * sequence, at which point it creates the system class loader. This
     * class loader will be the context class loader for the main application
     * thread (for example, the thread that invokes the {@code main} method of
     * the main class).
     *
     * The default system class loader is an implementation-dependent
     * instance of this class.
     *
     * If the system property "{@code java.system.class.loader}" is defined
     * when this method is first invoked then the value of that property is
     * taken to be the name of a class that will be returned as the system
     * class loader.  The class is loaded using the default system class loader
     * and must define a public constructor that takes a single parameter of
     * type {@code ClassLoader} which is used as the delegation parent.  An
     * instance is then created using this constructor with the default system
     * class loader as the parameter.  The resulting class loader is defined
     * to be the system class loader. During construction, the class loader
     * should take great care to avoid calling {@code getSystemClassLoader()}.
     * If circular initialization of the system class loader is detected then
     * an {@code IllegalStateException} is thrown.
     *
     * @implNote The system property to override the system class loader is not
     * examined until the VM is almost fully initialized. Code that executes
     * this method during startup should take care not to cache the return
     * value until the system is fully initialized.
     *
     * The name of the built-in system class loader is {@code "app"}.
     * The system property "{@code java.class.path}" is read during early
     * initialization of the VM to determine the class path.
     * An empty value of "{@code java.class.path}" property is interpreted
     * differently depending on whether the initial module (the module
     * containing the main class) is named or unnamed:
     * If named, the built-in system class loader will have no class path and
     * will search for classes and resources using the application module path;
     * otherwise, if unnamed, it will set the class path to the current
     * working directory.
     *
     * @return The system {@code ClassLoader}
     *
     * @throws IllegalStateException
     *          If invoked recursively during the construction of the class
     *          loader specified by the "{@code java.system.class.loader}"
     *          property.
     *
     * @throws Error
     *          If the system property "{@code java.system.class.loader}"
     *          is defined but the named class could not be loaded, the
     *          provider class does not define the required constructor, or an
     *          exception is thrown by that constructor when it is invoked. The
     *          underlying cause of the error can be retrieved via the
     *          {@link Throwable#getCause()} method.
     */
    // @CallerSensitive
    public static ClassLoader getSystemClassLoader() {
        switch (VM.initLevel()) {
            case 0:
            case 1:
            case 2:
                // the system class loader is the built-in app class loader during startup
                return getBuiltinAppClassLoader();
            case 3:
                String msg = "getSystemClassLoader cannot be called during the system class loader instantiation";
                throw new IllegalStateException(msg);
            default:
                // system fully initialized
                return scl;
        }
    }

    static ClassLoader getBuiltinPlatformClassLoader() {
        return ClassLoaders.platformClassLoader();
    }

    static ClassLoader getBuiltinAppClassLoader() {
        return ClassLoaders.appClassLoader();
    }

    /*
     * Initialize the system class loader that may be a custom class on the
     * application class path or application module path.
     */
    static synchronized ClassLoader initSystemClassLoader() {
        if (VM.initLevel() != 3) {
            throw new InternalError(String.str("system class loader cannot be set at initLevel ", VM.initLevel()));
        }

        // detect recursive initialization
        if (scl != null) {
            throw new IllegalStateException("recursive invocation");
        }

        ClassLoader builtinLoader = getBuiltinAppClassLoader();

        // All are privileged frames.
        String cn = null; // "java.system.class.loader"
        if (cn != null) {
            try {
                // custom class loader is only supported to be loaded from unnamed module
                Constructor<?> ctor = Class.forName(cn, false, builtinLoader).getDeclaredConstructor(ClassLoader.class);
                scl = (ClassLoader) ctor.newInstance(builtinLoader);
            } catch (Exception e) {
                Throwable cause = e;
                if (e instanceof InvocationTargetException) {
                    cause = e.getCause();
                    if (cause instanceof Error) {
                        throw (Error) cause;
                    }
                }
                if (cause instanceof RuntimeException) {
                    throw (RuntimeException) cause;
                }
                throw new Error(cause.getMessage(), cause);
            }
        } else {
            scl = builtinLoader;
        }
        return scl;
    }

    // Returns the class's class loader, or null if none.
    static ClassLoader getClassLoader(Class<?> caller) {
        // This can be null if the VM is requesting it
        if (caller == null) {
            return null;
        }
        // Circumvent security check since this is package-private
        return caller.getClassLoader0();
    }

    // The system class loader
    // @GuardedBy("ClassLoader.class")
    private static volatile ClassLoader scl;

    // -- Package --

    /**
     * Define a Package of the given Class object.
     *
     * If the given class represents an array type, a primitive type or void,
     * this method returns {@code null}.
     *
     * This method does not throw IllegalArgumentException.
     */
    Package definePackage(Class<?> c) {
        if (c.isPrimitive() || c.isArray()) {
            return null;
        }

        return _definePackage(c.getPackageName());
    }

    /**
     * Defines a Package of the given name and module
     *
     * This method does not throw IllegalArgumentException.
     *
     * @param name package name
     * @param m    module
     */
    /* oops! */public Package _definePackage(String name) {
        NamedPackage pkg = packages.get(name);
        if (pkg instanceof Package)
            return (Package)pkg;

        return (Package)packages.compute(name, (n, p) -> toPackage(n, p));
    }

    /*
     * Returns a Package object for the named package
     */
    private Package toPackage(String name, NamedPackage p) {
        // define Package object if the named package is not yet defined
        if (p == null)
            return NamedPackage.toPackage(name);

        // otherwise, replace the NamedPackage object with Package object
        if (p instanceof Package)
            return (Package)p;

        return NamedPackage.toPackage(p.packageName());
    }

    /**
     * Defines a package by <a href="#binary-name">name</a> in this {@code ClassLoader}.
     *
     * <a href="#binary-name">Package names</a> must be unique within a class loader and
     * cannot be redefined or changed once created.
     *
     * If a class loader wishes to define a package with specific properties,
     * such as version information, then the class loader should call this
     * {@code definePackage} method before calling {@code defineClass}.
     * Otherwise, the
     * {@link #defineClass(String, byte[], int, int) defineClass}
     * method will define a package in this class loader corresponding to the package
     * of the newly defined class; the properties of this defined package are
     * specified by {@link Package}.
     *
     * @apiNote
     * It is strongly recommended that a class loader does not call this
     * method to explicitly define packages in <em>named modules</em>; instead,
     * the package will be automatically defined when a class is {@linkplain
     * #defineClass(String, byte[], int, int) being defined}.
     * If it is desirable to define {@code Package} explicitly, it should ensure
     * that all packages in a named module are defined with the properties
     * specified by {@link Package}.  Otherwise, some {@code Package} objects
     * in a named module may be for example sealed with different seal base.
     *
     * @param name
     *         The <a href="#binary-name">package name</a>
     *
     * @return The newly defined {@code Package} object
     *
     * @throws NullPointerException
     *          if {@code name} is {@code null}.
     * @throws IllegalArgumentException
     *          if a package of the given {@code name} is already
     *          defined by this class loader
     */
    protected Package definePackage(String name) {
        Objects.requireNonNull(name);

        // definePackage is not final and may be overridden by custom class loader
        Package p = new Package(name, this);

        if (packages.putIfAbsent(name, p) != null)
            throw new IllegalArgumentException(name);

        return p;
    }

    /**
     * Returns a {@code Package} of the given <a href="#binary-name">name</a> that
     * has been defined by this class loader.
     *
     * @param name The <a href="#binary-name">package name</a>
     *
     * @return The {@code Package} of the given name that has been defined
     *         by this class loader, or {@code null} if not found
     *
     * @throws NullPointerException
     *          if {@code name} is {@code null}.
     */
    public final Package getDefinedPackage(String name) {
        Objects.requireNonNull(name, "name cannot be null");

        NamedPackage p = packages.get(name);
        if (p == null)
            return null;

        return _definePackage(name);
    }

    // -- Misc --

    /**
     * Returns the Map used as a storage for ClassLoaderValue(s)
     * associated with this ClassLoader, creating it if it doesn't already exist.
     */
    /* oops! */public Map<?, ?> createOrGetClassLoaderValueMap() {
        Map<?, ?> map = classLoaderValueMap;
        if (map == null) {
            map = Collections.synchronizedMap(new HashMap<>());
            boolean set = trySetObjectField("classLoaderValueMap", map);
            if (!set) {
                // beaten by someone else
                map = classLoaderValueMap;
            }
        }
        return map;
    }

    // the storage for ClassLoaderValue(s) associated with this ClassLoader
    private volatile Map<?, ?> classLoaderValueMap;

    /**
     * Attempts to atomically set a volatile field in this object. Returns
     * {@code true} if not beaten by another thread. Avoids the use of
     * AtomicReferenceFieldUpdater in this class.
     */
    private boolean trySetObjectField(String name, Object obj) {
        Unsafe unsafe = Unsafe.getUnsafe();
        Class<?> k = ClassLoader.class;
        long offset;
        offset = unsafe.objectFieldOffset(k, name);
        return unsafe.compareAndSetObject(this, offset, null, obj);
    }
}

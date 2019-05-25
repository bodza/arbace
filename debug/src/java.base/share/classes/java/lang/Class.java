package java.lang;

import java.lang.ref.SoftReference;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.StringJoiner;

import jdk.internal.loader.BootLoader;
import jdk.internal.loader.BuiltinClassLoader;
import jdk.internal.misc.Unsafe;
import jdk.internal.misc.VM;
import jdk.internal.reflect.CallerSensitive;
import jdk.internal.reflect.ConstantPool;
import jdk.internal.reflect.Reflection;
import jdk.internal.reflect.ReflectionFactory;
import sun.reflect.generics.factory.CoreReflectionFactory;
import sun.reflect.generics.factory.GenericsFactory;
import sun.reflect.generics.repository.ClassRepository;
import sun.reflect.generics.repository.MethodRepository;
import sun.reflect.generics.repository.ConstructorRepository;
import sun.reflect.generics.scope.ClassScope;
import sun.reflect.misc.ReflectUtil;

/**
 * Instances of the class {@code Class} represent classes and interfaces
 * in a running Java application. An enum type is a kind of class and an
 * annotation type is a kind of interface. Every array also
 * belongs to a class that is reflected as a {@code Class} object
 * that is shared by all arrays with the same element type and number
 * of dimensions.  The primitive Java types ({@code boolean},
 * {@code byte}, {@code char}, {@code short},
 * {@code int}, {@code long}, {@code float}, and
 * {@code double}), and the keyword {@code void} are also
 * represented as {@code Class} objects.
 *
 * {@code Class} has no public constructor. Instead a {@code Class}
 * object is constructed automatically by the Java Virtual Machine
 * when a class loader invokes one of the
 * {@link ClassLoader#defineClass(String,byte[],int,int) defineClass} methods
 * and passes the bytes of a {@code class} file.
 *
 * The methods of class {@code Class} expose many characteristics of a
 * class or interface. Most characteristics are derived from the {@code class}
 * file that the class loader passed to the Java Virtual Machine. A few
 * characteristics are determined by the class loading environment at run time,
 * such as the module returned by {@link #getModule() getModule()}.
 *
 * Some methods of class {@code Class} expose whether the declaration of
 * a class or interface in Java source code was <em>enclosed</em> within
 * another declaration. Other methods describe how a class or interface
 * is situated in a <em>nest</em>. A <a id="nest">nest</a> is a set of
 * classes and interfaces, in the same run-time package, that
 * allow mutual access to their {@code private} members.
 * The classes and interfaces are known as <em>nestmates</em>.
 * One nestmate acts as the
 * <em>nest host</em>, and enumerates the other nestmates which
 * belong to the nest; each of them in turn records it as the nest host.
 * The classes and interfaces which belong to a nest, including its host, are
 * determined when
 * {@code class} files are generated, for example, a Java compiler
 * will typically record a top-level class as the host of a nest where the
 * other members are the classes and interfaces whose declarations are
 * enclosed within the top-level class declaration.
 *
 * The following example uses a {@code Class} object to print the
 * class name of an object:
 *
 * <blockquote><pre>
 *     void printClassName(Object obj) {
 *         System.out.println("The class of " + obj + " is " + obj.getClass().getName());
 *     }
 * </pre></blockquote>
 *
 * It is also possible to get the {@code Class} object for a named
 * type (or for void) using a class literal.  See Section 15.8.2 of
 * <cite>The Java&trade; Language Specification</cite>.
 * For example:
 *
 * <blockquote>
 *     {@code System.out.println("The name of class Foo is: "+Foo.class.getName());}
 * </blockquote>
 *
 * @param <T> the type of the class modeled by this {@code Class}
 * object.  For example, the type of {@code String.class} is {@code
 * Class<String>}.  Use {@code Class<?>} if the class being modeled is
 * unknown.
 */
public final class Class<T> implements GenericDeclaration, Type {
    private static final int ANNOTATION = 0x00002000;
    private static final int ENUM       = 0x00004000;
    private static final int SYNTHETIC  = 0x00001000;

    private static native void registerNatives();
    static {
        registerNatives();
    }

    /*
     * Private constructor. Only the Java Virtual Machine creates Class objects.
     * This constructor is not used and prevents the default constructor being
     * generated.
     */
    private Class(ClassLoader loader, Class<?> arrayComponentType) {
        // Initialize final field for classLoader.  The initialization value of non-null
        // prevents future JIT optimizations from assuming this final field is null.
        classLoader = loader;
        componentType = arrayComponentType;
    }

    /**
     * Converts the object to a string. The string representation is the
     * string "class" or "interface", followed by a space, and then by the
     * fully qualified name of the class in the format returned by
     * {@code getName}.  If this {@code Class} object represents a
     * primitive type, this method returns the name of the primitive type.  If
     * this {@code Class} object represents void this method returns
     * "void". If this {@code Class} object represents an array type,
     * this method returns "class " followed by {@code getName}.
     *
     * @return a string representation of this class object.
     */
    public String toString() {
        return (isInterface() ? "interface " : (isPrimitive() ? "" : "class ")) + getName();
    }

    /**
     * Returns a string describing this {@code Class}, including
     * information about modifiers and type parameters.
     *
     * The string is formatted as a list of type modifiers, if any,
     * followed by the kind of type (empty string for primitive types
     * and {@code class}, {@code enum}, {@code interface}, or
     * <code>&#64;</code>{@code interface}, as appropriate), followed
     * by the type's name, followed by an angle-bracketed
     * comma-separated list of the type's type parameters, if any.
     *
     * A space is used to separate modifiers from one another and to
     * separate any modifiers from the kind of type. The modifiers
     * occur in canonical order. If there are no type parameters, the
     * type parameter list is elided.
     *
     * For an array type, the string starts with the type name,
     * followed by an angle-bracketed comma-separated list of the
     * type's type parameters, if any, followed by a sequence of
     * {@code []} characters, one set of brackets per dimension of
     * the array.
     *
     * Note that since information about the runtime representation
     * of a type is being generated, modifiers not present on the
     * originating source code or illegal on the originating source
     * code may be present.
     *
     * @return a string describing this {@code Class}, including
     * information about modifiers and type parameters
     */
    public String toGenericString() {
        if (isPrimitive()) {
            return toString();
        } else {
            StringBuilder sb = new StringBuilder();
            Class<?> component = this;
            int arrayDepth = 0;

            if (isArray()) {
                do {
                    arrayDepth++;
                    component = component.getComponentType();
                } while (component.isArray());
                sb.append(component.getName());
            } else {
                // Class modifiers are a superset of interface modifiers
                int modifiers = getModifiers() & Modifier.classModifiers();
                if (modifiers != 0) {
                    sb.append(Modifier.toString(modifiers));
                    sb.append(' ');
                }

                if (isAnnotation()) {
                    sb.append('@');
                }
                if (isInterface()) { // Note: all annotation types are interfaces
                    sb.append("interface");
                } else {
                    if (isEnum())
                        sb.append("enum");
                    else
                        sb.append("class");
                }
                sb.append(' ');
                sb.append(getName());
            }

            TypeVariable<?>[] typeparms = component.getTypeParameters();
            if (typeparms.length > 0) {
                StringJoiner sj = new StringJoiner(",", "<", ">");
                for (TypeVariable<?> typeparm : typeparms) {
                    sj.add(typeparm.getTypeName());
                }
                sb.append(sj.toString());
            }

            for (int i = 0; i < arrayDepth; i++)
                sb.append("[]");

            return sb.toString();
        }
    }

    /**
     * Returns the {@code Class} object associated with the class or
     * interface with the given string name.  Invoking this method is
     * equivalent to:
     *
     * <blockquote>
     *  {@code Class.forName(className, true, currentLoader)}
     * </blockquote>
     *
     * where {@code currentLoader} denotes the defining class loader of
     * the current class.
     *
     * For example, the following code fragment returns the
     * runtime {@code Class} descriptor for the class named
     * {@code java.lang.Thread}:
     *
     * <blockquote>
     *   {@code Class t = Class.forName("java.lang.Thread")}
     * </blockquote>
     *
     * A call to {@code forName("X")} causes the class named
     * {@code X} to be initialized.
     *
     * @param className   the fully qualified name of the desired class.
     * @return the {@code Class} object for the class with the
     *             specified name.
     * @throws LinkageError if the linkage fails
     * @throws ExceptionInInitializerError if the initialization provoked
     *            by this method fails
     * @throws ClassNotFoundException if the class cannot be located
     */
    @CallerSensitive
    public static Class<?> forName(String className) throws ClassNotFoundException {
        Class<?> caller = Reflection.getCallerClass();
        return forName0(className, true, ClassLoader.getClassLoader(caller), caller);
    }

    /**
     * Returns the {@code Class} object associated with the class or
     * interface with the given string name, using the given class loader.
     * Given the fully qualified name for a class or interface (in the same
     * format returned by {@code getName}) this method attempts to
     * locate, load, and link the class or interface.  The specified class
     * loader is used to load the class or interface.  If the parameter
     * {@code loader} is null, the class is loaded through the bootstrap
     * class loader.  The class is initialized only if the
     * {@code initialize} parameter is {@code true} and if it has
     * not been initialized earlier.
     *
     * If {@code name} denotes a primitive type or void, an attempt
     * will be made to locate a user-defined class in the unnamed package whose
     * name is {@code name}. Therefore, this method cannot be used to
     * obtain any of the {@code Class} objects representing primitive
     * types or void.
     *
     * If {@code name} denotes an array class, the component type of
     * the array class is loaded but not initialized.
     *
     * For example, in an instance method the expression:
     *
     * <blockquote>
     *  {@code Class.forName("Foo")}
     * </blockquote>
     *
     * is equivalent to:
     *
     * <blockquote>
     *  {@code Class.forName("Foo", true, this.getClass().getClassLoader())}
     * </blockquote>
     *
     * Note that this method throws errors related to loading, linking or
     * initializing as specified in Sections 12.2, 12.3 and 12.4 of <em>The
     * Java Language Specification</em>.
     * Note that this method does not check whether the requested class
     * is accessible to its caller.
     *
     * @param name       fully qualified name of the desired class
     * @param initialize if {@code true} the class will be initialized.
     *                   See Section 12.4 of <em>The Java Language Specification</em>.
     * @param loader     class loader from which the class must be loaded
     * @return class object representing the desired class
     *
     * @throws LinkageError if the linkage fails
     * @throws ExceptionInInitializerError if the initialization provoked
     *            by this method fails
     * @throws ClassNotFoundException if the class cannot be located by
     *            the specified class loader
     */
    @CallerSensitive
    public static Class<?> forName(String name, boolean initialize, ClassLoader loader) throws ClassNotFoundException {
        Class<?> caller = null;
        return forName0(name, initialize, loader, caller);
    }

    /** Called after security check for system loader access checks have been made. */
    private static native Class<?> forName0(String name, boolean initialize, ClassLoader loader, Class<?> caller) throws ClassNotFoundException;

    /**
     * Creates a new instance of the class represented by this {@code Class}
     * object.  The class is instantiated as if by a {@code new}
     * expression with an empty argument list.  The class is initialized if it
     * has not already been initialized.
     *
     * @deprecated This method propagates any exception thrown by the
     * nullary constructor, including a checked exception.  Use of
     * this method effectively bypasses the compile-time exception
     * checking that would otherwise be performed by the compiler.
     * The {@link
     * java.lang.reflect.Constructor#newInstance(java.lang.Object...)
     * Constructor.newInstance} method avoids this problem by wrapping
     * any exception thrown by the constructor in a (checked) {@link
     * java.lang.reflect.InvocationTargetException}.
     *
     * The call
     *
     * <pre>{@code
     * clazz.newInstance()
     * }</pre>
     *
     * can be replaced by
     *
     * <pre>{@code
     * clazz.getDeclaredConstructor().newInstance()
     * }</pre>
     *
     * The latter sequence of calls is inferred to be able to throw
     * the additional exception types {@link
     * InvocationTargetException} and {@link
     * NoSuchMethodException}. Both of these exception types are
     * subclasses of {@link ReflectiveOperationException}.
     *
     * @return a newly allocated instance of the class represented by this
     *          object.
     * @throws IllegalAccessException  if the class or its nullary
     *          constructor is not accessible.
     * @throws InstantiationException
     *          if this {@code Class} represents an abstract class,
     *          an interface, an array class, a primitive type, or void;
     *          or if the class has no nullary constructor;
     *          or if the instantiation fails for some other reason.
     * @throws ExceptionInInitializerError if the initialization
     *          provoked by this method fails.
     */
    @CallerSensitive
    // @Deprecated(since="9")
    public T newInstance() throws InstantiationException, IllegalAccessException {
        // NOTE: the following code may not be strictly correct under
        // the current Java memory model.

        // Constructor lookup
        if (cachedConstructor == null) {
            if (this == Class.class) {
                throw new IllegalAccessException("Can not call newInstance() on the Class for java.lang.Class");
            }
            try {
                Class<?>[] empty = {};
                final Constructor<T> c = getReflectionFactory().copyConstructor(getConstructor0(empty, Member.DECLARED));
                // Disable accessibility checks on the constructor
                // since we have to do the security check here anyway
                // (the stack depth is wrong for the Constructor's
                // security check to work)
                c.setAccessible(true);
                cachedConstructor = c;
            } catch (NoSuchMethodException e) {
                throw (InstantiationException) new InstantiationException(getName()).initCause(e);
            }
        }
        Constructor<T> tmpConstructor = cachedConstructor;
        // Security check (same as in java.lang.reflect.Constructor)
        Class<?> caller = Reflection.getCallerClass();
        if (newInstanceCallerCache != caller) {
            int modifiers = tmpConstructor.getModifiers();
            Reflection.ensureMemberAccess(caller, this, this, modifiers);
            newInstanceCallerCache = caller;
        }
        // Run constructor
        try {
            return tmpConstructor.newInstance((Object[])null);
        } catch (InvocationTargetException e) {
            Unsafe.getUnsafe().throwException(e.getTargetException());
            // Not reached
            return null;
        }
    }
    private transient volatile Constructor<T> cachedConstructor;
    private transient volatile Class<?>       newInstanceCallerCache;

    /**
     * Determines if the specified {@code Object} is assignment-compatible
     * with the object represented by this {@code Class}.  This method is
     * the dynamic equivalent of the Java language {@code instanceof}
     * operator. The method returns {@code true} if the specified
     * {@code Object} argument is non-null and can be cast to the
     * reference type represented by this {@code Class} object without
     * raising a {@code ClassCastException.} It returns {@code false}
     * otherwise.
     *
     * Specifically, if this {@code Class} object represents a
     * declared class, this method returns {@code true} if the specified
     * {@code Object} argument is an instance of the represented class (or
     * of any of its subclasses); it returns {@code false} otherwise. If
     * this {@code Class} object represents an array class, this method
     * returns {@code true} if the specified {@code Object} argument
     * can be converted to an object of the array class by an identity
     * conversion or by a widening reference conversion; it returns
     * {@code false} otherwise. If this {@code Class} object
     * represents an interface, this method returns {@code true} if the
     * class or any superclass of the specified {@code Object} argument
     * implements this interface; it returns {@code false} otherwise. If
     * this {@code Class} object represents a primitive type, this method
     * returns {@code false}.
     *
     * @param obj the object to check
     * @return true if {@code obj} is an instance of this class
     */
    // @HotSpotIntrinsicCandidate
    public native boolean isInstance(Object obj);

    /**
     * Determines if the class or interface represented by this
     * {@code Class} object is either the same as, or is a superclass or
     * superinterface of, the class or interface represented by the specified
     * {@code Class} parameter. It returns {@code true} if so;
     * otherwise it returns {@code false}. If this {@code Class}
     * object represents a primitive type, this method returns
     * {@code true} if the specified {@code Class} parameter is
     * exactly this {@code Class} object; otherwise it returns
     * {@code false}.
     *
     * Specifically, this method tests whether the type represented by the
     * specified {@code Class} parameter can be converted to the type
     * represented by this {@code Class} object via an identity conversion
     * or via a widening reference conversion. See <em>The Java Language
     * Specification</em>, sections 5.1.1 and 5.1.4, for details.
     *
     * @param cls the {@code Class} object to be checked
     * @return the {@code boolean} value indicating whether objects of the
     * type {@code cls} can be assigned to objects of this class
     * @throws NullPointerException if the specified Class parameter is
     *            null.
     */
    // @HotSpotIntrinsicCandidate
    public native boolean isAssignableFrom(Class<?> cls);

    /**
     * Determines if the specified {@code Class} object represents an
     * interface type.
     *
     * @return {@code true} if this object represents an interface;
     *          {@code false} otherwise.
     */
    // @HotSpotIntrinsicCandidate
    public native boolean isInterface();

    /**
     * Determines if this {@code Class} object represents an array class.
     *
     * @return {@code true} if this object represents an array class;
     *          {@code false} otherwise.
     */
    // @HotSpotIntrinsicCandidate
    public native boolean isArray();

    /**
     * Determines if the specified {@code Class} object represents a
     * primitive type.
     *
     * There are nine predefined {@code Class} objects to represent
     * the eight primitive types and void.  These are created by the Java
     * Virtual Machine, and have the same names as the primitive types that
     * they represent, namely {@code boolean}, {@code byte},
     * {@code char}, {@code short}, {@code int},
     * {@code long}, {@code float}, and {@code double}.
     *
     * These objects may only be accessed via the following public static
     * final variables, and are the only {@code Class} objects for which
     * this method returns {@code true}.
     *
     * @return true if and only if this class represents a primitive type
     */
    // @HotSpotIntrinsicCandidate
    public native boolean isPrimitive();

    /**
     * Returns true if this {@code Class} object represents an annotation
     * type.  Note that if this method returns true, {@link #isInterface()}
     * would also return true, as all annotation types are also interfaces.
     *
     * @return {@code true} if this class object represents an annotation
     *      type; {@code false} otherwise
     */
    public boolean isAnnotation() {
        return (getModifiers() & ANNOTATION) != 0;
    }

    /**
     * Returns {@code true} if this class is a synthetic class;
     * returns {@code false} otherwise.
     * @return {@code true} if and only if this class is a synthetic class as
     *         defined by the Java Language Specification.
     */
    public boolean isSynthetic() {
        return (getModifiers() & SYNTHETIC) != 0;
    }

    /**
     * Returns the  name of the entity (class, interface, array class,
     * primitive type, or void) represented by this {@code Class} object,
     * as a {@code String}.
     *
     * If this class object represents a reference type that is not an
     * array type then the binary name of the class is returned, as specified
     * by
     * <cite>The Java&trade; Language Specification</cite>.
     *
     * If this class object represents a primitive type or void, then the
     * name returned is a {@code String} equal to the Java language
     * keyword corresponding to the primitive type or void.
     *
     * If this class object represents a class of arrays, then the internal
     * form of the name consists of the name of the element type preceded by
     * one or more '{@code [}' characters representing the depth of the array
     * nesting.  The encoding of element type names is as follows:
     *
     * <blockquote><table class="striped">
     * <caption style="display:none">Element types and encodings</caption>
     * <thead>
     * <tr><th scope="col"> Element Type <th scope="col"> Encoding
     * </thead>
     * <tbody style="text-align:left">
     * <tr><th scope="row"> boolean      <td style="text-align:center"> Z
     * <tr><th scope="row"> byte         <td style="text-align:center"> B
     * <tr><th scope="row"> char         <td style="text-align:center"> C
     * <tr><th scope="row"> class or interface
     *                                   <td style="text-align:center"> L<i>classname</i>;
     * <tr><th scope="row"> double       <td style="text-align:center"> D
     * <tr><th scope="row"> float        <td style="text-align:center"> F
     * <tr><th scope="row"> int          <td style="text-align:center"> I
     * <tr><th scope="row"> long         <td style="text-align:center"> J
     * <tr><th scope="row"> short        <td style="text-align:center"> S
     * </tbody>
     * </table></blockquote>
     *
     * The class or interface name <i>classname</i> is the binary name of
     * the class specified above.
     *
     * Examples:
     * <blockquote><pre>
     * String.class.getName()
     *     returns "java.lang.String"
     * byte.class.getName()
     *     returns "byte"
     * (new Object[3]).getClass().getName()
     *     returns "[Ljava.lang.Object;"
     * (new int[3][4][5][6][7][8][9]).getClass().getName()
     *     returns "[[[[[[[I"
     * </pre></blockquote>
     *
     * @return the name of the class or interface
     *          represented by this object.
     */
    public String getName() {
        String name = this.name;
        return name != null ? name : initClassName();
    }

    // Cache the name to reduce the number of calls into the VM.
    // This field would be set by VM itself during initClassName call.
    private transient String name;
    private native String initClassName();

    /**
     * Returns the class loader for the class.  Some implementations may use
     * null to represent the bootstrap class loader. This method will return
     * null in such implementations if this class was loaded by the bootstrap
     * class loader.
     *
     * If this object
     * represents a primitive type or void, null is returned.
     *
     * @return the class loader that loaded the class or interface
     *          represented by this object.
     */
    @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public ClassLoader getClassLoader() {
        return getClassLoader0();
    }

    // Package-private to allow ClassLoader access
    ClassLoader getClassLoader0() { return classLoader; }

    // Initialized in JVM not by private constructor
    // This field is filtered from reflection access, i.e. getDeclaredField
    // will throw NoSuchFieldException
    private final ClassLoader classLoader;

    /**
     * Returns an array of {@code TypeVariable} objects that represent the
     * type variables declared by the generic declaration represented by this
     * {@code GenericDeclaration} object, in declaration order.  Returns an
     * array of length 0 if the underlying generic declaration declares no type
     * variables.
     *
     * @return an array of {@code TypeVariable} objects that represent
     *     the type variables declared by this generic declaration
     * @throws java.lang.reflect.GenericSignatureFormatError if the generic
     *     signature of this generic declaration does not conform to
     *     the format specified in
     *     <cite>The Java&trade; Virtual Machine Specification</cite>
     */
    @SuppressWarnings("unchecked")
    public TypeVariable<Class<T>>[] getTypeParameters() {
        ClassRepository info = getGenericInfo();
        if (info != null)
            return (TypeVariable<Class<T>>[])info.getTypeParameters();
        else
            return (TypeVariable<Class<T>>[])new TypeVariable<?>[0];
    }

    /**
     * Returns the {@code Class} representing the direct superclass of the
     * entity (class, interface, primitive type or void) represented by
     * this {@code Class}.  If this {@code Class} represents either the
     * {@code Object} class, an interface, a primitive type, or void, then
     * null is returned.  If this object represents an array class then the
     * {@code Class} object representing the {@code Object} class is
     * returned.
     *
     * @return the direct superclass of the class represented by this object
     */
    // @HotSpotIntrinsicCandidate
    public native Class<? super T> getSuperclass();

    /**
     * Returns the {@code Type} representing the direct superclass of
     * the entity (class, interface, primitive type or void) represented by
     * this {@code Class}.
     *
     * If the superclass is a parameterized type, the {@code Type}
     * object returned must accurately reflect the actual type
     * parameters used in the source code. The parameterized type
     * representing the superclass is created if it had not been
     * created before. See the declaration of {@link
     * java.lang.reflect.ParameterizedType ParameterizedType} for the
     * semantics of the creation process for parameterized types.  If
     * this {@code Class} represents either the {@code Object}
     * class, an interface, a primitive type, or void, then null is
     * returned.  If this object represents an array class then the
     * {@code Class} object representing the {@code Object} class is
     * returned.
     *
     * @throws java.lang.reflect.GenericSignatureFormatError if the generic
     *     class signature does not conform to the format specified in
     *     <cite>The Java&trade; Virtual Machine Specification</cite>
     * @throws TypeNotPresentException if the generic superclass
     *     refers to a non-existent type declaration
     * @throws java.lang.reflect.MalformedParameterizedTypeException if the
     *     generic superclass refers to a parameterized type that cannot be
     *     instantiated  for any reason
     * @return the direct superclass of the class represented by this object
     */
    public Type getGenericSuperclass() {
        ClassRepository info = getGenericInfo();
        if (info == null) {
            return getSuperclass();
        }

        // Historical irregularity:
        // Generic signature marks interfaces with superclass = Object
        // but this API returns null for interfaces
        if (isInterface()) {
            return null;
        }

        return info.getSuperclass();
    }

    /**
     * Gets the package of this class.
     *
     * If this class represents an array type, a primitive type or void,
     * this method returns {@code null}.
     *
     * @return the package of this class.
     */
    public Package getPackage() {
        if (isPrimitive() || isArray()) {
            return null;
        }
        ClassLoader cl = getClassLoader0();
        return cl != null ? cl.definePackage(this) : BootLoader.definePackage(this);
    }

    /**
     * Returns the fully qualified package name.
     *
     * If this class is a top level class, then this method returns the fully
     * qualified name of the package that the class is a member of, or the
     * empty string if the class is in an unnamed package.
     *
     * If this class is a member class, then this method is equivalent to
     * invoking {@code getPackageName()} on the {@linkplain #getEnclosingClass
     * enclosing class}.
     *
     * If this class is a {@linkplain #isLocalClass local class} or an {@linkplain
     * #isAnonymousClass() anonymous class}, then this method is equivalent to
     * invoking {@code getPackageName()} on the {@linkplain #getDeclaringClass
     * declaring class} of the {@linkplain #getEnclosingMethod enclosing method} or
     * {@linkplain #getEnclosingConstructor enclosing constructor}.
     *
     * If this class represents an array type then this method returns the
     * package name of the element type. If this class represents a primitive
     * type or void then the package name "{@code java.lang}" is returned.
     *
     * @return the fully qualified package name
     */
    public String getPackageName() {
        String pn = this.packageName;
        if (pn == null) {
            Class<?> c = this;
            while (c.isArray()) {
                c = c.getComponentType();
            }
            if (c.isPrimitive()) {
                pn = "java.lang";
            } else {
                String cn = c.getName();
                int dot = cn.lastIndexOf('.');
                pn = (dot != -1) ? cn.substring(0, dot).intern() : "";
            }
            this.packageName = pn;
        }
        return pn;
    }

    // cached package name
    private transient String packageName;

    /**
     * Returns the interfaces directly implemented by the class or interface
     * represented by this object.
     *
     * If this object represents a class, the return value is an array
     * containing objects representing all interfaces directly implemented by
     * the class.  The order of the interface objects in the array corresponds
     * to the order of the interface names in the {@code implements} clause of
     * the declaration of the class represented by this object.  For example,
     * given the declaration:
     * <blockquote>
     * {@code class Shimmer implements FloorWax, DessertTopping { ... }}
     * </blockquote>
     * suppose the value of {@code s} is an instance of
     * {@code Shimmer}; the value of the expression:
     * <blockquote>
     * {@code s.getClass().getInterfaces()[0]}
     * </blockquote>
     * is the {@code Class} object that represents interface
     * {@code FloorWax}; and the value of:
     * <blockquote>
     * {@code s.getClass().getInterfaces()[1]}
     * </blockquote>
     * is the {@code Class} object that represents interface
     * {@code DessertTopping}.
     *
     * If this object represents an interface, the array contains objects
     * representing all interfaces directly extended by the interface.  The
     * order of the interface objects in the array corresponds to the order of
     * the interface names in the {@code extends} clause of the declaration of
     * the interface represented by this object.
     *
     * If this object represents a class or interface that implements no
     * interfaces, the method returns an array of length 0.
     *
     * If this object represents a primitive type or void, the method
     * returns an array of length 0.
     *
     * @return an array of interfaces directly implemented by this class
     */
    public Class<?>[] getInterfaces() {
        // defensively copy before handing over to user code
        return getInterfaces(true);
    }

    private Class<?>[] getInterfaces(boolean cloneArray) {
        ReflectionData<T> rd = reflectionData();
        if (rd == null) {
            // no cloning required
            return getInterfaces0();
        } else {
            Class<?>[] interfaces = rd.interfaces;
            if (interfaces == null) {
                interfaces = getInterfaces0();
                rd.interfaces = interfaces;
            }
            // defensively copy if requested
            return cloneArray ? interfaces.clone() : interfaces;
        }
    }

    private native Class<?>[] getInterfaces0();

    /**
     * Returns the {@code Type}s representing the interfaces
     * directly implemented by the class or interface represented by
     * this object.
     *
     * If a superinterface is a parameterized type, the
     * {@code Type} object returned for it must accurately reflect
     * the actual type parameters used in the source code. The
     * parameterized type representing each superinterface is created
     * if it had not been created before. See the declaration of
     * {@link java.lang.reflect.ParameterizedType ParameterizedType}
     * for the semantics of the creation process for parameterized
     * types.
     *
     * If this object represents a class, the return value is an array
     * containing objects representing all interfaces directly implemented by
     * the class.  The order of the interface objects in the array corresponds
     * to the order of the interface names in the {@code implements} clause of
     * the declaration of the class represented by this object.
     *
     * If this object represents an interface, the array contains objects
     * representing all interfaces directly extended by the interface.  The
     * order of the interface objects in the array corresponds to the order of
     * the interface names in the {@code extends} clause of the declaration of
     * the interface represented by this object.
     *
     * If this object represents a class or interface that implements no
     * interfaces, the method returns an array of length 0.
     *
     * If this object represents a primitive type or void, the method
     * returns an array of length 0.
     *
     * @throws java.lang.reflect.GenericSignatureFormatError
     *     if the generic class signature does not conform to the format
     *     specified in
     *     <cite>The Java&trade; Virtual Machine Specification</cite>
     * @throws TypeNotPresentException if any of the generic
     *     superinterfaces refers to a non-existent type declaration
     * @throws java.lang.reflect.MalformedParameterizedTypeException
     *     if any of the generic superinterfaces refer to a parameterized
     *     type that cannot be instantiated for any reason
     * @return an array of interfaces directly implemented by this class
     */
    public Type[] getGenericInterfaces() {
        ClassRepository info = getGenericInfo();
        return (info == null) ?  getInterfaces() : info.getSuperInterfaces();
    }

    /**
     * Returns the {@code Class} representing the component type of an
     * array.  If this class does not represent an array class this method
     * returns null.
     *
     * @return the {@code Class} representing the component type of this
     * class if this class is an array
     */
    public Class<?> getComponentType() {
        // Only return for array types. Storage may be reused for Class for instance types.
        if (isArray()) {
            return componentType;
        } else {
            return null;
        }
    }

    private final Class<?> componentType;

    /**
     * Returns the Java language modifiers for this class or interface, encoded
     * in an integer. The modifiers consist of the Java Virtual Machine's
     * constants for {@code public}, {@code protected},
     * {@code private}, {@code final}, {@code static},
     * {@code abstract} and {@code interface}; they should be decoded
     * using the methods of class {@code Modifier}.
     *
     * If the underlying class is an array class, then its
     * {@code public}, {@code private} and {@code protected}
     * modifiers are the same as those of its component type.  If this
     * {@code Class} represents a primitive type or void, its
     * {@code public} modifier is always {@code true}, and its
     * {@code protected} and {@code private} modifiers are always
     * {@code false}. If this object represents an array class, a
     * primitive type or void, then its {@code final} modifier is always
     * {@code true} and its interface modifier is always
     * {@code false}. The values of its other modifiers are not determined
     * by this specification.
     *
     * The modifier encodings are defined in <em>The Java Virtual Machine
     * Specification</em>, table 4.1.
     *
     * @return the {@code int} representing the modifiers for this class
     */
    // @HotSpotIntrinsicCandidate
    public native int getModifiers();

    /**
     * Gets the signers of this class.
     *
     * @return the signers of this class, or null if there are no signers.  In
     *          particular, this method returns null if this object represents
     *          a primitive type or void.
     */
    public native Object[] getSigners();

    /**
     * Set the signers of this class.
     */
    native void setSigners(Object[] signers);

    /**
     * If this {@code Class} object represents a local or anonymous
     * class within a method, returns a {@link
     * java.lang.reflect.Method Method} object representing the
     * immediately enclosing method of the underlying class. Returns
     * {@code null} otherwise.
     *
     * In particular, this method returns {@code null} if the underlying
     * class is a local or anonymous class immediately enclosed by a type
     * declaration, instance initializer or static initializer.
     *
     * @return the immediately enclosing method of the underlying class, if
     *     that class is a local or anonymous class; otherwise {@code null}.
     */
    @CallerSensitive
    public Method getEnclosingMethod() throws SecurityException {
        EnclosingMethodInfo enclosingInfo = getEnclosingMethodInfo();

        if (enclosingInfo == null)
            return null;
        else {
            if (!enclosingInfo.isMethod())
                return null;

            MethodRepository typeInfo = MethodRepository.make(enclosingInfo.getDescriptor(), getFactory());
            Class<?>   returnType       = toClass(typeInfo.getReturnType());
            Type []    parameterTypes   = typeInfo.getParameterTypes();
            Class<?>[] parameterClasses = new Class<?>[parameterTypes.length];

            // Convert Types to Classes; returned types *should*
            // be class objects since the methodDescriptor's used
            // don't have generics information
            for (int i = 0; i < parameterClasses.length; i++)
                parameterClasses[i] = toClass(parameterTypes[i]);

            // Perform access check
            final Class<?> enclosingCandidate = enclosingInfo.getEnclosingClass();
            Method[] candidates = enclosingCandidate.privateGetDeclaredMethods(false);

            /*
             * Loop over all declared methods; match method name,
             * number of and type of parameters, *and* return
             * type.  Matching return type is also necessary
             * because of covariant returns, etc.
             */
            ReflectionFactory fact = getReflectionFactory();
            for (Method m : candidates) {
                if (m.getName().equals(enclosingInfo.getName()) && arrayContentsEq(parameterClasses, fact.getExecutableSharedParameterTypes(m))) {
                    // finally, check return type
                    if (m.getReturnType().equals(returnType)) {
                        return fact.copyMethod(m);
                    }
                }
            }

            throw new InternalError("Enclosing method not found");
        }
    }

    private native Object[] getEnclosingMethod0();

    private EnclosingMethodInfo getEnclosingMethodInfo() {
        Object[] enclosingInfo = getEnclosingMethod0();
        if (enclosingInfo == null)
            return null;
        else {
            return new EnclosingMethodInfo(enclosingInfo);
        }
    }

    private static final class EnclosingMethodInfo {
        private final Class<?> enclosingClass;
        private final String name;
        private final String descriptor;

        static void validate(Object[] enclosingInfo) {
            if (enclosingInfo.length != 3)
                throw new InternalError("Malformed enclosing method information");
            try {
                // The array is expected to have three elements:

                // the immediately enclosing class
                Class<?> enclosingClass = (Class<?>)enclosingInfo[0];
                assert (enclosingClass != null);

                // the immediately enclosing method or constructor's
                // name (can be null).
                String name = (String)enclosingInfo[1];

                // the immediately enclosing method or constructor's
                // descriptor (null iff name is).
                String descriptor = (String)enclosingInfo[2];
                assert ((name != null && descriptor != null) || name == descriptor);
            } catch (ClassCastException cce) {
                throw new InternalError("Invalid type in enclosing method information", cce);
            }
        }

        EnclosingMethodInfo(Object[] enclosingInfo) {
            validate(enclosingInfo);
            this.enclosingClass = (Class<?>)enclosingInfo[0];
            this.name = (String)enclosingInfo[1];
            this.descriptor = (String)enclosingInfo[2];
        }

        boolean isPartial() {
            return enclosingClass == null || name == null || descriptor == null;
        }

        boolean isConstructor() { return !isPartial() && "<init>".equals(name); }

        boolean isMethod() { return !isPartial() && !isConstructor() && !"<clinit>".equals(name); }

        Class<?> getEnclosingClass() { return enclosingClass; }

        String getName() { return name; }

        String getDescriptor() { return descriptor; }
    }

    private static Class<?> toClass(Type o) {
        if (o instanceof GenericArrayType)
            return Array.newInstance(toClass(((GenericArrayType)o).getGenericComponentType()), 0).getClass();
        return (Class<?>)o;
     }

    /**
     * If this {@code Class} object represents a local or anonymous
     * class within a constructor, returns a {@link
     * java.lang.reflect.Constructor Constructor} object representing
     * the immediately enclosing constructor of the underlying
     * class. Returns {@code null} otherwise.  In particular, this
     * method returns {@code null} if the underlying class is a local
     * or anonymous class immediately enclosed by a type declaration,
     * instance initializer or static initializer.
     *
     * @return the immediately enclosing constructor of the underlying class, if
     *     that class is a local or anonymous class; otherwise {@code null}.
     */
    @CallerSensitive
    public Constructor<?> getEnclosingConstructor() throws SecurityException {
        EnclosingMethodInfo enclosingInfo = getEnclosingMethodInfo();

        if (enclosingInfo == null)
            return null;
        else {
            if (!enclosingInfo.isConstructor())
                return null;

            ConstructorRepository typeInfo = ConstructorRepository.make(enclosingInfo.getDescriptor(), getFactory());
            Type []    parameterTypes   = typeInfo.getParameterTypes();
            Class<?>[] parameterClasses = new Class<?>[parameterTypes.length];

            // Convert Types to Classes; returned types *should*
            // be class objects since the methodDescriptor's used
            // don't have generics information
            for (int i = 0; i < parameterClasses.length; i++)
                parameterClasses[i] = toClass(parameterTypes[i]);

            // Perform access check
            final Class<?> enclosingCandidate = enclosingInfo.getEnclosingClass();

            Constructor<?>[] candidates = enclosingCandidate.privateGetDeclaredConstructors(false);
            /*
             * Loop over all declared constructors; match number
             * of and type of parameters.
             */
            ReflectionFactory fact = getReflectionFactory();
            for (Constructor<?> c : candidates) {
                if (arrayContentsEq(parameterClasses, fact.getExecutableSharedParameterTypes(c))) {
                    return fact.copyConstructor(c);
                }
            }

            throw new InternalError("Enclosing constructor not found");
        }
    }

    /**
     * If the class or interface represented by this {@code Class} object
     * is a member of another class, returns the {@code Class} object
     * representing the class in which it was declared.  This method returns
     * null if this class or interface is not a member of any other class.  If
     * this {@code Class} object represents an array class, a primitive
     * type, or void,then this method returns null.
     *
     * @return the declaring class for this class
     */
    @CallerSensitive
    public Class<?> getDeclaringClass() throws SecurityException {
        final Class<?> candidate = getDeclaringClass0();

        return candidate;
    }

    private native Class<?> getDeclaringClass0();

    /**
     * Returns the immediately enclosing class of the underlying
     * class.  If the underlying class is a top level class this
     * method returns {@code null}.
     * @return the immediately enclosing class of the underlying class
     */
    @CallerSensitive
    public Class<?> getEnclosingClass() throws SecurityException {
        // There are five kinds of classes (or interfaces):
        // a) Top level classes
        // b) Nested classes (static member classes)
        // c) Inner classes (non-static member classes)
        // d) Local classes (named classes declared within a method)
        // e) Anonymous classes

        // JVM Spec 4.7.7: A class must have an EnclosingMethod
        // attribute if and only if it is a local class or an
        // anonymous class.
        EnclosingMethodInfo enclosingInfo = getEnclosingMethodInfo();
        Class<?> enclosingCandidate;

        if (enclosingInfo == null) {
            // This is a top level or a nested class or an inner class (a, b, or c)
            enclosingCandidate = getDeclaringClass0();
        } else {
            Class<?> enclosingClass = enclosingInfo.getEnclosingClass();
            // This is a local class or an anonymous class (d or e)
            if (enclosingClass == this || enclosingClass == null)
                throw new InternalError("Malformed enclosing method information");
            else
                enclosingCandidate = enclosingClass;
        }

        return enclosingCandidate;
    }

    /**
     * Returns the simple name of the underlying class as given in the
     * source code. Returns an empty string if the underlying class is
     * anonymous.
     *
     * The simple name of an array is the simple name of the
     * component type with "[]" appended.  In particular the simple
     * name of an array whose component type is anonymous is "[]".
     *
     * @return the simple name of the underlying class
     */
    public String getSimpleName() {
        ReflectionData<T> rd = reflectionData();
        String simpleName = rd.simpleName;
        if (simpleName == null) {
            rd.simpleName = simpleName = getSimpleName0();
        }
        return simpleName;
    }

    private String getSimpleName0() {
        if (isArray()) {
            return getComponentType().getSimpleName() + "[]";
        }
        String simpleName = getSimpleBinaryName();
        if (simpleName == null) { // top level class
            simpleName = getName();
            simpleName = simpleName.substring(simpleName.lastIndexOf('.') + 1); // strip the package name
        }
        return simpleName;
    }

    /**
     * Return an informative string for the name of this type.
     *
     * @return an informative string for the name of this type
     */
    public String getTypeName() {
        if (isArray()) {
            try {
                Class<?> cl = this;
                int dimensions = 0;
                do {
                    dimensions++;
                    cl = cl.getComponentType();
                } while (cl.isArray());
                StringBuilder sb = new StringBuilder();
                sb.append(cl.getName());
                for (int i = 0; i < dimensions; i++) {
                    sb.append("[]");
                }
                return sb.toString();
            } catch (Throwable e) { /*FALLTHRU*/ }
        }
        return getName();
    }

    /**
     * Returns the canonical name of the underlying class as
     * defined by the Java Language Specification.  Returns null if
     * the underlying class does not have a canonical name (i.e., if
     * it is a local or anonymous class or an array whose component
     * type does not have a canonical name).
     * @return the canonical name of the underlying class if it exists, and
     * {@code null} otherwise.
     */
    public String getCanonicalName() {
        ReflectionData<T> rd = reflectionData();
        String canonicalName = rd.canonicalName;
        if (canonicalName == null) {
            rd.canonicalName = canonicalName = getCanonicalName0();
        }
        return canonicalName == ReflectionData.NULL_SENTINEL ? null : canonicalName;
    }

    private String getCanonicalName0() {
        if (isArray()) {
            String canonicalName = getComponentType().getCanonicalName();
            if (canonicalName != null)
                return canonicalName + "[]";
            else
                return ReflectionData.NULL_SENTINEL;
        }
        if (isLocalOrAnonymousClass())
            return ReflectionData.NULL_SENTINEL;
        Class<?> enclosingClass = getEnclosingClass();
        if (enclosingClass == null) { // top level class
            return getName();
        } else {
            String enclosingName = enclosingClass.getCanonicalName();
            if (enclosingName == null)
                return ReflectionData.NULL_SENTINEL;
            return enclosingName + "." + getSimpleName();
        }
    }

    /**
     * Returns {@code true} if and only if the underlying class
     * is an anonymous class.
     *
     * @return {@code true} if and only if this class is an anonymous class.
     */
    public boolean isAnonymousClass() {
        return !isArray() && isLocalOrAnonymousClass() && getSimpleBinaryName0() == null;
    }

    /**
     * Returns {@code true} if and only if the underlying class
     * is a local class.
     *
     * @return {@code true} if and only if this class is a local class.
     */
    public boolean isLocalClass() {
        return isLocalOrAnonymousClass() && (isArray() || getSimpleBinaryName0() != null);
    }

    /**
     * Returns {@code true} if and only if the underlying class
     * is a member class.
     *
     * @return {@code true} if and only if this class is a member class.
     */
    public boolean isMemberClass() {
        return !isLocalOrAnonymousClass() && getDeclaringClass0() != null;
    }

    /**
     * Returns the "simple binary name" of the underlying class, i.e.,
     * the binary name without the leading enclosing class name.
     * Returns {@code null} if the underlying class is a top level
     * class.
     */
    private String getSimpleBinaryName() {
        if (isTopLevelClass())
            return null;
        String name = getSimpleBinaryName0();
        if (name == null) // anonymous class
            return "";
        return name;
    }

    private native String getSimpleBinaryName0();

    /**
     * Returns {@code true} if this is a top level class.  Returns {@code false}
     * otherwise.
     */
    private boolean isTopLevelClass() {
        return !isLocalOrAnonymousClass() && getDeclaringClass0() == null;
    }

    /**
     * Returns {@code true} if this is a local class or an anonymous
     * class.  Returns {@code false} otherwise.
     */
    private boolean isLocalOrAnonymousClass() {
        // JVM Spec 4.7.7: A class must have an EnclosingMethod
        // attribute if and only if it is a local class or an
        // anonymous class.
        return hasEnclosingMethodInfo();
    }

    private boolean hasEnclosingMethodInfo() {
        Object[] enclosingInfo = getEnclosingMethod0();
        if (enclosingInfo != null) {
            EnclosingMethodInfo.validate(enclosingInfo);
            return true;
        }
        return false;
    }

    /**
     * Returns an array containing {@code Class} objects representing all
     * the public classes and interfaces that are members of the class
     * represented by this {@code Class} object.  This includes public
     * class and interface members inherited from superclasses and public class
     * and interface members declared by the class.  This method returns an
     * array of length 0 if this {@code Class} object has no public member
     * classes or interfaces.  This method also returns an array of length 0 if
     * this {@code Class} object represents a primitive type, an array
     * class, or void.
     *
     * @return the array of {@code Class} objects representing the public
     *         members of this class
     */
    @CallerSensitive
    public Class<?>[] getClasses() {
        List<Class<?>> list = new ArrayList<>();
        Class<?> currentClass = Class.this;
        while (currentClass != null) {
            for (Class<?> m : currentClass.getDeclaredClasses()) {
                if (Modifier.isPublic(m.getModifiers())) {
                    list.add(m);
                }
            }
            currentClass = currentClass.getSuperclass();
        }
        return list.toArray(new Class<?>[0]);
    }

    /**
     * Returns an array containing {@code Field} objects reflecting all
     * the accessible public fields of the class or interface represented by
     * this {@code Class} object.
     *
     * If this {@code Class} object represents a class or interface with
     * no accessible public fields, then this method returns an array of length
     * 0.
     *
     * If this {@code Class} object represents a class, then this method
     * returns the public fields of the class and of all its superclasses and
     * superinterfaces.
     *
     * If this {@code Class} object represents an interface, then this
     * method returns the fields of the interface and of all its
     * superinterfaces.
     *
     * If this {@code Class} object represents an array type, a primitive
     * type, or void, then this method returns an array of length 0.
     *
     * The elements in the returned array are not sorted and are not in any
     * particular order.
     *
     * @return the array of {@code Field} objects representing the
     *         public fields
     */
    @CallerSensitive
    public Field[] getFields() throws SecurityException {
        return copyFields(privateGetPublicFields());
    }

    /**
     * Returns an array containing {@code Method} objects reflecting all the
     * public methods of the class or interface represented by this {@code
     * Class} object, including those declared by the class or interface and
     * those inherited from superclasses and superinterfaces.
     *
     * If this {@code Class} object represents an array type, then the
     * returned array has a {@code Method} object for each of the public
     * methods inherited by the array type from {@code Object}. It does not
     * contain a {@code Method} object for {@code clone()}.
     *
     * If this {@code Class} object represents an interface then the
     * returned array does not contain any implicitly declared methods from
     * {@code Object}. Therefore, if no methods are explicitly declared in
     * this interface or any of its superinterfaces then the returned array
     * has length 0. (Note that a {@code Class} object which represents a class
     * always has public methods, inherited from {@code Object}.)
     *
     * The returned array never contains methods with names "{@code <init>}"
     * or "{@code <clinit>}".
     *
     * The elements in the returned array are not sorted and are not in any
     * particular order.
     *
     * Generally, the result is computed as with the following 4 step algorithm.
     * Let C be the class or interface represented by this {@code Class} object:
     * <ol>
     * <li>A union of methods is composed of:
     *   <ol type="a">
     *   <li>C's declared public instance and static methods as returned by
     *        {@link #getDeclaredMethods()} and filtered to include only public
     *        methods.</li>
     *   <li>If C is a class other than {@code Object}, then include the result
     *        of invoking this algorithm recursively on the superclass of C.</li>
     *   <li>Include the results of invoking this algorithm recursively on all
     *        direct superinterfaces of C, but include only instance methods.</li>
     *   </ol></li>
     * <li>Union from step 1 is partitioned into subsets of methods with same
     *      signature (name, parameter types) and return type.</li>
     * <li>Within each such subset only the most specific methods are selected.
     *      Let method M be a method from a set of methods with same signature
     *      and return type. M is most specific if there is no such method
     *      N != M from the same set, such that N is more specific than M.
     *      N is more specific than M if:
     *   <ol type="a">
     *   <li>N is declared by a class and M is declared by an interface; or</li>
     *   <li>N and M are both declared by classes or both by interfaces and
     *        N's declaring type is the same as or a subtype of M's declaring type
     *        (clearly, if M's and N's declaring types are the same type, then
     *        M and N are the same method).</li>
     *   </ol></li>
     * <li>The result of this algorithm is the union of all selected methods from
     *      step 3.</li>
     * </ol>
     *
     * @apiNote There may be more than one method with a particular name
     * and parameter types in a class because while the Java language forbids a
     * class to declare multiple methods with the same signature but different
     * return types, the Java virtual machine does not.  This
     * increased flexibility in the virtual machine can be used to
     * implement various language features.  For example, covariant
     * returns can be implemented with {@linkplain
     * java.lang.reflect.Method#isBridge bridge methods}; the bridge
     * method and the overriding method would have the same
     * signature but different return types.
     *
     * @return the array of {@code Method} objects representing the
     *         public methods of this class
     */
    @CallerSensitive
    public Method[] getMethods() throws SecurityException {
        return copyMethods(privateGetPublicMethods());
    }

    /**
     * Returns an array containing {@code Constructor} objects reflecting
     * all the public constructors of the class represented by this
     * {@code Class} object.  An array of length 0 is returned if the
     * class has no public constructors, or if the class is an array class, or
     * if the class reflects a primitive type or void.
     *
     * Note that while this method returns an array of {@code
     * Constructor<T>} objects (that is an array of constructors from
     * this class), the return type of this method is {@code
     * Constructor<?>[]} and <em>not</em> {@code Constructor<T>[]} as
     * might be expected.  This less informative return type is
     * necessary since after being returned from this method, the
     * array could be modified to hold {@code Constructor} objects for
     * different classes, which would violate the type guarantees of
     * {@code Constructor<T>[]}.
     *
     * @return the array of {@code Constructor} objects representing the
     *         public constructors of this class
     */
    @CallerSensitive
    public Constructor<?>[] getConstructors() throws SecurityException {
        return copyConstructors(privateGetDeclaredConstructors(true));
    }

    /**
     * Returns a {@code Field} object that reflects the specified public member
     * field of the class or interface represented by this {@code Class}
     * object. The {@code name} parameter is a {@code String} specifying the
     * simple name of the desired field.
     *
     * The field to be reflected is determined by the algorithm that
     * follows.  Let C be the class or interface represented by this object:
     *
     * <ol>
     * <li>If C declares a public field with the name specified, that is the
     *      field to be reflected.</li>
     * <li>If no field was found in step 1 above, this algorithm is applied
     *      recursively to each direct superinterface of C. The direct
     *      superinterfaces are searched in the order they were declared.</li>
     * <li>If no field was found in steps 1 and 2 above, and C has a
     *      superclass S, then this algorithm is invoked recursively upon S.
     *      If C has no superclass, then a {@code NoSuchFieldException}
     *      is thrown.</li>
     * </ol>
     *
     * If this {@code Class} object represents an array type, then this
     * method does not find the {@code length} field of the array type.
     *
     * @param name the field name
     * @return the {@code Field} object of this class specified by
     *         {@code name}
     * @throws NoSuchFieldException if a field with the specified name is
     *         not found.
     * @throws NullPointerException if {@code name} is {@code null}
     */
    @CallerSensitive
    public Field getField(String name) throws NoSuchFieldException, SecurityException {
        Objects.requireNonNull(name);
        Field field = getField0(name);
        if (field == null) {
            throw new NoSuchFieldException(name);
        }
        return getReflectionFactory().copyField(field);
    }

    /**
     * Returns a {@code Method} object that reflects the specified public
     * member method of the class or interface represented by this
     * {@code Class} object. The {@code name} parameter is a
     * {@code String} specifying the simple name of the desired method. The
     * {@code parameterTypes} parameter is an array of {@code Class}
     * objects that identify the method's formal parameter types, in declared
     * order. If {@code parameterTypes} is {@code null}, it is
     * treated as if it were an empty array.
     *
     * If this {@code Class} object represents an array type, then this
     * method finds any public method inherited by the array type from
     * {@code Object} except method {@code clone()}.
     *
     * If this {@code Class} object represents an interface then this
     * method does not find any implicitly declared method from
     * {@code Object}. Therefore, if no methods are explicitly declared in
     * this interface or any of its superinterfaces, then this method does not
     * find any method.
     *
     * This method does not find any method with name "{@code <init>}" or
     * "{@code <clinit>}".
     *
     * Generally, the method to be reflected is determined by the 4 step
     * algorithm that follows.
     * Let C be the class or interface represented by this {@code Class} object:
     * <ol>
     * <li>A union of methods is composed of:
     *   <ol type="a">
     *   <li>C's declared public instance and static methods as returned by
     *        {@link #getDeclaredMethods()} and filtered to include only public
     *        methods that match given {@code name} and {@code parameterTypes}</li>
     *   <li>If C is a class other than {@code Object}, then include the result
     *        of invoking this algorithm recursively on the superclass of C.</li>
     *   <li>Include the results of invoking this algorithm recursively on all
     *        direct superinterfaces of C, but include only instance methods.</li>
     *   </ol></li>
     * <li>This union is partitioned into subsets of methods with same
     *      return type (the selection of methods from step 1 also guarantees that
     *      they have the same method name and parameter types).</li>
     * <li>Within each such subset only the most specific methods are selected.
     *      Let method M be a method from a set of methods with same VM
     *      signature (return type, name, parameter types).
     *      M is most specific if there is no such method N != M from the same
     *      set, such that N is more specific than M. N is more specific than M
     *      if:
     *   <ol type="a">
     *   <li>N is declared by a class and M is declared by an interface; or</li>
     *   <li>N and M are both declared by classes or both by interfaces and
     *        N's declaring type is the same as or a subtype of M's declaring type
     *        (clearly, if M's and N's declaring types are the same type, then
     *        M and N are the same method).</li>
     *   </ol></li>
     * <li>The result of this algorithm is chosen arbitrarily from the methods
     *      with most specific return type among all selected methods from step 3.
     *      Let R be a return type of a method M from the set of all selected methods
     *      from step 3. M is a method with most specific return type if there is
     *      no such method N != M from the same set, having return type S != R,
     *      such that S is a subtype of R as determined by
     *      R.class.{@link #isAssignableFrom}(S.class).
     * </ol>
     *
     * @apiNote There may be more than one method with matching name and
     * parameter types in a class because while the Java language forbids a
     * class to declare multiple methods with the same signature but different
     * return types, the Java virtual machine does not.  This
     * increased flexibility in the virtual machine can be used to
     * implement various language features.  For example, covariant
     * returns can be implemented with {@linkplain
     * java.lang.reflect.Method#isBridge bridge methods}; the bridge
     * method and the overriding method would have the same
     * signature but different return types. This method would return the
     * overriding method as it would have a more specific return type.
     *
     * @param name the name of the method
     * @param parameterTypes the list of parameters
     * @return the {@code Method} object that matches the specified
     *         {@code name} and {@code parameterTypes}
     * @throws NoSuchMethodException if a matching method is not found
     *         or if the name is "&lt;init&gt;"or "&lt;clinit&gt;".
     * @throws NullPointerException if {@code name} is {@code null}
     */
    @CallerSensitive
    public Method getMethod(String name, Class<?>... parameterTypes) throws NoSuchMethodException, SecurityException {
        Objects.requireNonNull(name);
        Method method = getMethod0(name, parameterTypes);
        if (method == null) {
            throw new NoSuchMethodException(methodToString(name, parameterTypes));
        }
        return getReflectionFactory().copyMethod(method);
    }

    /**
     * Returns a {@code Constructor} object that reflects the specified
     * public constructor of the class represented by this {@code Class}
     * object. The {@code parameterTypes} parameter is an array of
     * {@code Class} objects that identify the constructor's formal
     * parameter types, in declared order.
     *
     * If this {@code Class} object represents an inner class
     * declared in a non-static context, the formal parameter types
     * include the explicit enclosing instance as the first parameter.
     *
     * The constructor to reflect is the public constructor of the class
     * represented by this {@code Class} object whose formal parameter
     * types match those specified by {@code parameterTypes}.
     *
     * @param parameterTypes the parameter array
     * @return the {@code Constructor} object of the public constructor that
     *         matches the specified {@code parameterTypes}
     * @throws NoSuchMethodException if a matching method is not found.
     */
    @CallerSensitive
    public Constructor<T> getConstructor(Class<?>... parameterTypes) throws NoSuchMethodException, SecurityException {
        return getReflectionFactory().copyConstructor(getConstructor0(parameterTypes, Member.PUBLIC));
    }

    /**
     * Returns an array of {@code Class} objects reflecting all the
     * classes and interfaces declared as members of the class represented by
     * this {@code Class} object. This includes public, protected, default
     * (package) access, and private classes and interfaces declared by the
     * class, but excludes inherited classes and interfaces.  This method
     * returns an array of length 0 if the class declares no classes or
     * interfaces as members, or if this {@code Class} object represents a
     * primitive type, an array class, or void.
     *
     * @return the array of {@code Class} objects representing all the
     *         declared members of this class
     */
    @CallerSensitive
    public Class<?>[] getDeclaredClasses() throws SecurityException {
        return getDeclaredClasses0();
    }

    /**
     * Returns an array of {@code Field} objects reflecting all the fields
     * declared by the class or interface represented by this
     * {@code Class} object. This includes public, protected, default
     * (package) access, and private fields, but excludes inherited fields.
     *
     * If this {@code Class} object represents a class or interface with no
     * declared fields, then this method returns an array of length 0.
     *
     * If this {@code Class} object represents an array type, a primitive
     * type, or void, then this method returns an array of length 0.
     *
     * The elements in the returned array are not sorted and are not in any
     * particular order.
     *
     * @return the array of {@code Field} objects representing all the
     *          declared fields of this class
     */
    @CallerSensitive
    public Field[] getDeclaredFields() throws SecurityException {
        return copyFields(privateGetDeclaredFields(false));
    }

    /**
     * Returns an array containing {@code Method} objects reflecting all the
     * declared methods of the class or interface represented by this {@code
     * Class} object, including public, protected, default (package)
     * access, and private methods, but excluding inherited methods.
     *
     * If this {@code Class} object represents a type that has multiple
     * declared methods with the same name and parameter types, but different
     * return types, then the returned array has a {@code Method} object for
     * each such method.
     *
     * If this {@code Class} object represents a type that has a class
     * initialization method {@code <clinit>}, then the returned array does
     * <em>not</em> have a corresponding {@code Method} object.
     *
     * If this {@code Class} object represents a class or interface with no
     * declared methods, then the returned array has length 0.
     *
     * If this {@code Class} object represents an array type, a primitive
     * type, or void, then the returned array has length 0.
     *
     * The elements in the returned array are not sorted and are not in any
     * particular order.
     *
     * @return the array of {@code Method} objects representing all the
     *          declared methods of this class
     */
    @CallerSensitive
    public Method[] getDeclaredMethods() throws SecurityException {
        return copyMethods(privateGetDeclaredMethods(false));
    }

    /**
     * Returns an array of {@code Constructor} objects reflecting all the
     * constructors declared by the class represented by this
     * {@code Class} object. These are public, protected, default
     * (package) access, and private constructors.  The elements in the array
     * returned are not sorted and are not in any particular order.  If the
     * class has a default constructor, it is included in the returned array.
     * This method returns an array of length 0 if this {@code Class}
     * object represents an interface, a primitive type, an array class, or
     * void.
     *
     * See <em>The Java Language Specification</em>, section 8.2.
     *
     * @return the array of {@code Constructor} objects representing all the
     *          declared constructors of this class
     */
    @CallerSensitive
    public Constructor<?>[] getDeclaredConstructors() throws SecurityException {
        return copyConstructors(privateGetDeclaredConstructors(false));
    }

    /**
     * Returns a {@code Field} object that reflects the specified declared
     * field of the class or interface represented by this {@code Class}
     * object. The {@code name} parameter is a {@code String} that specifies
     * the simple name of the desired field.
     *
     * If this {@code Class} object represents an array type, then this
     * method does not find the {@code length} field of the array type.
     *
     * @param name the name of the field
     * @return the {@code Field} object for the specified field in this
     *          class
     * @throws NoSuchFieldException if a field with the specified name is
     *          not found.
     * @throws NullPointerException if {@code name} is {@code null}
     */
    @CallerSensitive
    public Field getDeclaredField(String name) throws NoSuchFieldException, SecurityException {
        Objects.requireNonNull(name);
        Field field = searchFields(privateGetDeclaredFields(false), name);
        if (field == null) {
            throw new NoSuchFieldException(name);
        }
        return getReflectionFactory().copyField(field);
    }

    /**
     * Returns a {@code Method} object that reflects the specified
     * declared method of the class or interface represented by this
     * {@code Class} object. The {@code name} parameter is a
     * {@code String} that specifies the simple name of the desired
     * method, and the {@code parameterTypes} parameter is an array of
     * {@code Class} objects that identify the method's formal parameter
     * types, in declared order.  If more than one method with the same
     * parameter types is declared in a class, and one of these methods has a
     * return type that is more specific than any of the others, that method is
     * returned; otherwise one of the methods is chosen arbitrarily.  If the
     * name is "&lt;init&gt;"or "&lt;clinit&gt;" a {@code NoSuchMethodException}
     * is raised.
     *
     * If this {@code Class} object represents an array type, then this
     * method does not find the {@code clone()} method.
     *
     * @param name the name of the method
     * @param parameterTypes the parameter array
     * @return the {@code Method} object for the method of this class
     *          matching the specified name and parameters
     * @throws NoSuchMethodException if a matching method is not found.
     * @throws NullPointerException if {@code name} is {@code null}
     */
    @CallerSensitive
    public Method getDeclaredMethod(String name, Class<?>... parameterTypes) throws NoSuchMethodException, SecurityException {
        Objects.requireNonNull(name);
        Method method = searchMethods(privateGetDeclaredMethods(false), name, parameterTypes);
        if (method == null) {
            throw new NoSuchMethodException(methodToString(name, parameterTypes));
        }
        return getReflectionFactory().copyMethod(method);
    }

    /**
     * Returns the list of {@code Method} objects for the declared public
     * methods of this class or interface that have the specified method name
     * and parameter types.
     *
     * @param name the name of the method
     * @param parameterTypes the parameter array
     * @return the list of {@code Method} objects for the public methods of
     *         this class matching the specified name and parameters
     */
    /* oops! */public List<Method> getDeclaredPublicMethods(String name, Class<?>... parameterTypes) {
        Method[] methods = privateGetDeclaredMethods(/* publicOnly */ true);
        ReflectionFactory factory = getReflectionFactory();
        List<Method> result = new ArrayList<>();
        for (Method method : methods) {
            if (method.getName().equals(name) && Arrays.equals(factory.getExecutableSharedParameterTypes(method), parameterTypes)) {
                result.add(factory.copyMethod(method));
            }
        }
        return result;
    }

    /**
     * Returns a {@code Constructor} object that reflects the specified
     * constructor of the class or interface represented by this
     * {@code Class} object.  The {@code parameterTypes} parameter is
     * an array of {@code Class} objects that identify the constructor's
     * formal parameter types, in declared order.
     *
     * If this {@code Class} object represents an inner class
     * declared in a non-static context, the formal parameter types
     * include the explicit enclosing instance as the first parameter.
     *
     * @param parameterTypes the parameter array
     * @return The {@code Constructor} object for the constructor with the
     *          specified parameter list
     * @throws NoSuchMethodException if a matching method is not found.
     */
    @CallerSensitive
    public Constructor<T> getDeclaredConstructor(Class<?>... parameterTypes) throws NoSuchMethodException, SecurityException {
        return getReflectionFactory().copyConstructor(getConstructor0(parameterTypes, Member.DECLARED));
    }

    /*
     * Return the Virtual Machine's Class object for the named
     * primitive type.
     */
    static native Class<?> getPrimitiveClass(String name);

    /**
     * Add a package name prefix if the name is not absolute Remove leading "/"
     * if name is absolute
     */
    private String resolveName(String name) {
        if (!name.startsWith("/")) {
            Class<?> c = this;
            while (c.isArray()) {
                c = c.getComponentType();
            }
            String baseName = c.getPackageName();
            if (baseName != null && !baseName.isEmpty()) {
                name = baseName.replace('.', '/') + "/" + name;
            }
        } else {
            name = name.substring(1);
        }
        return name;
    }

    /**
     * Atomic operations support.
     */
    private static class Atomic {
        // initialize Unsafe machinery here, since we need to call Class.class instance method
        // and have to avoid calling it in the static initializer of the Class class...
        private static final Unsafe unsafe = Unsafe.getUnsafe();
        // offset of Class.reflectionData instance field
        private static final long reflectionDataOffset = unsafe.objectFieldOffset(Class.class, "reflectionData");

        static <T> boolean casReflectionData(Class<?> clazz, SoftReference<ReflectionData<T>> oldData, SoftReference<ReflectionData<T>> newData) {
            return unsafe.compareAndSetObject(clazz, reflectionDataOffset, oldData, newData);
        }
    }

    /**
     * Reflection support.
     */
    // Reflection data caches various derived names and reflective members. Cached
    // values may be invalidated when JVM TI RedefineClasses() is called
    private static class ReflectionData<T> {
        volatile Field[] declaredFields;
        volatile Field[] publicFields;
        volatile Method[] declaredMethods;
        volatile Method[] publicMethods;
        volatile Constructor<T>[] declaredConstructors;
        volatile Constructor<T>[] publicConstructors;
        // Intermediate results for getFields and getMethods
        volatile Field[] declaredPublicFields;
        volatile Method[] declaredPublicMethods;
        volatile Class<?>[] interfaces;

        // Cached names
        String simpleName;
        String canonicalName;
        static final String NULL_SENTINEL = new String();

        // Value of classRedefinedCount when we created this ReflectionData instance
        final int redefinedCount;

        ReflectionData(int redefinedCount) {
            this.redefinedCount = redefinedCount;
        }
    }

    private transient volatile SoftReference<ReflectionData<T>> reflectionData;

    // Incremented by the VM on each call to JVM TI RedefineClasses()
    // that redefines this class or a superclass.
    private transient volatile int classRedefinedCount;

    // Lazily create and cache ReflectionData
    private ReflectionData<T> reflectionData() {
        SoftReference<ReflectionData<T>> reflectionData = this.reflectionData;
        int classRedefinedCount = this.classRedefinedCount;
        ReflectionData<T> rd;
        if (reflectionData != null && (rd = reflectionData.get()) != null && rd.redefinedCount == classRedefinedCount) {
            return rd;
        }
        // else no SoftReference or cleared SoftReference or stale ReflectionData
        // -> create and replace new instance
        return newReflectionData(reflectionData, classRedefinedCount);
    }

    private ReflectionData<T> newReflectionData(SoftReference<ReflectionData<T>> oldReflectionData, int classRedefinedCount) {
        while (true) {
            ReflectionData<T> rd = new ReflectionData<>(classRedefinedCount);
            // try to CAS it...
            if (Atomic.casReflectionData(this, oldReflectionData, new SoftReference<>(rd))) {
                return rd;
            }
            // else retry
            oldReflectionData = this.reflectionData;
            classRedefinedCount = this.classRedefinedCount;
            if (oldReflectionData != null && (rd = oldReflectionData.get()) != null && rd.redefinedCount == classRedefinedCount) {
                return rd;
            }
        }
    }

    // Generic signature handling
    private native String getGenericSignature0();

    // Generic info repository; lazily initialized
    private transient volatile ClassRepository genericInfo;

    // accessor for factory
    private GenericsFactory getFactory() {
        // create scope and factory
        return CoreReflectionFactory.make(this, ClassScope.make(this));
    }

    // accessor for generic info repository;
    // generic info is lazily initialized
    private ClassRepository getGenericInfo() {
        ClassRepository genericInfo = this.genericInfo;
        if (genericInfo == null) {
            String signature = getGenericSignature0();
            if (signature == null) {
                genericInfo = ClassRepository.NONE;
            } else {
                genericInfo = ClassRepository.make(signature, getFactory());
            }
            this.genericInfo = genericInfo;
        }
        return (genericInfo != ClassRepository.NONE) ? genericInfo : null;
    }

    /* oops! */public native ConstantPool getConstantPool();

    // java.lang.reflect.Field handling

    // Returns an array of "root" fields. These Field objects must NOT
    // be propagated to the outside world, but must instead be copied
    // via ReflectionFactory.copyField.
    private Field[] privateGetDeclaredFields(boolean publicOnly) {
        Field[] res;
        ReflectionData<T> rd = reflectionData();
        if (rd != null) {
            res = publicOnly ? rd.declaredPublicFields : rd.declaredFields;
            if (res != null)
                return res;
        }
        // No cached value available; request value from VM
        res = Reflection.filterFields(this, getDeclaredFields0(publicOnly));
        if (rd != null) {
            if (publicOnly) {
                rd.declaredPublicFields = res;
            } else {
                rd.declaredFields = res;
            }
        }
        return res;
    }

    // Returns an array of "root" fields. These Field objects must NOT
    // be propagated to the outside world, but must instead be copied
    // via ReflectionFactory.copyField.
    private Field[] privateGetPublicFields() {
        Field[] res;
        ReflectionData<T> rd = reflectionData();
        if (rd != null) {
            res = rd.publicFields;
            if (res != null)
                return res;
        }

        // Use a linked hash set to ensure order is preserved and
        // fields from common super interfaces are not duplicated
        LinkedHashSet<Field> fields = new LinkedHashSet<>();

        // Local fields
        addAll(fields, privateGetDeclaredFields(true));

        // Direct superinterfaces, recursively
        for (Class<?> si : getInterfaces()) {
            addAll(fields, si.privateGetPublicFields());
        }

        // Direct superclass, recursively
        Class<?> sc = getSuperclass();
        if (sc != null) {
            addAll(fields, sc.privateGetPublicFields());
        }

        res = fields.toArray(new Field[0]);
        if (rd != null) {
            rd.publicFields = res;
        }
        return res;
    }

    private static void addAll(Collection<Field> c, Field[] o) {
        for (Field f : o) {
            c.add(f);
        }
    }

    // java.lang.reflect.Constructor handling

    // Returns an array of "root" constructors. These Constructor
    // objects must NOT be propagated to the outside world, but must
    // instead be copied via ReflectionFactory.copyConstructor.
    private Constructor<T>[] privateGetDeclaredConstructors(boolean publicOnly) {
        Constructor<T>[] res;
        ReflectionData<T> rd = reflectionData();
        if (rd != null) {
            res = publicOnly ? rd.publicConstructors : rd.declaredConstructors;
            if (res != null)
                return res;
        }
        // No cached value available; request value from VM
        if (isInterface()) {
            @SuppressWarnings("unchecked")
            Constructor<T>[] temporaryRes = (Constructor<T>[]) new Constructor<?>[0];
            res = temporaryRes;
        } else {
            res = getDeclaredConstructors0(publicOnly);
        }
        if (rd != null) {
            if (publicOnly) {
                rd.publicConstructors = res;
            } else {
                rd.declaredConstructors = res;
            }
        }
        return res;
    }

    // java.lang.reflect.Method handling

    // Returns an array of "root" methods. These Method objects must NOT
    // be propagated to the outside world, but must instead be copied
    // via ReflectionFactory.copyMethod.
    private Method[] privateGetDeclaredMethods(boolean publicOnly) {
        Method[] res;
        ReflectionData<T> rd = reflectionData();
        if (rd != null) {
            res = publicOnly ? rd.declaredPublicMethods : rd.declaredMethods;
            if (res != null)
                return res;
        }
        // No cached value available; request value from VM
        res = Reflection.filterMethods(this, getDeclaredMethods0(publicOnly));
        if (rd != null) {
            if (publicOnly) {
                rd.declaredPublicMethods = res;
            } else {
                rd.declaredMethods = res;
            }
        }
        return res;
    }

    // Returns an array of "root" methods. These Method objects must NOT
    // be propagated to the outside world, but must instead be copied
    // via ReflectionFactory.copyMethod.
    private Method[] privateGetPublicMethods() {
        Method[] res;
        ReflectionData<T> rd = reflectionData();
        if (rd != null) {
            res = rd.publicMethods;
            if (res != null)
                return res;
        }

        // No cached value available; compute value recursively.
        // Start by fetching public declared methods...
        PublicMethods pms = new PublicMethods();
        for (Method m : privateGetDeclaredMethods(/* publicOnly */ true)) {
            pms.merge(m);
        }
        // ...then recur over superclass methods...
        Class<?> sc = getSuperclass();
        if (sc != null) {
            for (Method m : sc.privateGetPublicMethods()) {
                pms.merge(m);
            }
        }
        // ...and finally over direct superinterfaces.
        for (Class<?> intf : getInterfaces(/* cloneArray */ false)) {
            for (Method m : intf.privateGetPublicMethods()) {
                // static interface methods are not inherited
                if (!Modifier.isStatic(m.getModifiers())) {
                    pms.merge(m);
                }
            }
        }

        res = pms.toArray();
        if (rd != null) {
            rd.publicMethods = res;
        }
        return res;
    }

    // Helpers for fetchers of one field, method, or constructor

    // This method does not copy the returned Field object!
    private static Field searchFields(Field[] fields, String name) {
        for (Field field : fields) {
            if (field.getName().equals(name)) {
                return field;
            }
        }
        return null;
    }

    // Returns a "root" Field object. This Field object must NOT
    // be propagated to the outside world, but must instead be copied
    // via ReflectionFactory.copyField.
    private Field getField0(String name) {
        // Note: the intent is that the search algorithm this routine
        // uses be equivalent to the ordering imposed by
        // privateGetPublicFields(). It fetches only the declared
        // public fields for each class, however, to reduce the number
        // of Field objects which have to be created for the common
        // case where the field being requested is declared in the
        // class which is being queried.
        Field res;
        // Search declared public fields
        if ((res = searchFields(privateGetDeclaredFields(true), name)) != null) {
            return res;
        }
        // Direct superinterfaces, recursively
        Class<?>[] interfaces = getInterfaces(/* cloneArray */ false);
        for (Class<?> c : interfaces) {
            if ((res = c.getField0(name)) != null) {
                return res;
            }
        }
        // Direct superclass, recursively
        if (!isInterface()) {
            Class<?> c = getSuperclass();
            if (c != null) {
                if ((res = c.getField0(name)) != null) {
                    return res;
                }
            }
        }
        return null;
    }

    // This method does not copy the returned Method object!
    private static Method searchMethods(Method[] methods, String name, Class<?>[] parameterTypes) {
        ReflectionFactory fact = getReflectionFactory();
        Method res = null;
        for (Method m : methods) {
            if (m.getName().equals(name)
                && arrayContentsEq(parameterTypes, fact.getExecutableSharedParameterTypes(m))
                && (res == null || (res.getReturnType() != m.getReturnType() && res.getReturnType().isAssignableFrom(m.getReturnType()))))
                res = m;
        }
        return res;
    }

    private static final Class<?>[] EMPTY_CLASS_ARRAY = new Class<?>[0];

    // Returns a "root" Method object. This Method object must NOT
    // be propagated to the outside world, but must instead be copied
    // via ReflectionFactory.copyMethod.
    private Method getMethod0(String name, Class<?>[] parameterTypes) {
        PublicMethods.MethodList res = getMethodsRecursive(name, parameterTypes == null ? EMPTY_CLASS_ARRAY : parameterTypes, /* includeStatic */ true);
        return res == null ? null : res.getMostSpecific();
    }

    // Returns a list of "root" Method objects. These Method objects must NOT
    // be propagated to the outside world, but must instead be copied
    // via ReflectionFactory.copyMethod.
    private PublicMethods.MethodList getMethodsRecursive(String name, Class<?>[] parameterTypes, boolean includeStatic) {
        // 1st check declared public methods
        Method[] methods = privateGetDeclaredMethods(/* publicOnly */ true);
        PublicMethods.MethodList res = PublicMethods.MethodList.filter(methods, name, parameterTypes, includeStatic);
        // if there is at least one match among declared methods, we need not
        // search any further as such match surely overrides matching methods
        // declared in superclass(es) or interface(s).
        if (res != null) {
            return res;
        }

        // if there was no match among declared methods,
        // we must consult the superclass (if any) recursively...
        Class<?> sc = getSuperclass();
        if (sc != null) {
            res = sc.getMethodsRecursive(name, parameterTypes, includeStatic);
        }

        // ...and coalesce the superclass methods with methods obtained
        // from directly implemented interfaces excluding static methods...
        for (Class<?> intf : getInterfaces(/* cloneArray */ false)) {
            res = PublicMethods.MethodList.merge(res, intf.getMethodsRecursive(name, parameterTypes, /* includeStatic */ false));
        }

        return res;
    }

    // Returns a "root" Constructor object. This Constructor object must NOT
    // be propagated to the outside world, but must instead be copied
    // via ReflectionFactory.copyConstructor.
    private Constructor<T> getConstructor0(Class<?>[] parameterTypes, int which) throws NoSuchMethodException {
        ReflectionFactory fact = getReflectionFactory();
        Constructor<T>[] constructors = privateGetDeclaredConstructors((which == Member.PUBLIC));
        for (Constructor<T> constructor : constructors) {
            if (arrayContentsEq(parameterTypes, fact.getExecutableSharedParameterTypes(constructor))) {
                return constructor;
            }
        }
        throw new NoSuchMethodException(methodToString("<init>", parameterTypes));
    }

    // Other helpers and base implementation

    private static boolean arrayContentsEq(Object[] a1, Object[] a2) {
        if (a1 == null) {
            return a2 == null || a2.length == 0;
        }

        if (a2 == null) {
            return a1.length == 0;
        }

        if (a1.length != a2.length) {
            return false;
        }

        for (int i = 0; i < a1.length; i++) {
            if (a1[i] != a2[i]) {
                return false;
            }
        }

        return true;
    }

    private static Field[] copyFields(Field[] arg) {
        Field[] out = new Field[arg.length];
        ReflectionFactory fact = getReflectionFactory();
        for (int i = 0; i < arg.length; i++) {
            out[i] = fact.copyField(arg[i]);
        }
        return out;
    }

    private static Method[] copyMethods(Method[] arg) {
        Method[] out = new Method[arg.length];
        ReflectionFactory fact = getReflectionFactory();
        for (int i = 0; i < arg.length; i++) {
            out[i] = fact.copyMethod(arg[i]);
        }
        return out;
    }

    private static <U> Constructor<U>[] copyConstructors(Constructor<U>[] arg) {
        Constructor<U>[] out = arg.clone();
        ReflectionFactory fact = getReflectionFactory();
        for (int i = 0; i < out.length; i++) {
            out[i] = fact.copyConstructor(out[i]);
        }
        return out;
    }

    private native Field[]       getDeclaredFields0(boolean publicOnly);
    private native Method[]      getDeclaredMethods0(boolean publicOnly);
    private native Constructor<T>[] getDeclaredConstructors0(boolean publicOnly);
    private native Class<?>[]   getDeclaredClasses0();

    /**
     * Helper method to get the method name from arguments.
     */
    private String methodToString(String name, Class<?>[] argTypes) {
        StringJoiner sj = new StringJoiner(", ", getName() + "." + name + "(", ")");
        if (argTypes != null) {
            for (int i = 0; i < argTypes.length; i++) {
                Class<?> c = argTypes[i];
                sj.add((c == null) ? "null" : c.getName());
            }
        }
        return sj.toString();
    }

    /**
     * Returns true if and only if this class was declared as an enum in the
     * source code.
     *
     * @return true if and only if this class was declared as an enum in the
     *     source code
     */
    public boolean isEnum() {
        // An enum must both directly extend java.lang.Enum and have
        // the ENUM bit set; classes for specialized enum constants
        // don't do the former.
        return (this.getModifiers() & ENUM) != 0 && this.getSuperclass() == java.lang.Enum.class;
    }

    // Fetches the factory for reflective objects
    private static ReflectionFactory getReflectionFactory() {
        if (reflectionFactory == null) {
            reflectionFactory = ReflectionFactory.getReflectionFactory();
        }
        return reflectionFactory;
    }
    private static ReflectionFactory reflectionFactory;

    /**
     * Returns the elements of this enum class or null if this
     * Class object does not represent an enum type.
     *
     * @return an array containing the values comprising the enum class
     *     represented by this Class object in the order they're
     *     declared, or null if this Class object does not
     *     represent an enum type
     */
    public T[] getEnumConstants() {
        T[] values = getEnumConstantsShared();
        return (values != null) ? values.clone() : null;
    }

    /**
     * Returns the elements of this enum class or null if this
     * Class object does not represent an enum type;
     * identical to getEnumConstants except that the result is
     * uncloned, cached, and shared by all callers.
     */
    /* oops! */public T[] getEnumConstantsShared() {
        T[] constants = enumConstants;
        if (constants == null) {
            if (!isEnum())
                return null;
            try {
                final Method values = getMethod("values");
                values.setAccessible(true);
                @SuppressWarnings("unchecked")
                T[] temporaryConstants = (T[])values.invoke(null);
                enumConstants = constants = temporaryConstants;
            }
            // These can happen when users concoct enum-like classes
            // that don't comply with the enum spec.
            catch (InvocationTargetException | NoSuchMethodException | IllegalAccessException ex) { return null; }
        }
        return constants;
    }
    private transient volatile T[] enumConstants;

    /**
     * Returns a map from simple name to enum constant.  This package-private
     * method is used internally by Enum to implement
     * {@code public static <T extends Enum<T>> T valueOf(Class<T>, String)}
     * efficiently.  Note that the map is returned by this method is
     * created lazily on first use.  Typically it won't ever get created.
     */
    Map<String, T> enumConstantDirectory() {
        Map<String, T> directory = enumConstantDirectory;
        if (directory == null) {
            T[] universe = getEnumConstantsShared();
            if (universe == null)
                throw new IllegalArgumentException(getName() + " is not an enum type");
            directory = new HashMap<>((int)(universe.length / 0.75f) + 1);
            for (T constant : universe) {
                directory.put(((Enum<?>)constant).name(), constant);
            }
            enumConstantDirectory = directory;
        }
        return directory;
    }
    private transient volatile Map<String, T> enumConstantDirectory;

    /**
     * Casts an object to the class or interface represented
     * by this {@code Class} object.
     *
     * @param obj the object to be cast
     * @return the object after casting, or null if obj is null
     *
     * @throws ClassCastException if the object is not
     * null and is not assignable to the type T.
     */
    @SuppressWarnings("unchecked")
    // @HotSpotIntrinsicCandidate
    public T cast(Object obj) {
        if (obj != null && !isInstance(obj))
            throw new ClassCastException(cannotCastMsg(obj));
        return (T) obj;
    }

    private String cannotCastMsg(Object obj) {
        return "Cannot cast " + obj.getClass().getName() + " to " + getName();
    }

    /**
     * Casts this {@code Class} object to represent a subclass of the class
     * represented by the specified class object.  Checks that the cast
     * is valid, and throws a {@code ClassCastException} if it is not.  If
     * this method succeeds, it always returns a reference to this class object.
     *
     * This method is useful when a client needs to "narrow" the type of
     * a {@code Class} object to pass it to an API that restricts the
     * {@code Class} objects that it is willing to accept.  A cast would
     * generate a compile-time warning, as the correctness of the cast
     * could not be checked at runtime (because generic types are implemented
     * by erasure).
     *
     * @param <U> the type to cast this class object to
     * @param clazz the class of the type to cast this class object to
     * @return this {@code Class} object, cast to represent a subclass of
     *    the specified class object.
     * @throws ClassCastException if this {@code Class} object does not
     *    represent a subclass of the specified class (here "subclass" includes
     *    the class itself).
     */
    @SuppressWarnings("unchecked")
    public <U> Class<? extends U> asSubclass(Class<U> clazz) {
        if (clazz.isAssignableFrom(this))
            return (Class<? extends U>) this;
        else
            throw new ClassCastException(this.toString());
    }

    /* Backing store of user-defined values pertaining to this class.
     * Maintained by the ClassValue class.
     */
    transient ClassValue.ClassValueMap classValueMap;

    private native Class<?> getNestHost0();

    /**
     * Returns the nest host of the <a href=#nest>nest</a> to which the class
     * or interface represented by this {@code Class} object belongs.
     * Every class and interface is a member of exactly one nest.
     * A class or interface that is not recorded as belonging to a nest
     * belongs to the nest consisting only of itself, and is the nest
     * host.
     *
     * Each of the {@code Class} objects representing array types,
     * primitive types, and {@code void} returns {@code this} to indicate
     * that the represented entity belongs to the nest consisting only of
     * itself, and is the nest host.
     *
     * If there is a {@linkplain LinkageError linkage error} accessing
     * the nest host, or if this class or interface is not enumerated as
     * a member of the nest by the nest host, then it is considered to belong
     * to its own nest and {@code this} is returned as the host.
     *
     * @apiNote A {@code class} file of version 55.0 or greater may record the
     * host of the nest to which it belongs by using the {@code NestHost}
     * attribute (JVMS 4.7.28). Alternatively, a {@code class} file of
     * version 55.0 or greater may act as a nest host by enumerating the nest's
     * other members with the
     * {@code NestMembers} attribute (JVMS 4.7.29).
     * A {@code class} file of version 54.0 or lower does not use these
     * attributes.
     *
     * @return the nest host of this class or interface
     */
    @CallerSensitive
    public Class<?> getNestHost() {
        if (isPrimitive() || isArray()) {
            return this;
        }
        Class<?> host;
        try {
            host = getNestHost0();
        } catch (LinkageError e) {
            // if we couldn't load our nest-host then we
            // act as-if we have no nest-host attribute
            return this;
        }
        // if null then nest membership validation failed, so we
        // act as-if we have no nest-host attribute
        if (host == null || host == this) {
            return this;
        }
        return host;
    }

    /**
     * Determines if the given {@code Class} is a nestmate of the
     * class or interface represented by this {@code Class} object.
     * Two classes or interfaces are nestmates
     * if they have the same {@linkplain #getNestHost() nest host}.
     *
     * @param c the class to check
     * @return {@code true} if this class and {@code c} are members of
     * the same nest; and {@code false} otherwise.
     */
    public boolean isNestmateOf(Class<?> c) {
        if (this == c) {
            return true;
        }
        if (isPrimitive() || isArray() || c.isPrimitive() || c.isArray()) {
            return false;
        }
        try {
            return getNestHost0() == c.getNestHost0();
        } catch (LinkageError e) {
            return false;
        }
    }

    private native Class<?>[] getNestMembers0();

    /**
     * Returns an array containing {@code Class} objects representing all the
     * classes and interfaces that are members of the nest to which the class
     * or interface represented by this {@code Class} object belongs.
     * The {@linkplain #getNestHost() nest host} of that nest is the zeroth
     * element of the array. Subsequent elements represent any classes or
     * interfaces that are recorded by the nest host as being members of
     * the nest; the order of such elements is unspecified. Duplicates are
     * permitted.
     * If the nest host of that nest does not enumerate any members, then the
     * array has a single element containing {@code this}.
     *
     * Each of the {@code Class} objects representing array types,
     * primitive types, and {@code void} returns an array containing only
     * {@code this}.
     *
     * This method validates that, for each class or interface which is
     * recorded as a member of the nest by the nest host, that class or
     * interface records itself as a member of that same nest. Any exceptions
     * that occur during this validation are rethrown by this method.
     *
     * @return an array of all classes and interfaces in the same nest as
     * this class
     *
     * @throws LinkageError
     *         If there is any problem loading or validating a nest member or
     *         its nest host
     */
    @CallerSensitive
    public Class<?>[] getNestMembers() {
        if (isPrimitive() || isArray()) {
            return new Class<?>[] { this };
        }
        Class<?>[] members = getNestMembers0();
        // Can't actually enable this due to bootstrapping issues
        // assert(members.length != 1 || members[0] == this); // expected invariant from VM

        return members;
    }
}

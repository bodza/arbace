package java.lang.reflect;

import java.lang.annotation.Annotation;
import java.lang.invoke.MethodHandle;

import jdk.internal.misc.VM;
import jdk.internal.reflect.CallerSensitive;
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
 *
 * Whether the checks for Java language access control can be suppressed
 * (and thus, whether access can be enabled) depends on whether the reflected
 * object corresponds to a member in an exported or open package
 * (see {@link #setAccessible(boolean)}).
 */
public class AccessibleObject implements AnnotatedElement {
    /**
     * Convenience method to set the {@code accessible} flag for an
     * array of reflected objects with a single security check (for efficiency).
     *
     * This method may be used to enable access to all reflected objects in
     * the array when access to each reflected object can be enabled as
     * specified by {@link #setAccessible(boolean) setAccessible(boolean)}.
     *
     * A {@code SecurityException} is also thrown if any of the elements of
     * the input {@code array} is a {@link java.lang.reflect.Constructor}
     * object for the class {@code java.lang.Class} and {@code flag} is true.
     *
     * @param array the array of AccessibleObjects
     * @param flag  the new value for the {@code accessible} flag
     *              in each object
     * @throws InaccessibleObjectException if access cannot be enabled for all
     *         objects in the array
     */
    @CallerSensitive
    public static void setAccessible(AccessibleObject[] array, boolean flag) {
        if (flag) {
            Class<?> caller = Reflection.getCallerClass();
            array = array.clone();
            for (AccessibleObject ao : array) {
                ao.checkCanSetAccessible(caller);
            }
        }
        for (AccessibleObject ao : array) {
            ao.setAccessible0(flag);
        }
    }

    /**
     * Set the {@code accessible} flag for this reflected object to
     * the indicated boolean value.  A value of {@code true} indicates that
     * the reflected object should suppress checks for Java language access
     * control when it is used. A value of {@code false} indicates that
     * the reflected object should enforce checks for Java language access
     * control when it is used, with the variation noted in the class description.
     *
     * This method may be used by a caller in class {@code C} to enable
     * access to a {@link Member member} of {@link Member#getDeclaringClass()
     * declaring class} {@code D} if any of the following hold:
     *
     * <ul>
     *     <li>{@code C} and {@code D} are in the same module.</li>
     *
     *     <li>The member is {@code public} and {@code D} is {@code public} in
     *     a package that the module containing {@code D} {@link
     *     Module#isExported(String,Module) exports} to at least the module
     *     containing {@code C}.</li>
     *
     *     <li>The member is {@code protected} {@code static}, {@code D} is
     *     {@code public} in a package that the module containing {@code D}
     *     exports to at least the module containing {@code C}, and {@code C}
     *     is a subclass of {@code D}.</li>
     *
     *     <li>{@code D} is in a package that the module containing {@code D}
     *     {@link Module#isOpen(String,Module) opens} to at least the module
     *     containing {@code C}.
     *     All packages in unnamed and open modules are open to all modules and
     *     so this method always succeeds when {@code D} is in an unnamed or
     *     open module.</li>
     * </ul>
     *
     * This method cannot be used to enable access to private members,
     * members with default (package) access, protected instance members, or
     * protected constructors when the declaring class is in a different module
     * to the caller and the package containing the declaring class is not open
     * to the caller's module.
     *
     * @param flag the new value for the {@code accessible} flag
     * @throws InaccessibleObjectException if access cannot be enabled
     */
    @CallerSensitive   // overrides in Method/Field/Constructor are @CS
    public void setAccessible(boolean flag) {
        setAccessible0(flag);
    }

    /**
     * Sets the accessible flag and returns the new value
     */
    boolean setAccessible0(boolean flag) {
        this.override = flag;
        return flag;
    }

    /**
     * Set the {@code accessible} flag for this reflected object to {@code true}
     * if possible. This method sets the {@code accessible} flag, as if by
     * invoking {@link #setAccessible(boolean) setAccessible(true)}, and returns
     * the possibly-updated value for the {@code accessible} flag. If access
     * cannot be enabled, i.e. the checks or Java language access control cannot
     * be suppressed, this method returns {@code false} (as opposed to {@code
     * setAccessible(true)} throwing {@code InaccessibleObjectException} when
     * it fails).
     *
     * This method is a no-op if the {@code accessible} flag for
     * this reflected object is {@code true}.
     *
     * For example, a caller can invoke {@code trySetAccessible}
     * on a {@code Method} object for a private instance method
     * {@code p.T::privateMethod} to suppress the checks for Java language access
     * control when the {@code Method} is invoked.
     * If {@code p.T} class is in a different module to the caller and
     * package {@code p} is open to at least the caller's module,
     * the code below successfully sets the {@code accessible} flag
     * to {@code true}.
     *
     * <pre>
     * {@code
     *     p.T obj = ....; // instance of p.T
     *     :
     *     Method m = p.T.class.getDeclaredMethod("privateMethod");
     *     if (m.trySetAccessible()) {
     *         m.invoke(obj);
     *     } else {
     *         // package p is not opened to the caller to access private member of T
     *         ...
     *     }
     * }</pre>
     *
     * @return {@code true} if the {@code accessible} flag is set to {@code true};
     *         {@code false} if access cannot be enabled.
     */
    @CallerSensitive
    public final boolean trySetAccessible() {
        if (override == true)
            return true;

        // if it's not a Constructor, Method, Field then no access check
        if (!Member.class.isInstance(this)) {
            return setAccessible0(true);
        }

        // does not allow to suppress access check for Class's constructor
        Class<?> declaringClass = ((Member) this).getDeclaringClass();
        if (declaringClass == Class.class && this instanceof Constructor) {
            return false;
        }

        if (checkCanSetAccessible(Reflection.getCallerClass(), declaringClass, false)) {
            return setAccessible0(true);
        } else {
            return false;
        }
    }

    /**
     * If the given AccessibleObject is a {@code Constructor}, {@code Method}
     * or {@code Field} then checks that its declaring class is in a package
     * that can be accessed by the given caller of setAccessible.
     */
    void checkCanSetAccessible(Class<?> caller) {
        // do nothing, needs to be overridden by Constructor, Method, Field
    }

    final void checkCanSetAccessible(Class<?> caller, Class<?> declaringClass) {
        checkCanSetAccessible(caller, declaringClass, true);
    }

    private boolean checkCanSetAccessible(Class<?> caller, Class<?> declaringClass, boolean throwExceptionIfDenied) {
        if (caller == MethodHandle.class) {
            throw new IllegalCallerException(); // should not happen
        }

        return true;
    }

    private boolean isSubclassOf(Class<?> queryClass, Class<?> ofClass) {
        while (queryClass != null) {
            if (queryClass == ofClass) {
                return true;
            }
            queryClass = queryClass.getSuperclass();
        }
        return false;
    }

    /**
     * Returns a short descriptive string to describe this object in log messages.
     */
    String toShortString() {
        return toString();
    }

    /**
     * Get the value of the {@code accessible} flag for this reflected object.
     *
     * @return the value of the object's {@code accessible} flag
     *
     * @deprecated
     * This method is deprecated because its name hints that it checks
     * if the reflected object is accessible when it actually indicates
     * if the checks for Java language access control are suppressed.
     * This method may return {@code false} on a reflected object that is
     * accessible to the caller. To test if this reflected object is accessible,
     * it should use {@link #canAccess(Object)}.
     */
    @Deprecated(since="9")
    public boolean isAccessible() {
        return override;
    }

    /**
     * Test if the caller can access this reflected object. If this reflected
     * object corresponds to an instance method or field then this method tests
     * if the caller can access the given {@code obj} with the reflected object.
     * For instance methods or fields then the {@code obj} argument must be an
     * instance of the {@link Member#getDeclaringClass() declaring class}. For
     * static members and constructors then {@code obj} must be {@code null}.
     *
     * This method returns {@code true} if the {@code accessible} flag
     * is set to {@code true}, i.e. the checks for Java language access control
     * are suppressed, or if the caller can access the member as
     * specified in <cite>The Java&trade; Language Specification</cite>,
     * with the variation noted in the class description.
     *
     * @param obj an instance object of the declaring class of this reflected
     *            object if it is an instance method or field
     *
     * @return {@code true} if the caller can access this reflected object.
     *
     * @throws IllegalArgumentException
     *         <ul>
     *         <li>if this reflected object is a static member or constructor and
     *              the given {@code obj} is non-{@code null}, or</li>
     *         <li>if this reflected object is an instance method or field
     *              and the given {@code obj} is {@code null} or of type
     *              that is not a subclass of the {@link Member#getDeclaringClass()
     *              declaring class} of the member.</li>
     *         </ul>
     */
    @CallerSensitive
    public final boolean canAccess(Object obj) {
        if (!Member.class.isInstance(this)) {
            return override;
        }

        Class<?> declaringClass = ((Member) this).getDeclaringClass();
        int modifiers = ((Member) this).getModifiers();
        if (!Modifier.isStatic(modifiers) && (this instanceof Method || this instanceof Field)) {
            if (obj == null) {
                throw new IllegalArgumentException("null object for " + this);
            }
            // if this object is an instance member, the given object
            // must be a subclass of the declaring class of this reflected object
            if (!declaringClass.isAssignableFrom(obj.getClass())) {
                throw new IllegalArgumentException("object is not an instance of " + declaringClass.getName());
            }
        } else if (obj != null) {
            throw new IllegalArgumentException("non-null object for " + this);
        }

        // access check is suppressed
        if (override)
            return true;

        Class<?> caller = Reflection.getCallerClass();
        Class<?> targetClass;
        if (this instanceof Constructor) {
            targetClass = declaringClass;
        } else {
            targetClass = Modifier.isStatic(modifiers) ? null : obj.getClass();
        }
        return verifyAccess(caller, declaringClass, targetClass, modifiers);
    }

    /**
     * Constructor: only used by the Java Virtual Machine.
     */
    protected AccessibleObject() {}

    // Indicates whether language-level access checks are overridden
    // by this object. Initializes to "false". This field is used by
    // Field, Method, and Constructor.
    //
    // NOTE: for security purposes, this field must not be visible
    // outside this package.
    boolean override;

    // Reflection factory used by subclasses for creating field,
    // method, and constructor accessors. Note that this is called
    // very early in the bootstrapping process.
    static final ReflectionFactory reflectionFactory = ReflectionFactory.getReflectionFactory();

    /**
     * @throws NullPointerException {@inheritDoc}
     */
    public <T extends Annotation> T getAnnotation(Class<T> annotationClass) {
        throw new AssertionError("All subclasses should override this method");
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
    public <T extends Annotation> T[] getAnnotationsByType(Class<T> annotationClass) {
        throw new AssertionError("All subclasses should override this method");
    }

    public Annotation[] getAnnotations() {
        return getDeclaredAnnotations();
    }

    /**
     * @throws NullPointerException {@inheritDoc}
     */
    @Override
    public <T extends Annotation> T getDeclaredAnnotation(Class<T> annotationClass) {
        // Only annotations on classes are inherited, for all other
        // objects getDeclaredAnnotation is the same as
        // getAnnotation.
        return getAnnotation(annotationClass);
    }

    /**
     * @throws NullPointerException {@inheritDoc}
     */
    @Override
    public <T extends Annotation> T[] getDeclaredAnnotationsByType(Class<T> annotationClass) {
        // Only annotations on classes are inherited, for all other
        // objects getDeclaredAnnotationsByType is the same as
        // getAnnotationsByType.
        return getAnnotationsByType(annotationClass);
    }

    public Annotation[] getDeclaredAnnotations() {
        throw new AssertionError("All subclasses should override this method");
    }

    // Shared access checking logic.

    // For non-public members or members in package-private classes,
    // it is necessary to perform somewhat expensive security checks.
    // If the security check succeeds for a given class, it will
    // always succeed (it is not affected by the granting or revoking
    // of permissions); we speed up the check in the common case by
    // remembering the last Class for which the check succeeded.
    //
    // The simple security check for Constructor is to see if
    // the caller has already been seen, verified, and cached.
    // (See also Class.newInstance(), which uses a similar method.)
    //
    // A more complicated security check cache is needed for Method and Field
    // The cache can be either null (empty cache), a 2-array of {caller,targetClass},
    // or a caller (with targetClass implicitly equal to memberClass).
    // In the 2-array case, the targetClass is always different from the memberClass.
    volatile Object securityCheckCache;

    final void checkAccess(Class<?> caller, Class<?> memberClass, Class<?> targetClass, int modifiers) throws IllegalAccessException
    {
        if (!verifyAccess(caller, memberClass, targetClass, modifiers)) {
            IllegalAccessException e = Reflection.newIllegalAccessException(caller, memberClass, targetClass, modifiers);
            if (printStackTraceWhenAccessFails()) {
                e.printStackTrace(System.err);
            }
            throw e;
        }
    }

    final boolean verifyAccess(Class<?> caller, Class<?> memberClass, Class<?> targetClass, int modifiers)
    {
        if (caller == memberClass) { // quick check
            return true; // ACCESS IS OK
        }
        Object cache = securityCheckCache; // read volatile
        if (targetClass != null // instance member or constructor
            && Modifier.isProtected(modifiers)
            && targetClass != memberClass) {
            // Must match a 2-list of { caller, targetClass }.
            if (cache instanceof Class[]) {
                Class<?>[] cache2 = (Class<?>[]) cache;
                if (cache2[1] == targetClass && cache2[0] == caller) {
                    return true; // ACCESS IS OK
                }
                // (Test cache[1] first since range check for [1]
                // subsumes range check for [0].)
            }
        } else if (cache == caller) {
            // Non-protected case (or targetClass == memberClass or static member).
            return true; // ACCESS IS OK
        }

        // If no return, fall through to the slow path.
        return slowVerifyAccess(caller, memberClass, targetClass, modifiers);
    }

    // Keep all this slow stuff out of line:
    private boolean slowVerifyAccess(Class<?> caller, Class<?> memberClass, Class<?> targetClass, int modifiers)
    {
        if (!Reflection.verifyMemberAccess(caller, memberClass, targetClass, modifiers)) {
            // access denied
            return false;
        }

        // Success: Update the cache.
        Object cache = (targetClass != null && Modifier.isProtected(modifiers) && targetClass != memberClass) ? new Class<?>[] { caller, targetClass } : caller;

        // Note:  The two cache elements are not volatile,
        // but they are effectively final.  The Java memory model
        // guarantees that the initializing stores for the cache
        // elements will occur before the volatile write.
        securityCheckCache = cache; // write volatile
        return true;
    }

    // true to print a stack trace when access fails
    private static volatile boolean printStackWhenAccessFails;

    // true if printStack* values are initialized
    private static volatile boolean printStackPropertiesSet;

    /**
     * Returns true if a stack trace should be printed when access fails.
     */
    private static boolean printStackTraceWhenAccessFails() {
        if (!printStackPropertiesSet && VM.initLevel() >= 1) {
            String s = null; // "sun.reflect.debugModuleAccessChecks"
            if (s != null) {
                printStackWhenAccessFails = !s.equalsIgnoreCase("false");
            }
            printStackPropertiesSet = true;
        }
        return printStackWhenAccessFails;
    }

    /**
     * Returns the root AccessibleObject; or null if this object is the root.
     *
     * All subclasses override this method.
     */
    AccessibleObject getRoot() {
        throw new InternalError();
    }
}

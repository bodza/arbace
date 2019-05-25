package jdk.internal.reflect;

import java.lang.invoke.MethodHandle;
import java.lang.reflect.Field;
import java.lang.reflect.Executable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Objects;

import jdk.internal.misc.VM;
import sun.reflect.misc.ReflectUtil;

/**
 * The master factory for all reflective objects, both those in
 * java.lang.reflect (Fields, Methods, Constructors) as well as their
 * delegates (FieldAccessors, MethodAccessors, ConstructorAccessors).
 *
 * The methods in this class are extremely unsafe and can cause
 * subversion of both the language and the verifier. For this reason,
 * they are all instance methods, and access to the constructor of
 * this factory is guarded by a security check, in similar style to
 * {@link jdk.internal.misc.Unsafe}.
 */
public class ReflectionFactory {
    private static boolean initted = false;
    private static final ReflectionFactory soleInstance = new ReflectionFactory();
    // Provides access to package-private mechanisms in java.lang.reflect
    private static volatile LangReflectAccess langReflectAccess;

    /* Method for static class initializer <clinit>, or null */
    private static volatile Method hasStaticInitializerMethod;

    // "Inflation" mechanism. Loading bytecodes to implement
    // Method.invoke() and Constructor.newInstance() currently costs
    // 3-4x more than an invocation via native code for the first
    // invocation (though subsequent invocations have been benchmarked
    // to be over 20x faster). Unfortunately this cost increases
    // startup time for certain applications that use reflection
    // intensively (but only once per class) to bootstrap themselves.
    // To avoid this penalty we reuse the existing JVM entry points
    // for the first few invocations of Methods and Constructors and
    // then switch to the bytecode-based implementations.
    //
    // Package-private to be accessible to NativeMethodAccessorImpl
    // and NativeConstructorAccessorImpl
    private static boolean noInflation        = false;
    private static int     inflationThreshold = 15;

    private ReflectionFactory() {
    }

    /**
     * Provides the caller with the capability to instantiate reflective
     * objects.
     *
     * The returned <code>ReflectionFactory</code> object should be
     * carefully guarded by the caller, since it can be used to read and
     * write private data and invoke private methods, as well as to load
     * unverified bytecodes.  It must never be passed to untrusted code.
     */
    public static ReflectionFactory getReflectionFactory() {
        return soleInstance;
    }

    /**
     * Returns an alternate reflective Method instance for the given method
     * intended for reflection to invoke, if present.
     *
     * A trusted method can define an alternate implementation for a method `foo`
     * by defining a method named "reflected$foo" that will be invoked
     * reflectively.
     */
    private static Method findMethodForReflection(Method method) {
        String altName = "reflected$" + method.getName();
        try {
           return method.getDeclaringClass().getDeclaredMethod(altName, method.getParameterTypes());
        } catch (NoSuchMethodException ex) {
            return null;
        }
    }

    // Routines used by java.lang.reflect

    /** Called only by java.lang.reflect.Modifier's static initializer */
    public void setLangReflectAccess(LangReflectAccess access) {
        langReflectAccess = access;
    }

    /**
     * Note: this routine can cause the declaring class for the field
     * be initialized and therefore must not be called until the
     * first get/set of this field.
     * @param field the field
     * @param override true if caller has overridden accessibility
     */
    public FieldAccessor newFieldAccessor(Field field, boolean override) {
        checkInitted();

        Field root = langReflectAccess.getRoot(field);
        if (root != null) {
            // FieldAccessor will use the root unless the modifiers have
            // been overrridden
            if (root.getModifiers() == field.getModifiers() || !override) {
                field = root;
            }
        }
        return UnsafeFieldAccessorFactory.newFieldAccessor(field, override);
    }

    public MethodAccessor newMethodAccessor(Method method) {
        checkInitted();

        if (Reflection.isCallerSensitive(method)) {
            Method altMethod = findMethodForReflection(method);
            if (altMethod != null) {
                method = altMethod;
            }
        }

        // use the root Method that will not cache caller class
        Method root = langReflectAccess.getRoot(method);
        if (root != null) {
            method = root;
        }

        if (noInflation && !ReflectUtil.isVMAnonymousClass(method.getDeclaringClass())) {
            return new MethodAccessorGenerator().
                generateMethod(method.getDeclaringClass(),
                               method.getName(),
                               method.getParameterTypes(),
                               method.getReturnType(),
                               method.getExceptionTypes(),
                               method.getModifiers());
        } else {
            NativeMethodAccessorImpl acc = new NativeMethodAccessorImpl(method);
            DelegatingMethodAccessorImpl res = new DelegatingMethodAccessorImpl(acc);
            acc.setParent(res);
            return res;
        }
    }

    public ConstructorAccessor newConstructorAccessor(Constructor<?> c) {
        checkInitted();

        Class<?> declaringClass = c.getDeclaringClass();
        if (Modifier.isAbstract(declaringClass.getModifiers())) {
            return new InstantiationExceptionConstructorAccessorImpl(null);
        }
        if (declaringClass == Class.class) {
            return new InstantiationExceptionConstructorAccessorImpl("Can not instantiate java.lang.Class");
        }

        // use the root Constructor that will not cache caller class
        Constructor<?> root = langReflectAccess.getRoot(c);
        if (root != null) {
            c = root;
        }

        // Bootstrapping issue: since we use Class.newInstance() in
        // the ConstructorAccessor generation process, we have to
        // break the cycle here.
        if (Reflection.isSubclassOf(declaringClass, ConstructorAccessorImpl.class)) {
            return new BootstrapConstructorAccessorImpl(c);
        }

        if (noInflation && !ReflectUtil.isVMAnonymousClass(c.getDeclaringClass())) {
            return new MethodAccessorGenerator().
                generateConstructor(c.getDeclaringClass(),
                                    c.getParameterTypes(),
                                    c.getExceptionTypes(),
                                    c.getModifiers());
        } else {
            NativeConstructorAccessorImpl acc = new NativeConstructorAccessorImpl(c);
            DelegatingConstructorAccessorImpl res = new DelegatingConstructorAccessorImpl(acc);
            acc.setParent(res);
            return res;
        }
    }

    // Routines used by java.lang

    /**
      * Creates a new java.lang.reflect.Field. Access checks as per
      * java.lang.reflect.AccessibleObject are not overridden.
      */
    public Field newField(Class<?> declaringClass, String name, Class<?> type, int modifiers, int slot, String signature, byte[] annotations) {
        return langReflectAccess().newField(declaringClass, name, type, modifiers, slot, signature, null);
    }

    /**
      * Creates a new java.lang.reflect.Method. Access checks as per
      * java.lang.reflect.AccessibleObject are not overridden.
      */
    public Method newMethod(Class<?> declaringClass, String name, Class<?>[] parameterTypes, Class<?> returnType, Class<?>[] checkedExceptions, int modifiers, int slot, String signature, byte[] annotations, byte[] parameterAnnotations, byte[] annotationDefault) {
        return langReflectAccess().newMethod(declaringClass, name, parameterTypes, returnType, checkedExceptions, modifiers, slot, signature, null, null, null);
    }

    /**
      * Creates a new java.lang.reflect.Constructor. Access checks as
      * per java.lang.reflect.AccessibleObject are not overridden.
      */
    public Constructor<?> newConstructor(Class<?> declaringClass, Class<?>[] parameterTypes, Class<?>[] checkedExceptions, int modifiers, int slot, String signature, byte[] annotations, byte[] parameterAnnotations) {
        return langReflectAccess().newConstructor(declaringClass, parameterTypes, checkedExceptions, modifiers, slot, signature, null, null);
    }

    /** Gets the MethodAccessor object for a java.lang.reflect.Method */
    public MethodAccessor getMethodAccessor(Method m) {
        return langReflectAccess().getMethodAccessor(m);
    }

    /** Sets the MethodAccessor object for a java.lang.reflect.Method */
    public void setMethodAccessor(Method m, MethodAccessor accessor) {
        langReflectAccess().setMethodAccessor(m, accessor);
    }

    /**
      * Gets the ConstructorAccessor object for a
      * java.lang.reflect.Constructor
      */
    public ConstructorAccessor getConstructorAccessor(Constructor<?> c) {
        return langReflectAccess().getConstructorAccessor(c);
    }

    /**
      * Sets the ConstructorAccessor object for a
      * java.lang.reflect.Constructor
      */
    public void setConstructorAccessor(Constructor<?> c, ConstructorAccessor accessor) {
        langReflectAccess().setConstructorAccessor(c, accessor);
    }

    /**
      * Makes a copy of the passed method. The returned method is a
      * "child" of the passed one; see the comments in Method.java for
      * details.
      */
    public Method copyMethod(Method arg) {
        return langReflectAccess().copyMethod(arg);
    }

    /** Makes a copy of the passed method. The returned method is NOT
     * a "child" but a "sibling" of the Method in arg. Should only be
     * used on non-root methods. */
    public Method leafCopyMethod(Method arg) {
        return langReflectAccess().leafCopyMethod(arg);
    }

    /**
      * Makes a copy of the passed field. The returned field is a
      * "child" of the passed one; see the comments in Field.java for
      * details.
      */
    public Field copyField(Field arg) {
        return langReflectAccess().copyField(arg);
    }

    /**
      * Makes a copy of the passed constructor. The returned
      * constructor is a "child" of the passed one; see the comments
      * in Constructor.java for details.
      */
    public <T> Constructor<T> copyConstructor(Constructor<T> arg) {
        return langReflectAccess().copyConstructor(arg);
    }

    public Class<?>[] getExecutableSharedParameterTypes(Executable ex) {
        return langReflectAccess().getExecutableSharedParameterTypes(ex);
    }

    /**
     * Given a class, determines whether its superclass has
     * any constructors that are accessible from the class.
     * This is a special purpose method intended to do access
     * checking for a serializable class and its superclasses
     * up to, but not including, the first non-serializable
     * superclass. This also implies that the superclass is
     * always non-null, because a serializable class must be a
     * class (not an interface) and Object is not serializable.
     *
     * @param cl the class from which access is checked
     * @return whether the superclass has a constructor accessible from cl
     */
    private boolean superHasAccessibleConstructor(Class<?> cl) {
        Class<?> superCl = cl.getSuperclass();
        assert superCl != null;
        if (packageEquals(cl, superCl)) {
            // accessible if any non-private constructor is found
            for (Constructor<?> ctor : superCl.getDeclaredConstructors()) {
                if ((ctor.getModifiers() & Modifier.PRIVATE) == 0) {
                    return true;
                }
            }
            if (Reflection.areNestMates(cl, superCl)) {
                return true;
            }
            return false;
        } else {
            // sanity check to ensure the parent is protected or public
            if ((superCl.getModifiers() & (Modifier.PROTECTED | Modifier.PUBLIC)) == 0) {
                return false;
            }
            // accessible if any constructor is protected or public
            for (Constructor<?> ctor : superCl.getDeclaredConstructors()) {
                if ((ctor.getModifiers() & (Modifier.PROTECTED | Modifier.PUBLIC)) != 0) {
                    return true;
                }
            }
            return false;
        }
    }

    private final Constructor<?> generateConstructor(Class<?> cl, Constructor<?> constructorToCall) {
        ConstructorAccessor acc = new MethodAccessorGenerator().
            generateSerializationConstructor(cl,
                                             constructorToCall.getParameterTypes(),
                                             constructorToCall.getExceptionTypes(),
                                             constructorToCall.getModifiers(),
                                             constructorToCall.getDeclaringClass());
        Constructor<?> c = newConstructor(constructorToCall.getDeclaringClass(),
                                          constructorToCall.getParameterTypes(),
                                          constructorToCall.getExceptionTypes(),
                                          constructorToCall.getModifiers(),
                                          langReflectAccess().getConstructorSlot(constructorToCall),
                                          langReflectAccess().getConstructorSignature(constructorToCall),
                                          langReflectAccess().getConstructorAnnotations(constructorToCall),
                                          langReflectAccess().getConstructorParameterAnnotations(constructorToCall));
        setConstructorAccessor(c, acc);
        c.setAccessible(true);
        return c;
    }

    // Internals only below this point

    static int inflationThreshold() {
        return inflationThreshold;
    }

    /**
     * We have to defer full initialization of this class until after
     * the static initializer is run since java.lang.reflect.Method's
     * static initializer (more properly, that for
     * java.lang.reflect.AccessibleObject) causes this class's to be
     * run, before the system properties are set up.
     */
    private static void checkInitted() {
        if (initted)
            return;

        // Defer initialization until module system is initialized so as
        // to avoid inflation and spinning bytecode in unnamed modules
        // during early startup.
        if (!VM.isModuleSystemInited()) {
            return;
        }

        String val = null; // "sun.reflect.noInflation"
        if (val != null && val.equals("true")) {
            noInflation = true;
        }

        val = null; // "sun.reflect.inflationThreshold"
        if (val != null) {
            try {
                inflationThreshold = Integer.parseInt(val);
            } catch (NumberFormatException e) {
                throw new RuntimeException("Unable to parse property sun.reflect.inflationThreshold", e);
            }
        }

        initted = true;
    }

    private static LangReflectAccess langReflectAccess() {
        if (langReflectAccess == null) {
            // Call a static method to get class java.lang.reflect.Modifier
            // initialized. Its static initializer will cause
            // setLangReflectAccess() to be called from the context of the
            // java.lang.reflect package.
            Modifier.isPublic(Modifier.PUBLIC);
        }
        return langReflectAccess;
    }

    /**
     * Returns true if classes are defined in the classloader and same package, false
     * otherwise.
     * @param cl1 a class
     * @param cl2 another class
     * @returns true if the two classes are in the same classloader and package
     */
    private static boolean packageEquals(Class<?> cl1, Class<?> cl2) {
        assert !cl1.isArray() && !cl2.isArray();

        if (cl1 == cl2) {
            return true;
        }

        return cl1.getClassLoader() == cl2.getClassLoader() && Objects.equals(cl1.getPackageName(), cl2.getPackageName());
    }
}

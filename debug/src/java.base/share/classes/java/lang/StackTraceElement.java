package java.lang;

import jdk.internal.loader.BuiltinClassLoader;
import jdk.internal.misc.VM;

import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * An element in a stack trace, as returned by {@link
 * Throwable#getStackTrace()}.  Each element represents a single stack frame.
 * All stack frames except for the one at the top of the stack represent
 * a method invocation.  The frame at the top of the stack represents the
 * execution point at which the stack trace was generated.  Typically,
 * this is the point at which the throwable corresponding to the stack trace
 * was created.
 */
public final class StackTraceElement {
    // For Throwables and StackWalker, the VM initially sets this field to a
    // reference to the declaring Class.  The Class reference is used to
    // construct the 'format' bitmap, and then is cleared.
    //
    // For STEs constructed using the public constructors, this field is not used.
    private transient Class<?> declaringClassObject;

    // Normally initialized by VM
    private String classLoaderName;
    private String declaringClass;
    private String methodName;
    private String fileName;
    private int    lineNumber;
    private byte   format = 0; // Default to show all

    /**
     * Creates a stack trace element representing the specified execution
     * point.
     *
     * @param declaringClass the fully qualified name of the class containing
     *        the execution point represented by the stack trace element
     * @param methodName the name of the method containing the execution point
     *        represented by the stack trace element
     * @param fileName the name of the file containing the execution point
     *         represented by the stack trace element, or {@code null} if
     *         this information is unavailable
     * @param lineNumber the line number of the source line containing the
     *         execution point represented by this stack trace element, or
     *         a negative number if this information is unavailable. A value
     *         of -2 indicates that the method containing the execution point
     *         is a native method
     *
     * @throws NullPointerException if {@code declaringClass} or
     *         {@code methodName} is null
     */
    public StackTraceElement(String declaringClass, String methodName, String fileName, int lineNumber) {
        this(null, declaringClass, methodName, fileName, lineNumber);
    }

    /**
     * Creates a stack trace element representing the specified execution
     * point.
     *
     * @param classLoaderName the class loader name if the class loader of
     *        the class containing the execution point represented by
     *        the stack trace is named; otherwise {@code null}
     * @param declaringClass the fully qualified name of the class containing
     *        the execution point represented by the stack trace element
     * @param methodName the name of the method containing the execution point
     *        represented by the stack trace element
     * @param fileName the name of the file containing the execution point
     *        represented by the stack trace element, or {@code null} if
     *        this information is unavailable
     * @param lineNumber the line number of the source line containing the
     *        execution point represented by this stack trace element, or
     *        a negative number if this information is unavailable. A value
     *        of -2 indicates that the method containing the execution point
     *        is a native method
     *
     * @throws NullPointerException if {@code declaringClass} is {@code null}
     *         or {@code methodName} is {@code null}
     */
    public StackTraceElement(String classLoaderName, String declaringClass, String methodName, String fileName, int lineNumber) {
        this.classLoaderName = classLoaderName;
        this.declaringClass  = Objects.requireNonNull(declaringClass, "Declaring class is null");
        this.methodName      = Objects.requireNonNull(methodName, "Method name is null");
        this.fileName        = fileName;
        this.lineNumber      = lineNumber;
    }

    /*
     * Private constructor for the factory methods to create StackTraceElement
     * for Throwable and StackFrameInfo
     */
    private StackTraceElement() {}

    /**
     * Returns the name of the source file containing the execution point
     * represented by this stack trace element.  Generally, this corresponds
     * to the {@code SourceFile} attribute of the relevant {@code class}
     * file (as per <i>The Java Virtual Machine Specification</i>, Section
     * 4.7.7).  In some systems, the name may refer to some source code unit
     * other than a file, such as an entry in source repository.
     *
     * @return the name of the file containing the execution point
     *         represented by this stack trace element, or {@code null} if
     *         this information is unavailable.
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * Returns the line number of the source line containing the execution
     * point represented by this stack trace element.  Generally, this is
     * derived from the {@code LineNumberTable} attribute of the relevant
     * {@code class} file (as per <i>The Java Virtual Machine
     * Specification</i>, Section 4.7.8).
     *
     * @return the line number of the source line containing the execution
     *         point represented by this stack trace element, or a negative
     *         number if this information is unavailable.
     */
    public int getLineNumber() {
        return lineNumber;
    }

    /**
     * Returns the name of the class loader of the class containing the
     * execution point represented by this stack trace element.
     *
     * @return the name of the class loader of the class containing the execution
     *         point represented by this stack trace element; {@code null}
     *         if the class loader is not named.
     */
    public String getClassLoaderName() {
        return classLoaderName;
    }

    /**
     * Returns the fully qualified name of the class containing the
     * execution point represented by this stack trace element.
     *
     * @return the fully qualified name of the {@code Class} containing
     *         the execution point represented by this stack trace element.
     */
    public String getClassName() {
        return declaringClass;
    }

    /**
     * Returns the name of the method containing the execution point
     * represented by this stack trace element.  If the execution point is
     * contained in an instance or class initializer, this method will return
     * the appropriate <i>special method name</i>, {@code <init>} or
     * {@code <clinit>}, as per Section 3.9 of <i>The Java Virtual
     * Machine Specification</i>.
     *
     * @return the name of the method containing the execution point
     *         represented by this stack trace element.
     */
    public String getMethodName() {
        return methodName;
    }

    /**
     * Returns true if the method containing the execution point
     * represented by this stack trace element is a native method.
     *
     * @return {@code true} if the method containing the execution point
     *         represented by this stack trace element is a native method.
     */
    public boolean isNativeMethod() {
        return lineNumber == -2;
    }

    /**
     * Returns a string representation of this stack trace element.
     *
     * @apiNote The format of this string depends on the implementation, but the
     * following examples may be regarded as typical:
     * <ul>
     * <li>
     *     "{@code com.foo.loader/foo@9.0/com.foo.Main.run(Main.java:101)}"
     * - See the description below.
     * </li>
     * <li>
     *     "{@code com.foo.loader/foo@9.0/com.foo.Main.run(Main.java)}"
     * - The line number is unavailable.
     * </li>
     * <li>
     *     "{@code com.foo.loader/foo@9.0/com.foo.Main.run(Unknown Source)}"
     * - Neither the file name nor the line number is available.
     * </li>
     * <li>
     *     "{@code com.foo.loader/foo@9.0/com.foo.Main.run(Native Method)}"
     * - The method containing the execution point is a native method.
     * </li>
     * <li>
     *     "{@code com.foo.loader//com.foo.bar.App.run(App.java:12)}"
     * - The class of the execution point is defined in the unnamed module of
     * the class loader named {@code com.foo.loader}.
     * </li>
     * <li>
     *     "{@code acme@2.1/org.acme.Lib.test(Lib.java:80)}"
     * - The class of the execution point is defined in {@code acme} module
     * loaded by a built-in class loader such as the application class loader.
     * </li>
     * <li>
     *     "{@code MyClass.mash(MyClass.java:9)}"
     * - {@code MyClass} class is on the application class path.
     * </li>
     * </ul>
     *
     * The first example shows a stack trace element consisting of
     * three elements, each separated by {@code "/"} followed with
     * the source file name and the line number of the source line
     * containing the execution point.
     *
     * The first element "{@code com.foo.loader}" is
     * the name of the class loader.  The second element "{@code foo@9.0}"
     * is the module name and version.  The third element is the method
     * containing the execution point; "{@code com.foo.Main"}" is the
     * fully-qualified class name and "{@code run}" is the name of the method.
     * "{@code Main.java}" is the source file name and "{@code 101}" is
     * the line number.
     *
     * If a class is defined in an <em>unnamed module</em>
     * then the second element is omitted as shown in
     * "{@code com.foo.loader//com.foo.bar.App.run(App.java:12)}".
     *
     * If the class loader is a <a href="ClassLoader.html#builtinLoaders">
     * built-in class loader</a> or is not named then the first element
     * and its following {@code "/"} are omitted as shown in
     * "{@code acme@2.1/org.acme.Lib.test(Lib.java:80)}".
     * If the first element is omitted and the module is an unnamed module,
     * the second element and its following {@code "/"} are also omitted
     * as shown in "{@code MyClass.mash(MyClass.java:9)}".
     *
     * The {@code toString} method may return two different values on two
     * {@code StackTraceElement} instances that are
     * {@linkplain #equals(Object) equal}, for example one created via the
     * constructor, and one obtained from {@link java.lang.Throwable} or
     * {@link java.lang.StackWalker.StackFrame}, where an implementation may
     * choose to omit some element in the returned string.
     */
    public String toString() {
        String s = "";
        if (!dropClassLoaderName() && classLoaderName != null && !classLoaderName.isEmpty()) {
            s += classLoaderName + "/";
        }
        s = s.isEmpty() ? declaringClass : s + "/" + declaringClass;

        return s + "." + methodName + "(" +
             (isNativeMethod() ? "Native Method)" :
              (fileName != null && lineNumber >= 0 ?
               fileName + ":" + lineNumber + ")" :
                (fileName != null ?  ""+fileName+")" : "Unknown Source)")));
    }

    /**
     * Returns true if the specified object is another
     * {@code StackTraceElement} instance representing the same execution
     * point as this instance.  Two stack trace elements {@code a} and
     * {@code b} are equal if and only if:
     * <pre>{@code
     *     equals(a.getClassLoaderName(), b.getClassLoaderName()) &&
     *     equals(a.getClassName(), b.getClassName()) &&
     *     equals(a.getMethodName(), b.getMethodName())
     *     equals(a.getFileName(), b.getFileName()) &&
     *     a.getLineNumber() == b.getLineNumber()
     *
     * }</pre>
     * where {@code equals} has the semantics of {@link
     * java.util.Objects#equals(Object, Object) Objects.equals}.
     *
     * @param obj the object to be compared with this stack trace element.
     * @return true if the specified object is another
     *         {@code StackTraceElement} instance representing the same
     *         execution point as this instance.
     */
    public boolean equals(Object obj) {
        if (obj==this)
            return true;
        if (!(obj instanceof StackTraceElement))
            return false;
        StackTraceElement e = (StackTraceElement)obj;
        return Objects.equals(classLoaderName, e.classLoaderName) &&
            e.declaringClass.equals(declaringClass) &&
            e.lineNumber == lineNumber &&
            Objects.equals(methodName, e.methodName) &&
            Objects.equals(fileName, e.fileName);
    }

    /**
     * Returns a hash code value for this stack trace element.
     */
    public int hashCode() {
        int result = 31*declaringClass.hashCode() + methodName.hashCode();
        result = 31*result + Objects.hashCode(classLoaderName);
        result = 31*result + Objects.hashCode(fileName);
        result = 31*result + lineNumber;
        return result;
    }

    /**
     * Called from of() methods to set the 'format' bitmap using the Class
     * reference stored in declaringClassObject, and then clear the reference.
     *
     * If the loader is one of the built-in loaders (`boot`, `platform`, or `app`)
     * then set BUILTIN_CLASS_LOADER to omit the first element (`<loader>/`).
     */
    private synchronized void computeFormat() {
        try {
            Class<?> cls = (Class<?>) declaringClassObject;
            ClassLoader loader = cls.getClassLoader0();

            byte bits = 0;
            if (loader instanceof BuiltinClassLoader) {
                bits |= BUILTIN_CLASS_LOADER;
            }
            format = bits;
        } finally {
            // Class reference no longer needed, clear it
            declaringClassObject = null;
        }
    }

    private static final byte BUILTIN_CLASS_LOADER = 0x1;

    private boolean dropClassLoaderName() {
        return (format & BUILTIN_CLASS_LOADER) == BUILTIN_CLASS_LOADER;
    }

    /*
     * Returns an array of StackTraceElements of the given depth
     * filled from the backtrace of a given Throwable.
     */
    static StackTraceElement[] of(Throwable x, int depth) {
        StackTraceElement[] stackTrace = new StackTraceElement[depth];
        for (int i = 0; i < depth; i++) {
            stackTrace[i] = new StackTraceElement();
        }

        // VM to fill in StackTraceElement
        initStackTraceElements(stackTrace, x);

        // ensure the proper StackTraceElement initialization
        for (StackTraceElement ste : stackTrace) {
            ste.computeFormat();
        }
        return stackTrace;
    }

    /*
     * Returns a StackTraceElement from a given StackFrameInfo.
     */
    static StackTraceElement of(StackFrameInfo sfi) {
        StackTraceElement ste = new StackTraceElement();
        initStackTraceElement(ste, sfi);

        ste.computeFormat();
        return ste;
    }

    /*
     * Sets the given stack trace elements with the backtrace
     * of the given Throwable.
     */
    private static native void initStackTraceElements(StackTraceElement[] elements, Throwable x);
    /*
     * Sets the given stack trace element with the given StackFrameInfo
     */
    private static native void initStackTraceElement(StackTraceElement element, StackFrameInfo sfi);
}

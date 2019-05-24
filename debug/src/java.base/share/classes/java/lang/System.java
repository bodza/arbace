package java.lang;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.Console;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.nio.charset.CharacterCodingException;
import java.nio.channels.Channel;
import java.nio.channels.spi.SelectorProvider;
import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.ResourceBundle;
import java.util.function.Supplier;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import jdk.internal.reflect.CallerSensitive;
import jdk.internal.reflect.Reflection;
import jdk.internal.HotSpotIntrinsicCandidate;
import jdk.internal.misc.VM;
import sun.reflect.annotation.AnnotationType;
import sun.nio.ch.Interruptible;

/**
 * The {@code System} class contains several useful class fields
 * and methods. It cannot be instantiated.
 *
 * Among the facilities provided by the {@code System} class
 * are standard input, standard output, and error output streams;
 * access to externally defined properties and environment
 * variables; a means of loading files and libraries; and a utility
 * method for quickly copying a portion of an array.
 */
public final class System {
    /* Register the natives via the static initializer.
     *
     * VM will invoke the initializeSystemClass method to complete
     * the initialization for this class separated from clinit.
     * Note that to use properties set by the VM, see the constraints
     * described in the initializeSystemClass method.
     */
    private static native void registerNatives();
    static {
        registerNatives();
    }

    /** Don't let anyone instantiate this class */
    private System() {
    }

    /**
     * The "standard" input stream. This stream is already
     * open and ready to supply input data. Typically this stream
     * corresponds to keyboard input or another input source specified by
     * the host environment or user.
     */
    public static final InputStream in = null;

    /**
     * The "standard" output stream. This stream is already
     * open and ready to accept output data. Typically this stream
     * corresponds to display output or another output destination
     * specified by the host environment or user.
     *
     * For simple stand-alone Java applications, a typical way to write
     * a line of output data is:
     * <blockquote><pre>
     *     System.out.println(data)
     * </pre></blockquote>
     *
     * See the {@code println} methods in class {@code PrintStream}.
     */
    public static final PrintStream out = null;

    /**
     * The "standard" error output stream. This stream is already
     * open and ready to accept output data.
     *
     * Typically this stream corresponds to display output or another
     * output destination specified by the host environment or user. By
     * convention, this output stream is used to display error messages
     * or other information that should come to the immediate attention
     * of a user even if the principal output stream, the value of the
     * variable {@code out}, has been redirected to a file or other
     * destination that is typically not continuously monitored.
     */
    public static final PrintStream err = null;

    /**
     * Reassigns the "standard" input stream.
     *
     * @param in the new standard input stream.
     */
    public static void setIn(InputStream in) {
        setIn0(in);
    }

    /**
     * Reassigns the "standard" output stream.
     *
     * @param out the new standard output stream
     */
    public static void setOut(PrintStream out) {
        setOut0(out);
    }

    /**
     * Reassigns the "standard" error output stream.
     *
     * @param err the new standard error output stream.
     */
    public static void setErr(PrintStream err) {
        setErr0(err);
    }

    private static volatile Console cons;
    /**
     * Returns the unique {@link java.io.Console Console} object associated
     * with the current Java virtual machine, if any.
     *
     * @return The system console, if any, otherwise {@code null}.
     */
     public static Console console() {
         Console c;
         if ((c = cons) == null) {
             synchronized (System.class) {
                 if ((c = cons) == null) {
                     cons = c = Console.console();
                 }
             }
         }
         return c;
     }

    private static native void setIn0(InputStream in);
    private static native void setOut0(PrintStream out);
    private static native void setErr0(PrintStream err);

    /**
     * Returns the current time in milliseconds.  Note that
     * while the unit of time of the return value is a millisecond,
     * the granularity of the value depends on the underlying
     * operating system and may be larger.  For example, many
     * operating systems measure time in units of tens of
     * milliseconds.
     *
     * @return the difference, measured in milliseconds, between
     *          the current time and midnight, January 1, 1970 UTC.
     */
    @HotSpotIntrinsicCandidate
    public static native long currentTimeMillis();

    /**
     * Returns the current value of the running Java Virtual Machine's
     * high-resolution time source, in nanoseconds.
     *
     * This method can only be used to measure elapsed time and is
     * not related to any other notion of system or wall-clock time.
     * The value returned represents nanoseconds since some fixed but
     * arbitrary <i>origin</i> time (perhaps in the future, so values
     * may be negative).  The same origin is used by all invocations of
     * this method in an instance of a Java virtual machine; other
     * virtual machine instances are likely to use a different origin.
     *
     * This method provides nanosecond precision, but not necessarily
     * nanosecond resolution (that is, how frequently the value changes)
     * - no guarantees are made except that the resolution is at least as
     * good as that of {@link #currentTimeMillis()}.
     *
     * Differences in successive calls that span greater than
     * approximately 292 years (2<sup>63</sup> nanoseconds) will not
     * correctly compute elapsed time due to numerical overflow.
     *
     * The values returned by this method become meaningful only when
     * the difference between two such values, obtained within the same
     * instance of a Java virtual machine, is computed.
     *
     * For example, to measure how long some code takes to execute:
     * <pre> {@code
     * long startTime = System.nanoTime();
     * // ... the code being measured ...
     * long elapsedNanos = System.nanoTime() - startTime;}</pre>
     *
     * To compare elapsed time against a timeout, use <pre> {@code
     * if (System.nanoTime() - startTime >= timeoutNanos) ...}</pre>
     * instead of <pre> {@code
     * if (System.nanoTime() >= startTime + timeoutNanos) ...}</pre>
     * because of the possibility of numerical overflow.
     *
     * @return the current value of the running Java Virtual Machine's
     *         high-resolution time source, in nanoseconds
     */
    @HotSpotIntrinsicCandidate
    public static native long nanoTime();

    /**
     * Copies an array from the specified source array, beginning at the
     * specified position, to the specified position of the destination array.
     * A subsequence of array components are copied from the source
     * array referenced by {@code src} to the destination array
     * referenced by {@code dest}. The number of components copied is
     * equal to the {@code length} argument. The components at
     * positions {@code srcPos} through
     * {@code srcPos+length-1} in the source array are copied into
     * positions {@code destPos} through
     * {@code destPos+length-1}, respectively, of the destination
     * array.
     *
     * If the {@code src} and {@code dest} arguments refer to the
     * same array object, then the copying is performed as if the
     * components at positions {@code srcPos} through
     * {@code srcPos+length-1} were first copied to a temporary
     * array with {@code length} components and then the contents of
     * the temporary array were copied into positions
     * {@code destPos} through {@code destPos+length-1} of the
     * destination array.
     *
     * If {@code dest} is {@code null}, then a
     * {@code NullPointerException} is thrown.
     *
     * If {@code src} is {@code null}, then a
     * {@code NullPointerException} is thrown and the destination
     * array is not modified.
     *
     * Otherwise, if any of the following is true, an
     * {@code ArrayStoreException} is thrown and the destination is
     * not modified:
     * <ul>
     * <li>The {@code src} argument refers to an object that is not an
     *     array.
     * <li>The {@code dest} argument refers to an object that is not an
     *     array.
     * <li>The {@code src} argument and {@code dest} argument refer
     *     to arrays whose component types are different primitive types.
     * <li>The {@code src} argument refers to an array with a primitive
     *    component type and the {@code dest} argument refers to an array
     *     with a reference component type.
     * <li>The {@code src} argument refers to an array with a reference
     *    component type and the {@code dest} argument refers to an array
     *     with a primitive component type.
     * </ul>
     *
     * Otherwise, if any of the following is true, an
     * {@code IndexOutOfBoundsException} is
     * thrown and the destination is not modified:
     * <ul>
     * <li>The {@code srcPos} argument is negative.
     * <li>The {@code destPos} argument is negative.
     * <li>The {@code length} argument is negative.
     * <li>{@code srcPos+length} is greater than
     *     {@code src.length}, the length of the source array.
     * <li>{@code destPos+length} is greater than
     *     {@code dest.length}, the length of the destination array.
     * </ul>
     *
     * Otherwise, if any actual component of the source array from
     * position {@code srcPos} through
     * {@code srcPos+length-1} cannot be converted to the component
     * type of the destination array by assignment conversion, an
     * {@code ArrayStoreException} is thrown. In this case, let
     * <b><i>k</i></b> be the smallest nonnegative integer less than
     * length such that {@code src[srcPos+}<i>k</i>{@code ]}
     * cannot be converted to the component type of the destination
     * array; when the exception is thrown, source array components from
     * positions {@code srcPos} through
     * {@code srcPos+}<i>k</i>{@code -1}
     * will already have been copied to destination array positions
     * {@code destPos} through
     * {@code destPos+}<i>k</i>{@code -1} and no other
     * positions of the destination array will have been modified.
     * (Because of the restrictions already itemized, this
     * paragraph effectively applies only to the situation where both
     * arrays have component types that are reference types.)
     *
     * @param src      the source array.
     * @param srcPos   starting position in the source array.
     * @param dest     the destination array.
     * @param destPos  starting position in the destination data.
     * @param length   the number of array elements to be copied.
     * @throws IndexOutOfBoundsException  if copying would cause
     *             access of data outside array bounds.
     * @throws ArrayStoreException  if an element in the {@code src}
     *             array could not be stored into the {@code dest} array
     *             because of a type mismatch.
     * @throws NullPointerException if either {@code src} or
     *             {@code dest} is {@code null}.
     */
    @HotSpotIntrinsicCandidate
    public static native void arraycopy(Object src, int srcPos, Object dest, int destPos, int length);

    /**
     * Returns the same hash code for the given object as
     * would be returned by the default method hashCode(),
     * whether or not the given object's class overrides
     * hashCode().
     * The hash code for the null reference is zero.
     *
     * @param x object for which the hashCode is to be calculated
     * @return the hashCode
     */
    @HotSpotIntrinsicCandidate
    public static native int identityHashCode(Object x);

    /**
     * Returns the system-dependent line separator string.
     *
     * On UNIX systems, it returns {@code "\n"}.
     *
     * @return the system-dependent line separator string
     */
    public static String lineSeparator() {
        return lineSeparator;
    }

    private static String lineSeparator;

    /**
     * Terminates the currently running Java Virtual Machine. The
     * argument serves as a status code; by convention, a nonzero status
     * code indicates abnormal termination.
     *
     * This method calls the {@code exit} method in class
     * {@code Runtime}. This method never returns normally.
     *
     * The call {@code System.exit(n)} is effectively equivalent to
     * the call:
     * <blockquote><pre>
     * Runtime.getRuntime().exit(n)
     * </pre></blockquote>
     *
     * @param status   exit status.
     */
    public static void exit(int status) {
        Runtime.getRuntime().exit(status);
    }

    /**
     * Runs the garbage collector.
     *
     * Calling the {@code gc} method suggests that the Java Virtual
     * Machine expend effort toward recycling unused objects in order to
     * make the memory they currently occupy available for quick reuse.
     * When control returns from the method call, the Java Virtual
     * Machine has made a best effort to reclaim space from all discarded
     * objects.
     *
     * The call {@code System.gc()} is effectively equivalent to the
     * call:
     * <blockquote><pre>
     * Runtime.getRuntime().gc()
     * </pre></blockquote>
     */
    public static void gc() {
        Runtime.getRuntime().gc();
    }

    /**
     * Runs the finalization methods of any objects pending finalization.
     *
     * Calling this method suggests that the Java Virtual Machine expend
     * effort toward running the {@code finalize} methods of objects
     * that have been found to be discarded but whose {@code finalize}
     * methods have not yet been run. When control returns from the
     * method call, the Java Virtual Machine has made a best effort to
     * complete all outstanding finalizations.
     *
     * The call {@code System.runFinalization()} is effectively
     * equivalent to the call:
     * <blockquote><pre>
     * Runtime.getRuntime().runFinalization()
     * </pre></blockquote>
     */
    public static void runFinalization() {
        Runtime.getRuntime().runFinalization();
    }

    /**
     * Loads the native library specified by the {@code libname}
     * argument.  The {@code libname} argument must not contain any platform
     * specific prefix, file extension or path. If a native library
     * called {@code libname} is statically linked with the VM, then the
     * JNI_OnLoad_{@code libname} function exported by the library is invoked.
     * See the <a href="{@docRoot}/../specs/jni/index.html"> JNI Specification</a>
     * for more details.
     *
     * Otherwise, the libname argument is loaded from a system library
     * location and mapped to a native library image in an implementation-
     * dependent manner.
     *
     * @param libname   the name of the library.
     * @throws UnsatisfiedLinkError if either the libname argument
     *             contains a file path, the native library is not statically
     *             linked with the VM,  or the library cannot be mapped to a
     *             native library image by the host system.
     * @throws NullPointerException if {@code libname} is {@code null}
     */
    @CallerSensitive
    public static void loadLibrary(String libname) {
    }

    /**
     * Create PrintStream for stdout/err based on encoding.
     */
    private static PrintStream newPrintStream(FileOutputStream fos, String enc) {
       if (enc != null) {
            try {
                return new PrintStream(new BufferedOutputStream(fos, 128), true, enc);
            } catch (UnsupportedEncodingException uee) {}
        }
        return new PrintStream(new BufferedOutputStream(fos, 128), true);
    }

    /**
     * Initialize the system class.  Called after thread initialization.
     */
    private static void initPhase1() {
        lineSeparator = "\n"; // "line.separator"

        FileInputStream fdIn = new FileInputStream(FileDescriptor.in);
        FileOutputStream fdOut = new FileOutputStream(FileDescriptor.out);
        FileOutputStream fdErr = new FileOutputStream(FileDescriptor.err);
        setIn0(new BufferedInputStream(fdIn));
        setOut0(newPrintStream(fdOut, "UTF-8")); // "sun.stdout.encoding"
        setErr0(newPrintStream(fdErr, "UTF-8")); // "sun.stderr.encoding"

        // Setup Java signal handlers for HUP, TERM, and INT (where available).
        Terminator.setup();

        // The main thread is not added to its thread group in the same
        // way as other threads; we must do it ourselves here.
        Thread current = Thread.currentThread();
        current.getThreadGroup().add(current);

        // Subsystems that are invoked during initialization can invoke
        // VM.isBooted() in order to avoid doing things that should
        // wait until the VM is fully initialized. The initialization level
        // is incremented from 0 to 1 here to indicate the first phase of
        // initialization has completed.
        // IMPORTANT: Ensure that this remains the last initialization action!
        VM.initLevel(1);
    }

    /*
     * Invoked by VM.  Phase 2 module system initialization.
     * Only classes in java.base can be loaded in this phase.
     *
     * @param printToStderr print exceptions to stderr rather than stdout
     * @param printStackTrace print stack trace when exception occurs
     *
     * @return JNI_OK for success, JNI_ERR for failure
     */
    private static int initPhase2(boolean printToStderr, boolean printStackTrace) {
        // module system initialized
        VM.initLevel(2);

        return 0; // JNI_OK
    }

    /*
     * Invoked by VM.  Phase 3 is the final system initialization:
     * 1. set security manager
     * 2. set system class loader
     * 3. set TCCL
     *
     * This method must be called after the module system initialization.
     * The security manager and system class loader may be custom class from
     * the application classpath or modulepath.
     */
    private static void initPhase3() {
        // initializing the system class loader
        VM.initLevel(3);

        // system class loader initialized
        ClassLoader scl = ClassLoader.initSystemClassLoader();

        // set TCCL
        Thread.currentThread().setContextClassLoader(scl);

        // system is fully initialized
        VM.initLevel(4);
    }
}

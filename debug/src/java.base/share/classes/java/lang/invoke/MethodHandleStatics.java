package java.lang.invoke;

import jdk.internal.misc.Unsafe;

/**
 * This class consists exclusively of static names internal to the
 * method handle implementation.
 * Usage:  {@code import static java.lang.invoke.MethodHandleStatics.*}
 */
class MethodHandleStatics {
    private MethodHandleStatics() { } // do not instantiate

    static final Unsafe UNSAFE = Unsafe.getUnsafe();

    static final int COMPILE_THRESHOLD;
    static final int DONT_INLINE_THRESHOLD;
    static final int PROFILE_LEVEL;
    static final boolean PROFILE_GWT;
    static final int CUSTOMIZE_THRESHOLD;
    static final boolean VAR_HANDLE_GUARDS;
    static final int MAX_ARITY;

    static {
        COMPILE_THRESHOLD = 0; // "java.lang.invoke.MethodHandle.COMPILE_THRESHOLD"
        DONT_INLINE_THRESHOLD = 30; // "java.lang.invoke.MethodHandle.DONT_INLINE_THRESHOLD"
        PROFILE_LEVEL = 0; // "java.lang.invoke.MethodHandle.PROFILE_LEVEL"
        PROFILE_GWT = true; // "java.lang.invoke.MethodHandle.PROFILE_GWT"
        CUSTOMIZE_THRESHOLD = 127; // "java.lang.invoke.MethodHandle.CUSTOMIZE_THRESHOLD"
        VAR_HANDLE_GUARDS = true; // "java.lang.invoke.VarHandle.VAR_HANDLE_GUARDS"

        // Do not adjust this except for special platforms:
        MAX_ARITY = 255; // "java.lang.invoke.MethodHandleImpl.MAX_ARITY"

        if (CUSTOMIZE_THRESHOLD < -1 || CUSTOMIZE_THRESHOLD > 127) {
            throw newInternalError("CUSTOMIZE_THRESHOLD should be in [-1...127] range");
        }
    }

    // handy shared exception makers (they simplify the common case code)
    static InternalError newInternalError(String message) {
        return new InternalError(message);
    }
    static InternalError newInternalError(String message, Exception cause) {
        return new InternalError(message, cause);
    }
    static InternalError newInternalError(Exception cause) {
        return new InternalError(cause);
    }
    static RuntimeException newIllegalStateException(String message) {
        return new IllegalStateException(message);
    }
    static RuntimeException newIllegalStateException(String message, Object obj) {
        return new IllegalStateException(message(message, obj));
    }
    static RuntimeException newIllegalArgumentException(String message) {
        return new IllegalArgumentException(message);
    }
    static RuntimeException newIllegalArgumentException(String message, Object obj) {
        return new IllegalArgumentException(message(message, obj));
    }
    static RuntimeException newIllegalArgumentException(String message, Object obj, Object obj2) {
        return new IllegalArgumentException(message(message, obj, obj2));
    }

    /** Propagate unchecked exceptions and errors, but wrap anything checked and throw that instead. */
    static Error uncaughtException(Throwable ex) {
        if (ex instanceof Error)
            throw (Error) ex;
        if (ex instanceof RuntimeException)
            throw (RuntimeException) ex;
        throw new InternalError("uncaught exception", ex);
    }

    private static String message(String message, Object obj) {
        if (obj != null)
            message = message + ": " + obj;
        return message;
    }

    private static String message(String message, Object obj, Object obj2) {
        if (obj != null || obj2 != null)
            message = message + ": " + obj + ", " + obj2;
        return message;
    }
    static void rangeCheck2(int start, int end, int size) {
        if (0 > start || start > end || end > size)
            throw new IndexOutOfBoundsException(start+".."+end);
    }
    static int rangeCheck1(int index, int size) {
        if (0 > index || index >= size)
            throw new IndexOutOfBoundsException(index);
        return index;
    }
}

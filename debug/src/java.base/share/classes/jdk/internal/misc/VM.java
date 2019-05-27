package jdk.internal.misc;

import static java.lang.Thread.State.*;
import java.util.Map;

public class VM {
    // the init level when the VM is fully initialized
    private static final int JAVA_LANG_SYSTEM_INITED     = 1;
    private static final int MODULE_SYSTEM_INITED        = 2;
    private static final int SYSTEM_LOADER_INITIALIZING  = 3;
    private static final int SYSTEM_BOOTED               = 4;
    private static final int SYSTEM_SHUTDOWN             = 5;

    // 0, 1, 2, ...
    private static volatile int initLevel;
    private static final Object lock = new Object();

    /**
     * Sets the init level.
     */
    public static void initLevel(int value) {
        synchronized (lock) {
            if (value <= initLevel || value > SYSTEM_SHUTDOWN)
                throw new InternalError(String.str("Bad level: ", value));
            initLevel = value;
            lock.notifyAll();
        }
    }

    /**
     * Returns the current init level.
     */
    public static int initLevel() {
        return initLevel;
    }

    /**
     * Waits for the init level to get the given value.
     */
    public static void awaitInitLevel(int value) throws InterruptedException {
        synchronized (lock) {
            while (initLevel < value) {
                lock.wait();
            }
        }
    }

    /**
     * Returns {@code true} if the module system has been initialized.
     */
    public static boolean isModuleSystemInited() {
        return VM.initLevel() >= MODULE_SYSTEM_INITED;
    }

    /**
     * Returns {@code true} if the VM is fully initialized.
     */
    public static boolean isBooted() {
        return initLevel >= SYSTEM_BOOTED;
    }

    /**
     * Set shutdown state.  Shutdown completes when all registered shutdown
     * hooks have been run.
     */
    public static void shutdown() {
        initLevel(SYSTEM_SHUTDOWN);
    }

    /**
     * Returns {@code true} if the VM has been shutdown
     */
    public static boolean isShutdown() {
        return initLevel == SYSTEM_SHUTDOWN;
    }

    /**
     * Returns true if the given class loader is the bootstrap class loader
     * or the platform class loader.
     */
    public static boolean isSystemDomainLoader(ClassLoader loader) {
        return loader == null || loader == ClassLoader.getPlatformClassLoader();
    }

    /* Current count of objects pending for finalization */
    private static volatile int finalRefCount;

    /* Peak count of objects pending for finalization */
    private static volatile int peakFinalRefCount;

    /*
     * Gets the number of objects pending for finalization.
     *
     * @return the number of objects pending for finalization.
     */
    public static int getFinalRefCount() {
        return finalRefCount;
    }

    /*
     * Gets the peak number of objects pending for finalization.
     *
     * @return the peak number of objects pending for finalization.
     */
    public static int getPeakFinalRefCount() {
        return peakFinalRefCount;
    }

    /*
     * Add {@code n} to the objects pending for finalization count.
     *
     * @param n an integer value to be added to the objects pending
     * for finalization count
     */
    public static void addFinalRefCount(int n) {
        // The caller must hold lock to synchronize the update.

        finalRefCount += n;
        if (finalRefCount > peakFinalRefCount) {
            peakFinalRefCount = finalRefCount;
        }
    }

    /**
     * Returns Thread.State for the given threadStatus
     */
    public static Thread.State toThreadState(int threadStatus) {
        if ((threadStatus & JVMTI_THREAD_STATE_RUNNABLE) != 0) {
            return RUNNABLE;
        } else if ((threadStatus & JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER) != 0) {
            return BLOCKED;
        } else if ((threadStatus & JVMTI_THREAD_STATE_WAITING_INDEFINITELY) != 0) {
            return WAITING;
        } else if ((threadStatus & JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT) != 0) {
            return TIMED_WAITING;
        } else if ((threadStatus & JVMTI_THREAD_STATE_TERMINATED) != 0) {
            return TERMINATED;
        } else if ((threadStatus & JVMTI_THREAD_STATE_ALIVE) == 0) {
            return NEW;
        } else {
            return RUNNABLE;
        }
    }

    /* The threadStatus field is set by the VM at state transition
     * in the hotspot implementation. Its value is set according to
     * the JVM TI specification GetThreadState function.
     */
    private static final int JVMTI_THREAD_STATE_ALIVE = 0x0001;
    private static final int JVMTI_THREAD_STATE_TERMINATED = 0x0002;
    private static final int JVMTI_THREAD_STATE_RUNNABLE = 0x0004;
    private static final int JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER = 0x0400;
    private static final int JVMTI_THREAD_STATE_WAITING_INDEFINITELY = 0x0010;
    private static final int JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT = 0x0020;

    /**
     * Returns {@code true} if we are in a set UID program.
     */
    public static boolean isSetUID() {
        long uid = getuid();
        long euid = geteuid();
        long gid = getgid();
        long egid = getegid();
        return uid != euid  || gid != egid;
    }

    /**
     * Returns the real user ID of the calling process,
     * or -1 if the value is not available.
     */
    public static native long getuid();

    /**
     * Returns the effective user ID of the calling process,
     * or -1 if the value is not available.
     */
    public static native long geteuid();

    /**
     * Returns the real group ID of the calling process,
     * or -1 if the value is not available.
     */
    public static native long getgid();

    /**
     * Returns the effective group ID of the calling process,
     * or -1 if the value is not available.
     */
    public static native long getegid();

    /**
     * Get a nanosecond time stamp adjustment in the form of a single long.
     *
     * This value can be used to create an instant using
     * {@link java.time.Instant#ofEpochSecond(long, long)
     *  java.time.Instant.ofEpochSecond(offsetInSeconds,
     *  getNanoTimeAdjustment(offsetInSeconds))}.
     *
     * The value returned has the best resolution available to the JVM on
     * the current system.
     * This is usually down to microseconds - or tenth of microseconds -
     * depending on the OS/Hardware and the JVM implementation.
     *
     * @param offsetInSeconds The offset in seconds from which the nanosecond
     *        time stamp should be computed.
     *
     * @apiNote The offset should be recent enough - so that
     *         {@code offsetInSeconds} is within {@code +/- 2^32} seconds of the
     *         current UTC time. If the offset is too far off, {@code -1} will be
     *         returned. As such, {@code -1} must not be considered as a valid
     *         nano time adjustment, but as an exception value indicating
     *         that an offset closer to the current time should be used.
     *
     * @return A nanosecond time stamp adjustment in the form of a single long.
     *     If the offset is too far off the current time, this method returns -1.
     *     In that case, the caller should call this method again, passing a
     *     more accurate offset.
     */
    public static native long getNanoTimeAdjustment(long offsetInSeconds);

    /**
     * Returns the VM arguments for this runtime environment.
     *
     * @implNote
     * The HotSpot JVM processes the input arguments from multiple sources
     * in the following order:
     * 1. JAVA_TOOL_OPTIONS environment variable
     * 2. Options from JNI Invocation API
     * 3. _JAVA_OPTIONS environment variable
     *
     * If VM options file is specified via -XX:VMOptionsFile, the vm options
     * file is read and expanded in place of -XX:VMOptionFile option.
     */
    public static native String[] getRuntimeArguments();

    static {
        initialize();
    }
    private static native void initialize();
}

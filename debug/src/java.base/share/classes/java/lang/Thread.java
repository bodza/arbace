package java.lang;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.HashMap;

import jdk.internal.misc.TerminatingThreadLocal;
import jdk.internal.reflect.Reflection;

/**
 * A <i>thread</i> is a thread of execution in a program. The Java
 * Virtual Machine allows an application to have multiple threads of
 * execution running concurrently.
 *
 * Every thread has a priority. Threads with higher priority are
 * executed in preference to threads with lower priority. Each thread
 * may or may not also be marked as a daemon. When code running in
 * some thread creates a new {@code Thread} object, the new
 * thread has its priority initially set equal to the priority of the
 * creating thread, and is a daemon thread if and only if the
 * creating thread is a daemon.
 *
 * When a Java Virtual Machine starts up, there is usually a single
 * non-daemon thread (which typically calls the method named
 * {@code main} of some designated class). The Java Virtual
 * Machine continues to execute threads until either of the following
 * occurs:
 * <ul>
 * <li>The {@code exit} method of class {@code Runtime} has been
 *     called and the security manager has permitted the exit operation
 *     to take place.
 * <li>All threads that are not daemon threads have died, either by
 *     returning from the call to the {@code run} method or by
 *     throwing an exception that propagates beyond the {@code run}
 *     method.
 * </ul>
 *
 * There are two ways to create a new thread of execution. One is to
 * declare a class to be a subclass of {@code Thread}. This
 * subclass should override the {@code run} method of class
 * {@code Thread}. An instance of the subclass can then be
 * allocated and started. For example, a thread that computes primes
 * larger than a stated value could be written as follows:
 * <hr><blockquote><pre>
 *     class PrimeThread extends Thread {
 *         long minPrime;
 *         PrimeThread(long minPrime) {
 *             this.minPrime = minPrime;
 *         }
 *
 *         public void run() {
 *             // compute primes larger than minPrime
 *             &nbsp;.&nbsp;.&nbsp;.
 *         }
 *     }
 * </pre></blockquote><hr>
 *
 * The following code would then create a thread and start it running:
 * <blockquote><pre>
 *     PrimeThread p = new PrimeThread(143);
 *     p.start();
 * </pre></blockquote>
 *
 * The other way to create a thread is to declare a class that
 * implements the {@code Runnable} interface. That class then
 * implements the {@code run} method. An instance of the class can
 * then be allocated, passed as an argument when creating
 * {@code Thread}, and started. The same example in this other
 * style looks like the following:
 * <hr><blockquote><pre>
 *     class PrimeRun implements Runnable {
 *         long minPrime;
 *         PrimeRun(long minPrime) {
 *             this.minPrime = minPrime;
 *         }
 *
 *         public void run() {
 *             // compute primes larger than minPrime
 *             &nbsp;.&nbsp;.&nbsp;.
 *         }
 *     }
 * </pre></blockquote><hr>
 *
 * The following code would then create a thread and start it running:
 * <blockquote><pre>
 *     PrimeRun p = new PrimeRun(143);
 *     new Thread(p).start();
 * </pre></blockquote>
 *
 * Every thread has a name for identification purposes. More than
 * one thread may have the same name. If a name is not specified when
 * a thread is created, a new name is generated for it.
 *
 * Unless otherwise noted, passing a {@code null} argument to a constructor
 * or method in this class will cause a {@link NullPointerException} to be
 * thrown.
 */
public class Thread implements Runnable {
    /* Make sure registerNatives is the first thing <clinit> does. */
    private static native void registerNatives();
    static {
        registerNatives();
    }

    private volatile String name;
    private int priority;

    /* Whether or not the thread is a daemon thread. */
    private boolean daemon = false;

    /* Fields reserved for exclusive use by the JVM */
    private boolean stillborn = false;
    private long eetop;

    /* What will be run. */
    private Runnable target;

    /* The group of this thread */
    private ThreadGroup group;

    /* The context ClassLoader for this thread */
    private ClassLoader contextClassLoader;

    /* For autonumbering anonymous threads. */
    private static int threadInitNumber;
    private static synchronized int nextThreadNum() {
        return threadInitNumber++;
    }

    /* ThreadLocal values pertaining to this thread. This map is maintained
     * by the ThreadLocal class. */
    ThreadLocal.ThreadLocalMap threadLocals = null;

    /*
     * InheritableThreadLocal values pertaining to this thread. This map is
     * maintained by the InheritableThreadLocal class.
     */
    ThreadLocal.ThreadLocalMap inheritableThreadLocals = null;

    /*
     * The requested stack size for this thread, or 0 if the creator did
     * not specify a stack size.  It is up to the VM to do whatever it
     * likes with this number; some VMs will ignore it.
     */
    private final long stackSize;

    /*
     * JVM-private state that persists after native thread termination.
     */
    private long nativeParkEventPointer;

    /*
     * Thread ID
     */
    private final long tid;

    /* For generating thread ID */
    private static long threadSeqNumber;

    private static synchronized long nextThreadID() {
        return ++threadSeqNumber;
    }

    /*
     * Java thread status for tools, default indicates thread 'not yet started'
     */
    private volatile int threadStatus;

    /**
     * The minimum priority that a thread can have.
     */
    public static final int MIN_PRIORITY = 1;

    /**
     * The default priority that is assigned to a thread.
     */
    public static final int NORM_PRIORITY = 5;

    /**
     * The maximum priority that a thread can have.
     */
    public static final int MAX_PRIORITY = 10;

    /**
     * Returns a reference to the currently executing thread object.
     *
     * @return the currently executing thread.
     */
    // @HotSpotIntrinsicCandidate
    public static native Thread currentThread();

    /**
     * A hint to the scheduler that the current thread is willing to yield
     * its current use of a processor. The scheduler is free to ignore this hint.
     *
     * Yield is a heuristic attempt to improve relative progression
     * between threads that would otherwise over-utilise a CPU. Its use
     * should be combined with detailed profiling and benchmarking to
     * ensure that it actually has the desired effect.
     *
     * It is rarely appropriate to use this method. It may be useful
     * for debugging or testing purposes, where it may help to reproduce
     * bugs due to race conditions. It may also be useful when designing
     * concurrency control constructs such as the ones in the
     * {@link java.util.concurrent.locks} package.
     */
    public static native void yield();

    /**
     * Causes the currently executing thread to sleep (temporarily cease
     * execution) for the specified number of milliseconds, subject to
     * the precision and accuracy of system timers and schedulers. The thread
     * does not lose ownership of any monitors.
     *
     * @param millis
     *         the length of time to sleep in milliseconds
     *
     * @throws IllegalArgumentException
     *          if the value of {@code millis} is negative
     *
     * @throws InterruptedException
     *          if any thread has interrupted the current thread. The
     *          <i>interrupted status</i> of the current thread is
     *          cleared when this exception is thrown.
     */
    public static native void sleep(long millis) throws InterruptedException;

    /**
     * Causes the currently executing thread to sleep (temporarily cease
     * execution) for the specified number of milliseconds plus the specified
     * number of nanoseconds, subject to the precision and accuracy of system
     * timers and schedulers. The thread does not lose ownership of any
     * monitors.
     *
     * @param millis
     *         the length of time to sleep in milliseconds
     *
     * @param nanos
     *         {@code 0-999999} additional nanoseconds to sleep
     *
     * @throws IllegalArgumentException
     *          if the value of {@code millis} is negative, or the value of
     *          {@code nanos} is not in the range {@code 0-999999}
     *
     * @throws InterruptedException
     *          if any thread has interrupted the current thread. The
     *          <i>interrupted status</i> of the current thread is
     *          cleared when this exception is thrown.
     */
    public static void sleep(long millis, int nanos) throws InterruptedException {
        if (millis < 0) {
            throw new IllegalArgumentException("timeout value is negative");
        }

        if (nanos < 0 || nanos > 999999) {
            throw new IllegalArgumentException("nanosecond timeout value out of range");
        }

        if (nanos >= 500000 || (nanos != 0 && millis == 0)) {
            millis++;
        }

        sleep(millis);
    }

    /**
     * Indicates that the caller is momentarily unable to progress, until the
     * occurrence of one or more actions on the part of other activities. By
     * invoking this method within each iteration of a spin-wait loop construct,
     * the calling thread indicates to the runtime that it is busy-waiting.
     * The runtime may take action to improve the performance of invoking
     * spin-wait loop constructions.
     *
     * @apiNote
     * As an example consider a method in a class that spins in a loop until
     * some flag is set outside of that method. A call to the {@code onSpinWait}
     * method should be placed inside the spin loop.
     * <pre>{@code
     *     class EventHandler {
     *         volatile boolean eventNotificationNotReceived;
     *         void waitForEventAndHandleIt() {
     *             while (eventNotificationNotReceived) {
     *                 java.lang.Thread.onSpinWait();
     *             }
     *             readAndProcessEvent();
     *         }
     *
     *         void readAndProcessEvent() {
     *             // Read event from some source and process it
     *              . . .
     *         }
     *     }
     * }</pre>
     *
     * The code above would remain correct even if the {@code onSpinWait}
     * method was not called at all. However on some architectures the Java
     * Virtual Machine may issue the processor instructions to address such
     * code patterns in a more beneficial way.
     */
    // @HotSpotIntrinsicCandidate
    public static void onSpinWait() {}

    /**
     * Allocates a new {@code Thread} object so that it has {@code target}
     * as its run object, has the specified {@code name} as its name,
     * belongs to the thread group referred to by {@code group}, has
     * the specified {@code stackSize}, and inherits initial values for
     * {@linkplain InheritableThreadLocal inheritable thread-local} variables
     * if {@code inheritThreadLocals} is {@code true}.
     *
     * This constructor is identical to {@link
     * #Thread(ThreadGroup,Runnable,String,long)} with the added ability to
     * suppress, or not, the inheriting of initial values for inheritable
     * thread-local variables from the constructing thread. This allows for
     * finer grain control over inheritable thread-locals. Care must be taken
     * when passing a value of {@code false} for {@code inheritThreadLocals},
     * as it may lead to unexpected behavior if the new thread executes code
     * that expects a specific thread-local value to be inherited.
     *
     * Specifying a value of {@code true} for the {@code inheritThreadLocals}
     * parameter will cause this constructor to behave exactly like the
     * {@code Thread(ThreadGroup, Runnable, String, long)} constructor.
     *
     * @param g
     *         the thread group.
     * @param target
     *         the object whose {@code run} method is invoked when this thread
     *         is started. If {@code null}, this thread's run method is invoked.
     * @param name
     *         the name of the new thread
     * @param stackSize
     *         the desired stack size for the new thread, or zero to indicate
     *         that this parameter is to be ignored
     * @param inheritThreadLocals
     *         if {@code true}, inherit initial values for inheritable
     *         thread-locals from the constructing thread, otherwise no initial
     *         values are inherited
     */
    public Thread(ThreadGroup g, Runnable target, String name, long stackSize, boolean inheritThreadLocals) {
        if (name == null) {
            throw new NullPointerException("name cannot be null");
        }

        this.name = name;

        Thread parent = currentThread();
        if (g == null) {
            g = parent.getThreadGroup();
        }

        g.addUnstarted();

        this.group = g;
        this.daemon = parent.isDaemon();
        this.priority = parent.getPriority();
        this.contextClassLoader = parent.getContextClassLoader();
        this.target = target;
        setPriority(priority);
        if (inheritThreadLocals && parent.inheritableThreadLocals != null)
            this.inheritableThreadLocals = ThreadLocal.createInheritedMap(parent.inheritableThreadLocals);
        /* Stash the specified stack size in case the VM cares */
        this.stackSize = stackSize;

        /* Set thread ID */
        this.tid = nextThreadID();
    }

    /**
     * Throws CloneNotSupportedException as a Thread can not be meaningfully
     * cloned. Construct a new Thread instead.
     *
     * @throws CloneNotSupportedException
     *          always
     */
    // @Override
    protected Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
    }

    /**
     * Allocates a new {@code Thread} object. This constructor has the same
     * effect as {@linkplain #Thread(ThreadGroup,Runnable,String) Thread}
     * {@code (null, null, gname)}, where {@code gname} is a newly generated
     * name. Automatically generated names are of the form
     * {@code "Thread-"+}<i>n</i>, where <i>n</i> is an integer.
     */
    public Thread() {
        this(null, null, String.str("Thread-", nextThreadNum()), 0);
    }

    /**
     * Allocates a new {@code Thread} object. This constructor has the same
     * effect as {@linkplain #Thread(ThreadGroup,Runnable,String) Thread}
     * {@code (null, target, gname)}, where {@code gname} is a newly generated
     * name. Automatically generated names are of the form
     * {@code "Thread-"+}<i>n</i>, where <i>n</i> is an integer.
     *
     * @param target
     *         the object whose {@code run} method is invoked when this thread
     *         is started. If {@code null}, this classes {@code run} method does
     *         nothing.
     */
    public Thread(Runnable target) {
        this(null, target, String.str("Thread-", nextThreadNum()), 0);
    }

    /**
     * Allocates a new {@code Thread} object. This constructor has the same
     * effect as {@linkplain #Thread(ThreadGroup,Runnable,String) Thread}
     * {@code (group, target, gname)}, where {@code gname} is a newly generated
     * name. Automatically generated names are of the form
     * {@code "Thread-"+}<i>n</i>, where <i>n</i> is an integer.
     *
     * @param group
     *         the thread group.
     *
     * @param target
     *         the object whose {@code run} method is invoked when this thread
     *         is started. If {@code null}, this thread's run method is invoked.
     */
    public Thread(ThreadGroup group, Runnable target) {
        this(group, target, String.str("Thread-", nextThreadNum()), 0);
    }

    /**
     * Allocates a new {@code Thread} object. This constructor has the same
     * effect as {@linkplain #Thread(ThreadGroup,Runnable,String) Thread}
     * {@code (null, null, name)}.
     *
     * @param name
     *          the name of the new thread
     */
    public Thread(String name) {
        this(null, null, name, 0);
    }

    /**
     * Allocates a new {@code Thread} object. This constructor has the same
     * effect as {@linkplain #Thread(ThreadGroup,Runnable,String) Thread}
     * {@code (group, null, name)}.
     *
     * @param group
     *         the thread group.
     *
     * @param name
     *         the name of the new thread
     */
    public Thread(ThreadGroup group, String name) {
        this(group, null, name, 0);
    }

    /**
     * Allocates a new {@code Thread} object. This constructor has the same
     * effect as {@linkplain #Thread(ThreadGroup,Runnable,String) Thread}
     * {@code (null, target, name)}.
     *
     * @param target
     *         the object whose {@code run} method is invoked when this thread
     *         is started. If {@code null}, this thread's run method is invoked.
     *
     * @param name
     *         the name of the new thread
     */
    public Thread(Runnable target, String name) {
        this(null, target, name, 0);
    }

    /**
     * Allocates a new {@code Thread} object so that it has {@code target}
     * as its run object, has the specified {@code name} as its name,
     * and belongs to the thread group referred to by {@code group}.
     *
     * The priority of the newly created thread is set equal to the
     * priority of the thread creating it, that is, the currently running
     * thread. The method {@linkplain #setPriority setPriority} may be
     * used to change the priority to a new value.
     *
     * The newly created thread is initially marked as being a daemon
     * thread if and only if the thread creating it is currently marked
     * as a daemon thread. The method {@linkplain #setDaemon setDaemon}
     * may be used to change whether or not a thread is a daemon.
     *
     * @param group
     *         the thread group.
     *
     * @param target
     *         the object whose {@code run} method is invoked when this thread
     *         is started. If {@code null}, this thread's run method is invoked.
     *
     * @param name
     *         the name of the new thread
     */
    public Thread(ThreadGroup group, Runnable target, String name) {
        this(group, target, name, 0);
    }

    /**
     * Allocates a new {@code Thread} object so that it has {@code target}
     * as its run object, has the specified {@code name} as its name,
     * and belongs to the thread group referred to by {@code group}, and has
     * the specified <i>stack size</i>.
     *
     * This constructor is identical to {@link
     * #Thread(ThreadGroup,Runnable,String)} with the exception of the fact
     * that it allows the thread stack size to be specified.  The stack size
     * is the approximate number of bytes of address space that the virtual
     * machine is to allocate for this thread's stack.  <b>The effect of the
     * {@code stackSize} parameter, if any, is highly platform dependent.</b>
     *
     * On some platforms, specifying a higher value for the
     * {@code stackSize} parameter may allow a thread to achieve greater
     * recursion depth before throwing a {@link StackOverflowError}.
     * Similarly, specifying a lower value may allow a greater number of
     * threads to exist concurrently without throwing an {@link
     * OutOfMemoryError} (or other internal error).  The details of
     * the relationship between the value of the {@code stackSize} parameter
     * and the maximum recursion depth and concurrency level are
     * platform-dependent.  <b>On some platforms, the value of the
     * {@code stackSize} parameter may have no effect whatsoever.</b>
     *
     * The virtual machine is free to treat the {@code stackSize}
     * parameter as a suggestion.  If the specified value is unreasonably low
     * for the platform, the virtual machine may instead use some
     * platform-specific minimum value; if the specified value is unreasonably
     * high, the virtual machine may instead use some platform-specific
     * maximum.  Likewise, the virtual machine is free to round the specified
     * value up or down as it sees fit (or to ignore it completely).
     *
     * Specifying a value of zero for the {@code stackSize} parameter will
     * cause this constructor to behave exactly like the
     * {@code Thread(ThreadGroup, Runnable, String)} constructor.
     *
     * <i>Due to the platform-dependent nature of the behavior of this
     * constructor, extreme care should be exercised in its use.
     * The thread stack size necessary to perform a given computation will
     * likely vary from one JRE implementation to another.  In light of this
     * variation, careful tuning of the stack size parameter may be required,
     * and the tuning may need to be repeated for each JRE implementation on
     * which an application is to run.</i>
     *
     * Implementation note: Java platform implementers are encouraged to
     * document their implementation's behavior with respect to the
     * {@code stackSize} parameter.
     *
     * @param group
     *         the thread group.
     *
     * @param target
     *         the object whose {@code run} method is invoked when this thread
     *         is started. If {@code null}, this thread's run method is invoked.
     *
     * @param name
     *         the name of the new thread
     *
     * @param stackSize
     *         the desired stack size for the new thread, or zero to indicate
     *         that this parameter is to be ignored.
     */
    public Thread(ThreadGroup group, Runnable target, String name, long stackSize) {
        this(group, target, name, stackSize, true);
    }

    /**
     * Causes this thread to begin execution; the Java Virtual Machine
     * calls the {@code run} method of this thread.
     *
     * The result is that two threads are running concurrently: the
     * current thread (which returns from the call to the
     * {@code start} method) and the other thread (which executes its
     * {@code run} method).
     *
     * It is never legal to start a thread more than once.
     * In particular, a thread may not be restarted once it has completed
     * execution.
     *
     * @throws IllegalThreadStateException  if the thread was already started.
     */
    public synchronized void start() {
        /**
         * This method is not invoked for the main method thread or "system"
         * group threads created/set up by the VM. Any new functionality added
         * to this method in the future may have to also be added to the VM.
         *
         * A zero status value corresponds to state "NEW".
         */
        if (threadStatus != 0)
            throw new IllegalThreadStateException();

        /* Notify the group that this thread is about to be started
         * so that it can be added to the group's list of threads
         * and the group's unstarted count can be decremented. */
        group.add(this);

        boolean started = false;
        try {
            start0();
            started = true;
        } finally {
            try {
                if (!started) {
                    group.threadStartFailed(this);
                }
            } catch (Throwable ignore) {
                /* do nothing. If start0 threw a Throwable then
                  it will be passed up the call stack */
            }
        }
    }

    private native void start0();

    /**
     * If this thread was constructed using a separate
     * {@code Runnable} run object, then that
     * {@code Runnable} object's {@code run} method is called;
     * otherwise, this method does nothing and returns.
     *
     * Subclasses of {@code Thread} should override this method.
     */
    // @Override
    public void run() {
        if (target != null) {
            target.run();
        }
    }

    /**
     * This method is called by the system to give a Thread
     * a chance to clean up before it actually exits.
     */
    private void exit() {
        if (threadLocals != null && TerminatingThreadLocal.REGISTRY.isPresent()) {
            TerminatingThreadLocal.threadTerminated();
        }
        if (group != null) {
            group.threadTerminated(this);
            group = null;
        }
        /* Aggressively null out all reference fields: see bug 4006245 */
        target = null;
        /* Speed the release of some of these resources */
        threadLocals = null;
        inheritableThreadLocals = null;
        uncaughtExceptionHandler = null;
    }

    /**
     * Forces the thread to stop executing.
     *
     * The thread represented by this thread is forced to stop whatever
     * it is doing abnormally and to throw a newly created
     * {@code ThreadDeath} object as an exception.
     *
     * It is permitted to stop a thread that has not yet been started.
     * If the thread is eventually started, it immediately terminates.
     *
     * An application should not normally try to catch
     * {@code ThreadDeath} unless it must do some extraordinary
     * cleanup operation (note that the throwing of
     * {@code ThreadDeath} causes {@code finally} clauses of
     * {@code try} statements to be executed before the thread
     * officially dies).  If a {@code catch} clause catches a
     * {@code ThreadDeath} object, it is important to rethrow the
     * object so that the thread actually dies.
     *
     * The top-level error handler that reacts to otherwise uncaught
     * exceptions does not print out a message or otherwise notify the
     * application if the uncaught exception is an instance of
     * {@code ThreadDeath}.
     *
     * @deprecated This method is inherently unsafe.  Stopping a thread with
     *       Thread.stop causes it to unlock all of the monitors that it
     *       has locked (as a natural consequence of the unchecked
     *       {@code ThreadDeath} exception propagating up the stack).  If
     *       any of the objects previously protected by these monitors were in
     *       an inconsistent state, the damaged objects become visible to
     *       other threads, potentially resulting in arbitrary behavior.  Many
     *       uses of {@code stop} should be replaced by code that simply
     *       modifies some variable to indicate that the target thread should
     *       stop running.  The target thread should check this variable
     *       regularly, and return from its run method in an orderly fashion
     *       if the variable indicates that it is to stop running.  If the
     *       target thread waits for long periods (on a condition variable,
     *       for example), the {@code interrupt} method should be used to
     *       interrupt the wait.
     *       For more information, see
     *       <a href="{@docRoot}/java.base/java/lang/doc-files/threadPrimitiveDeprecation.html">Why
     *       are Thread.stop, Thread.suspend and Thread.resume Deprecated?</a>.
     */
    // @Deprecated(since="1.2")
    public final void stop() {
        // A zero status value corresponds to "NEW", it can't change to
        // not-NEW because we hold the lock.
        if (threadStatus != 0) {
            resume(); // Wake up thread if it was suspended; no-op otherwise
        }

        // The VM can handle all thread states
        stop0(new ThreadDeath());
    }

    /**
     * Interrupts this thread.
     *
     * If this thread is blocked in an invocation of the {@link
     * Object#wait() wait()}, {@link Object#wait(long) wait(long)}, or {@link
     * Object#wait(long, int) wait(long, int)} methods of the {@link Object}
     * class, or of the {@link #join()}, {@link #join(long)}, {@link
     * #join(long, int)}, {@link #sleep(long)}, or {@link #sleep(long, int)},
     * methods of this class, then its interrupt status will be cleared and it
     * will receive an {@link InterruptedException}.
     *
     * If none of the previous conditions hold then this thread's interrupt
     * status will be set.
     *
     * Interrupting a thread that is not alive need not have any effect.
     */
    public void interrupt() {
        // set interrupt status
        interrupt0();
    }

    /**
     * Tests whether the current thread has been interrupted.  The
     * <i>interrupted status</i> of the thread is cleared by this method.  In
     * other words, if this method were to be called twice in succession, the
     * second call would return false (unless the current thread were
     * interrupted again, after the first call had cleared its interrupted
     * status and before the second call had examined it).
     *
     * A thread interruption ignored because a thread was not alive
     * at the time of the interrupt will be reflected by this method
     * returning false.
     *
     * @return {@code true} if the current thread has been interrupted;
     *          {@code false} otherwise.
     */
    public static boolean interrupted() {
        return currentThread().isInterrupted(true);
    }

    /**
     * Tests whether this thread has been interrupted.  The <i>interrupted
     * status</i> of the thread is unaffected by this method.
     *
     * A thread interruption ignored because a thread was not alive
     * at the time of the interrupt will be reflected by this method
     * returning false.
     *
     * @return {@code true} if this thread has been interrupted;
     *          {@code false} otherwise.
     */
    public boolean isInterrupted() {
        return isInterrupted(false);
    }

    /**
     * Tests if some Thread has been interrupted.  The interrupted state
     * is reset or not based on the value of ClearInterrupted that is
     * passed.
     */
    // @HotSpotIntrinsicCandidate
    private native boolean isInterrupted(boolean ClearInterrupted);

    /**
     * Tests if this thread is alive. A thread is alive if it has
     * been started and has not yet died.
     *
     * @return {@code true} if this thread is alive;
     *          {@code false} otherwise.
     */
    public final native boolean isAlive();

    /**
     * Suspends this thread.
     *
     * If the thread is alive, it is suspended and makes no further
     * progress unless and until it is resumed.
     *
     * @deprecated This method has been deprecated, as it is
     *   inherently deadlock-prone.  If the target thread holds a lock on the
     *   monitor protecting a critical system resource when it is suspended, no
     *   thread can access this resource until the target thread is resumed. If
     *   the thread that would resume the target thread attempts to lock this
     *   monitor prior to calling {@code resume}, deadlock results.  Such
     *   deadlocks typically manifest themselves as "frozen" processes.
     *   For more information, see
     *   <a href="{@docRoot}/java.base/java/lang/doc-files/threadPrimitiveDeprecation.html">Why
     *   are Thread.stop, Thread.suspend and Thread.resume Deprecated?</a>.
     */
    // @Deprecated(since="1.2")
    public final void suspend() {
        suspend0();
    }

    /**
     * Resumes a suspended thread.
     *
     * If the thread is alive but suspended, it is resumed and is
     * permitted to make progress in its execution.
     *
     * @deprecated This method exists solely for use with {@link #suspend},
     *     which has been deprecated because it is deadlock-prone.
     *     For more information, see
     *     <a href="{@docRoot}/java.base/java/lang/doc-files/threadPrimitiveDeprecation.html">Why
     *     are Thread.stop, Thread.suspend and Thread.resume Deprecated?</a>.
     */
    // @Deprecated(since="1.2")
    public final void resume() {
        resume0();
    }

    /**
     * Changes the priority of this thread.
     *
     * Otherwise, the priority of this thread is set to the smaller of
     * the specified {@code newPriority} and the maximum permitted
     * priority of the thread's thread group.
     *
     * @param newPriority priority to set this thread to
     * @throws IllegalArgumentException  If the priority is not in the
     *               range {@code MIN_PRIORITY} to
     *               {@code MAX_PRIORITY}.
     */
    public final void setPriority(int newPriority) {
        ThreadGroup g;
        if (newPriority > MAX_PRIORITY || newPriority < MIN_PRIORITY) {
            throw new IllegalArgumentException();
        }
        if ((g = getThreadGroup()) != null) {
            if (newPriority > g.getMaxPriority()) {
                newPriority = g.getMaxPriority();
            }
            setPriority0(priority = newPriority);
        }
    }

    /**
     * Returns this thread's priority.
     *
     * @return this thread's priority.
     */
    public final int getPriority() {
        return priority;
    }

    /**
     * Changes the name of this thread to be equal to the argument {@code name}.
     *
     * @param name   the new name for this thread.
     */
    public final synchronized void setName(String name) {
        if (name == null) {
            throw new NullPointerException("name cannot be null");
        }

        this.name = name;
        if (threadStatus != 0) {
            setNativeName(name);
        }
    }

    /**
     * Returns this thread's name.
     *
     * @return this thread's name.
     */
    public final String getName() {
        return name;
    }

    /**
     * Returns the thread group to which this thread belongs.
     * This method returns null if this thread has died
     * (been stopped).
     *
     * @return this thread's thread group.
     */
    public final ThreadGroup getThreadGroup() {
        return group;
    }

    /**
     * Returns an estimate of the number of active threads in the current
     * thread's {@linkplain java.lang.ThreadGroup thread group} and its
     * subgroups. Recursively iterates over all subgroups in the current
     * thread's thread group.
     *
     * The value returned is only an estimate because the number of
     * threads may change dynamically while this method traverses internal
     * data structures, and might be affected by the presence of certain
     * system threads. This method is intended primarily for debugging
     * and monitoring purposes.
     *
     * @return an estimate of the number of active threads in the current
     *          thread's thread group and in any other thread group that
     *          has the current thread's thread group as an ancestor
     */
    public static int activeCount() {
        return currentThread().getThreadGroup().activeCount();
    }

    /**
     * Copies into the specified array every active thread in the current
     * thread's thread group and its subgroups. This method simply
     * invokes the {@link java.lang.ThreadGroup#enumerate(Thread[])}
     * method of the current thread's thread group.
     *
     * An application might use the {@linkplain #activeCount activeCount}
     * method to get an estimate of how big the array should be, however
     * <i>if the array is too short to hold all the threads, the extra threads
     * are silently ignored.</i>  If it is critical to obtain every active
     * thread in the current thread's thread group and its subgroups, the
     * invoker should verify that the returned int value is strictly less
     * than the length of {@code tarray}.
     *
     * Due to the inherent race condition in this method, it is recommended
     * that the method only be used for debugging and monitoring purposes.
     *
     * @param tarray
     *         an array into which to put the list of threads
     *
     * @return the number of threads put into the array
     */
    public static int enumerate(Thread tarray[]) {
        return currentThread().getThreadGroup().enumerate(tarray);
    }

    /**
     * Counts the number of stack frames in this thread. The thread must
     * be suspended.
     *
     * @return the number of stack frames in this thread.
     * @throws IllegalThreadStateException  if this thread is not
     *             suspended.
     * @deprecated The definition of this call depends on {@link #suspend},
     *             which is deprecated.  Further, the results of this call
     *             were never well-defined.
     *             This method is subject to removal in a future version of Java SE.
     */
    // @Deprecated(since="1.2", forRemoval=true)
    public native int countStackFrames();

    /**
     * Waits at most {@code millis} milliseconds for this thread to
     * die. A timeout of {@code 0} means to wait forever.
     *
     * This implementation uses a loop of {@code this.wait} calls
     * conditioned on {@code this.isAlive}. As a thread terminates the
     * {@code this.notifyAll} method is invoked. It is recommended that
     * applications not use {@code wait}, {@code notify}, or
     * {@code notifyAll} on {@code Thread} instances.
     *
     * @param millis
     *         the time to wait in milliseconds
     *
     * @throws IllegalArgumentException
     *          if the value of {@code millis} is negative
     *
     * @throws InterruptedException
     *          if any thread has interrupted the current thread. The
     *          <i>interrupted status</i> of the current thread is
     *          cleared when this exception is thrown.
     */
    public final synchronized void join(long millis) throws InterruptedException {
        long base = System.currentTimeMillis();
        long now = 0;

        if (millis < 0) {
            throw new IllegalArgumentException("timeout value is negative");
        }

        if (millis == 0) {
            while (isAlive()) {
                wait(0);
            }
        } else {
            while (isAlive()) {
                long delay = millis - now;
                if (delay <= 0) {
                    break;
                }
                wait(delay);
                now = System.currentTimeMillis() - base;
            }
        }
    }

    /**
     * Waits at most {@code millis} milliseconds plus
     * {@code nanos} nanoseconds for this thread to die.
     *
     * This implementation uses a loop of {@code this.wait} calls
     * conditioned on {@code this.isAlive}. As a thread terminates the
     * {@code this.notifyAll} method is invoked. It is recommended that
     * applications not use {@code wait}, {@code notify}, or
     * {@code notifyAll} on {@code Thread} instances.
     *
     * @param millis
     *         the time to wait in milliseconds
     *
     * @param nanos
     *         {@code 0-999999} additional nanoseconds to wait
     *
     * @throws IllegalArgumentException
     *          if the value of {@code millis} is negative, or the value
     *          of {@code nanos} is not in the range {@code 0-999999}
     *
     * @throws InterruptedException
     *          if any thread has interrupted the current thread. The
     *          <i>interrupted status</i> of the current thread is
     *          cleared when this exception is thrown.
     */
    public final synchronized void join(long millis, int nanos) throws InterruptedException {
        if (millis < 0) {
            throw new IllegalArgumentException("timeout value is negative");
        }

        if (nanos < 0 || nanos > 999999) {
            throw new IllegalArgumentException("nanosecond timeout value out of range");
        }

        if (nanos >= 500000 || (nanos != 0 && millis == 0)) {
            millis++;
        }

        join(millis);
    }

    /**
     * Waits for this thread to die.
     *
     * An invocation of this method behaves in exactly the same
     * way as the invocation
     *
     * <blockquote>
     * {@linkplain #join(long) join}{@code (0)}
     * </blockquote>
     *
     * @throws InterruptedException
     *          if any thread has interrupted the current thread. The
     *          <i>interrupted status</i> of the current thread is
     *          cleared when this exception is thrown.
     */
    public final void join() throws InterruptedException {
        join(0);
    }

    /**
     * Marks this thread as either a {@linkplain #isDaemon daemon} thread
     * or a user thread. The Java Virtual Machine exits when the only
     * threads running are all daemon threads.
     *
     * This method must be invoked before the thread is started.
     *
     * @param on
     *         if {@code true}, marks this thread as a daemon thread
     *
     * @throws IllegalThreadStateException
     *          if this thread is {@linkplain #isAlive alive}
     */
    public final void setDaemon(boolean on) {
        if (isAlive()) {
            throw new IllegalThreadStateException();
        }
        daemon = on;
    }

    /**
     * Tests if this thread is a daemon thread.
     *
     * @return {@code true} if this thread is a daemon thread;
     *          {@code false} otherwise.
     */
    public final boolean isDaemon() {
        return daemon;
    }

    /**
     * Returns a string representation of this thread, including the
     * thread's name, priority, and thread group.
     *
     * @return a string representation of this thread.
     */
    public String toString() {
        ThreadGroup group = getThreadGroup();
        if (group != null) {
            return String.str("Thread[", getName(), ",", getPriority(), ",", group.getName(), "]");
        } else {
            return String.str("Thread[", getName(), ",", getPriority(), ",", "", "]");
        }
    }

    /**
     * Returns the context {@code ClassLoader} for this thread. The context
     * {@code ClassLoader} is provided by the creator of the thread for use
     * by code running in this thread when loading classes and resources.
     * If not {@linkplain #setContextClassLoader set}, the default is the
     * {@code ClassLoader} context of the parent thread. The context
     * {@code ClassLoader} of the
     * primordial thread is typically set to the class loader used to load the
     * application.
     *
     * @return the context {@code ClassLoader} for this thread, or {@code null}
     *          indicating the system class loader (or, failing that, the
     *          bootstrap class loader)
     */
    // @CallerSensitive
    public ClassLoader getContextClassLoader() {
        return contextClassLoader;
    }

    /**
     * Sets the context ClassLoader for this Thread. The context
     * ClassLoader can be set when a thread is created, and allows
     * the creator of the thread to provide the appropriate class loader,
     * through {@code getContextClassLoader}, to code running in the thread
     * when loading classes and resources.
     *
     * @param cl
     *         the context ClassLoader for this Thread, or null  indicating the
     *         system class loader (or, failing that, the bootstrap class loader)
     */
    public void setContextClassLoader(ClassLoader cl) {
        contextClassLoader = cl;
    }

    /**
     * Returns {@code true} if and only if the current thread holds the
     * monitor lock on the specified object.
     *
     * This method is designed to allow a program to assert that
     * the current thread already holds a specified lock:
     * <pre>
     *     assert Thread.holdsLock(obj);
     * </pre>
     *
     * @param obj the object on which to test lock ownership
     * @throws NullPointerException if obj is {@code null}
     * @return {@code true} if the current thread holds the monitor lock on
     *         the specified object.
     */
    public static native boolean holdsLock(Object obj);

    private static native Thread[] getThreads();

    /**
     * Returns the identifier of this Thread.  The thread ID is a positive
     * {@code long} number generated when this thread was created.
     * The thread ID is unique and remains unchanged during its lifetime.
     * When a thread is terminated, this thread ID may be reused.
     *
     * @return this thread's ID.
     */
    public long getId() {
        return tid;
    }

    /**
     * A thread state.  A thread can be in one of the following states:
     * <ul>
     * <li>{@link #NEW}<br>
     *     A thread that has not yet started is in this state.</li>
     * <li>{@link #RUNNABLE}<br>
     *     A thread executing in the Java virtual machine is in this state.</li>
     * <li>{@link #BLOCKED}<br>
     *     A thread that is blocked waiting for a monitor lock
     *     is in this state.</li>
     * <li>{@link #WAITING}<br>
     *     A thread that is waiting indefinitely for another thread to
     *     perform a particular action is in this state.</li>
     * <li>{@link #TIMED_WAITING}<br>
     *     A thread that is waiting for another thread to perform an action
     *     for up to a specified waiting time is in this state.</li>
     * <li>{@link #TERMINATED}<br>
     *     A thread that has exited is in this state.</li>
     * </ul>
     *
     * A thread can be in only one state at a given point in time.
     * These states are virtual machine states which do not reflect
     * any operating system thread states.
     */
    public enum State {
        /**
         * Thread state for a thread which has not yet started.
         */
        NEW,

        /**
         * Thread state for a runnable thread.  A thread in the runnable
         * state is executing in the Java virtual machine but it may
         * be waiting for other resources from the operating system
         * such as processor.
         */
        RUNNABLE,

        /**
         * Thread state for a thread blocked waiting for a monitor lock.
         * A thread in the blocked state is waiting for a monitor lock
         * to enter a synchronized block/method or
         * reenter a synchronized block/method after calling
         * {@link Object#wait() Object.wait}.
         */
        BLOCKED,

        /**
         * Thread state for a waiting thread.
         * A thread is in the waiting state due to calling one of the
         * following methods:
         * <ul>
         *   <li>{@link Object#wait() Object.wait} with no timeout</li>
         *   <li>{@link #join() Thread.join} with no timeout</li>
         * </ul>
         *
         * A thread in the waiting state is waiting for another thread to
         * perform a particular action.
         *
         * For example, a thread that has called {@code Object.wait()}
         * on an object is waiting for another thread to call
         * {@code Object.notify()} or {@code Object.notifyAll()} on
         * that object. A thread that has called {@code Thread.join()}
         * is waiting for a specified thread to terminate.
         */
        WAITING,

        /**
         * Thread state for a waiting thread with a specified waiting time.
         * A thread is in the timed waiting state due to calling one of
         * the following methods with a specified positive waiting time:
         * <ul>
         *   <li>{@link #sleep Thread.sleep}</li>
         *   <li>{@link Object#wait(long) Object.wait} with timeout</li>
         *   <li>{@link #join(long) Thread.join} with timeout</li>
         * </ul>
         */
        TIMED_WAITING,

        /**
         * Thread state for a terminated thread.
         * The thread has completed execution.
         */
        TERMINATED;
    }

    /**
     * Returns the state of this thread.
     * This method is designed for use in monitoring of the system state,
     * not for synchronization control.
     *
     * @return this thread's state.
     */
    public State getState() {
        // get current thread state
        return jdk.internal.misc.VM.toThreadState(threadStatus);
    }

    // Added in JSR-166

    /**
     * Interface for handlers invoked when a {@code Thread} abruptly
     * terminates due to an uncaught exception.
     *
     * When a thread is about to terminate due to an uncaught exception
     * the Java Virtual Machine will query the thread for its
     * {@code UncaughtExceptionHandler} using
     * {@link #getUncaughtExceptionHandler} and will invoke the handler's
     * {@code uncaughtException} method, passing the thread and the
     * exception as arguments.
     * If a thread has not had its {@code UncaughtExceptionHandler}
     * explicitly set, then its {@code ThreadGroup} object acts as its
     * {@code UncaughtExceptionHandler}. If the {@code ThreadGroup} object
     * has no
     * special requirements for dealing with the exception, it can forward
     * the invocation to the {@linkplain #getDefaultUncaughtExceptionHandler
     * default uncaught exception handler}.
     */
    // @FunctionalInterface
    public interface UncaughtExceptionHandler {
        /**
         * Method invoked when the given thread terminates due to the
         * given uncaught exception.
         *
         * Any exception thrown by this method will be ignored by the
         * Java Virtual Machine.
         * @param t the thread
         * @param e the exception
         */
        void uncaughtException(Thread t, Throwable e);
    }

    // null unless explicitly set
    private volatile UncaughtExceptionHandler uncaughtExceptionHandler;

    // null unless explicitly set
    private static volatile UncaughtExceptionHandler defaultUncaughtExceptionHandler;

    /**
     * Set the default handler invoked when a thread abruptly terminates
     * due to an uncaught exception, and no other handler has been defined
     * for that thread.
     *
     * Uncaught exception handling is controlled first by the thread, then
     * by the thread's {@link ThreadGroup} object and finally by the default
     * uncaught exception handler. If the thread does not have an explicit
     * uncaught exception handler set, and the thread's thread group
     * (including parent thread groups)  does not specialize its
     * {@code uncaughtException} method, then the default handler's
     * {@code uncaughtException} method will be invoked.
     *
     * By setting the default uncaught exception handler, an application
     * can change the way in which uncaught exceptions are handled (such as
     * logging to a specific device, or file) for those threads that would
     * already accept whatever &quot;default&quot; behavior the system
     * provided.
     *
     * Note that the default uncaught exception handler should not usually
     * defer to the thread's {@code ThreadGroup} object, as that could cause
     * infinite recursion.
     *
     * @param eh the object to use as the default uncaught exception handler.
     * If {@code null} then there is no default handler.
     */
    public static void setDefaultUncaughtExceptionHandler(UncaughtExceptionHandler eh) {
         defaultUncaughtExceptionHandler = eh;
     }

    /**
     * Returns the default handler invoked when a thread abruptly terminates
     * due to an uncaught exception. If the returned value is {@code null},
     * there is no default.
     * @return the default uncaught exception handler for all threads
     */
    public static UncaughtExceptionHandler getDefaultUncaughtExceptionHandler() {
        return defaultUncaughtExceptionHandler;
    }

    /**
     * Returns the handler invoked when this thread abruptly terminates
     * due to an uncaught exception. If this thread has not had an
     * uncaught exception handler explicitly set then this thread's
     * {@code ThreadGroup} object is returned, unless this thread
     * has terminated, in which case {@code null} is returned.
     * @return the uncaught exception handler for this thread
     */
    public UncaughtExceptionHandler getUncaughtExceptionHandler() {
        return uncaughtExceptionHandler != null ? uncaughtExceptionHandler : group;
    }

    /**
     * Set the handler invoked when this thread abruptly terminates
     * due to an uncaught exception.
     *
     * A thread can take full control of how it responds to uncaught
     * exceptions by having its uncaught exception handler explicitly set.
     * If no such handler is set then the thread's {@code ThreadGroup}
     * object acts as its handler.
     * @param eh the object to use as this thread's uncaught exception
     * handler. If {@code null} then this thread has no explicit handler.
     */
    public void setUncaughtExceptionHandler(UncaughtExceptionHandler eh) {
        uncaughtExceptionHandler = eh;
    }

    /**
     * Dispatch an uncaught exception to the handler. This method is
     * intended to be called only by the JVM.
     */
    private void dispatchUncaughtException(Throwable e) {
        getUncaughtExceptionHandler().uncaughtException(this, e);
    }

    // The following three initially uninitialized fields are exclusively
    // managed by class java.util.concurrent.ThreadLocalRandom. These
    // fields are used to build the high-performance PRNGs in the
    // concurrent code, and we can not risk accidental false sharing.
    // Hence, the fields are isolated with @Contended.

    /** The current seed for a ThreadLocalRandom */
    // @Contended("tlr")
    long threadLocalRandomSeed;

    /** Probe hash value; nonzero if threadLocalRandomSeed initialized */
    // @Contended("tlr")
    int threadLocalRandomProbe;

    /** Secondary seed isolated from public ThreadLocalRandom sequence */
    // @Contended("tlr")
    int threadLocalRandomSecondarySeed;

    /* Some private helper methods */
    private native void setPriority0(int newPriority);
    private native void stop0(Object o);
    private native void suspend0();
    private native void resume0();
    private native void interrupt0();
    private native void setNativeName(String name);
}
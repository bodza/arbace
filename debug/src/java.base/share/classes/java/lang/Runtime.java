package java.lang;

import java.io.*;

import jdk.internal.reflect.Reflection;

/**
 * Every Java application has a single instance of class
 * {@code Runtime} that allows the application to interface with
 * the environment in which the application is running. The current
 * runtime can be obtained from the {@code getRuntime} method.
 *
 * An application cannot create its own instance of this class.
 */
public class Runtime {
    private static final Runtime currentRuntime = new Runtime();

    /**
     * Returns the runtime object associated with the current Java application.
     * Most of the methods of class {@code Runtime} are instance
     * methods and must be invoked with respect to the current runtime object.
     *
     * @return the {@code Runtime} object associated with the current
     *          Java application.
     */
    public static Runtime getRuntime() {
        return currentRuntime;
    }

    /** Don't let anyone else instantiate this class */
    private Runtime() {}

    /**
     * Terminates the currently running Java virtual machine by initiating its
     * shutdown sequence.  This method never returns normally.  The argument
     * serves as a status code; by convention, a nonzero status code indicates
     * abnormal termination.
     *
     * All registered {@linkplain #addShutdownHook shutdown hooks}, if any,
     * are started in some unspecified order and allowed to run concurrently
     * until they finish.  Once this is done the virtual machine
     * {@linkplain #halt halts}.
     *
     * If this method is invoked after all shutdown hooks have already
     * been run and the status is nonzero then this method halts the
     * virtual machine with the given status code. Otherwise, this method
     * blocks indefinitely.
     *
     * The {@link System#exit(int) System.exit} method is the
     * conventional and convenient means of invoking this method.
     *
     * @param status
     *         Termination status.  By convention, a nonzero status code
     *         indicates abnormal termination.
     */
    public void exit(int status) {
        Shutdown.exit(status);
    }

    /**
     * Registers a new virtual-machine shutdown hook.
     *
     * The Java virtual machine <i>shuts down</i> in response to two kinds
     * of events:
     *
     *   <ul>
     *   <li>The program <i>exits</i> normally, when the last non-daemon
     *   thread exits or when the {@link #exit exit} (equivalently,
     *   {@link System#exit(int) System.exit}) method is invoked, or
     *
     *   <li>The virtual machine is <i>terminated</i> in response to a
     *   user interrupt, such as typing {@code ^C}, or a system-wide event,
     *   such as user logoff or system shutdown.
     *   </ul>
     *
     * A <i>shutdown hook</i> is simply an initialized but unstarted
     * thread.  When the virtual machine begins its shutdown sequence it will
     * start all registered shutdown hooks in some unspecified order and let
     * them run concurrently.  When all the hooks have finished it will then
     * halt. Note that daemon threads will continue to run during the shutdown
     * sequence, as will non-daemon threads if shutdown was initiated by
     * invoking the {@link #exit exit} method.
     *
     * Once the shutdown sequence has begun it can be stopped only by
     * invoking the {@link #halt halt} method, which forcibly
     * terminates the virtual machine.
     *
     * Once the shutdown sequence has begun it is impossible to register a
     * new shutdown hook or de-register a previously-registered hook.
     * Attempting either of these operations will cause an
     * {@link IllegalStateException} to be thrown.
     *
     * Shutdown hooks run at a delicate time in the life cycle of a virtual
     * machine and should therefore be coded defensively.  They should, in
     * particular, be written to be thread-safe and to avoid deadlocks insofar
     * as possible.  They should also not rely blindly upon services that may
     * have registered their own shutdown hooks and therefore may themselves in
     * the process of shutting down.  Attempts to use other thread-based
     * services such as the AWT event-dispatch thread, for example, may lead to
     * deadlocks.
     *
     * Shutdown hooks should also finish their work quickly.  When a
     * program invokes {@link #exit exit} the expectation is
     * that the virtual machine will promptly shut down and exit.  When the
     * virtual machine is terminated due to user logoff or system shutdown the
     * underlying operating system may only allow a fixed amount of time in
     * which to shut down and exit.  It is therefore inadvisable to attempt any
     * user interaction or to perform a long-running computation in a shutdown hook.
     *
     * Uncaught exceptions are handled in shutdown hooks just as in any
     * other thread, by invoking the
     * {@link ThreadGroup#uncaughtException uncaughtException} method of the
     * thread's {@link ThreadGroup} object. The default implementation of this
     * method prints the exception's stack trace to {@link System#err} and
     * terminates the thread; it does not cause the virtual machine to exit or halt.
     *
     * In rare circumstances the virtual machine may <i>abort</i>, that is,
     * stop running without shutting down cleanly.  This occurs when the
     * virtual machine is terminated externally, for example with the
     * {@code SIGKILL} signal on Unix or the {@code TerminateProcess} call on
     * Microsoft Windows.  The virtual machine may also abort if a native
     * method goes awry by, for example, corrupting internal data structures or
     * attempting to access nonexistent memory.  If the virtual machine aborts
     * then no guarantee can be made about whether or not any shutdown hooks
     * will be run.
     *
     * @param hook
     *          An initialized but unstarted {@link Thread} object
     *
     * @throws IllegalArgumentException
     *          If the specified hook has already been registered,
     *          or if it can be determined that the hook is already running or
     *          has already been run
     *
     * @throws IllegalStateException
     *          If the virtual machine is already in the process
     *          of shutting down
     */
    public void addShutdownHook(Thread hook) {
        ApplicationShutdownHooks.add(hook);
    }

    /**
     * De-registers a previously-registered virtual-machine shutdown hook.
     *
     * @param hook the hook to remove
     * @return {@code true} if the specified hook had previously been
     * registered and was successfully de-registered, {@code false}
     * otherwise.
     *
     * @throws IllegalStateException
     *          If the virtual machine is already in the process of shutting
     *          down
     */
    public boolean removeShutdownHook(Thread hook) {
        return ApplicationShutdownHooks.remove(hook);
    }

    /**
     * Forcibly terminates the currently running Java virtual machine.  This
     * method never returns normally.
     *
     * This method should be used with extreme caution.  Unlike the
     * {@link #exit exit} method, this method does not cause shutdown
     * hooks to be started.  If the shutdown sequence has already been
     * initiated then this method does not wait for any running
     * shutdown hooks to finish their work.
     *
     * @param status
     *         Termination status. By convention, a nonzero status code
     *         indicates abnormal termination. If the {@link Runtime#exit exit}
     *         (equivalently, {@link System#exit(int) System.exit}) method
     *         has already been invoked then this status code
     *         will override the status code passed to that method.
     */
    public void halt(int status) {
        Shutdown.beforeHalt();
        Shutdown.halt(status);
    }

    /**
     * Returns the number of processors available to the Java virtual machine.
     *
     * This value may change during a particular invocation of the virtual
     * machine.  Applications that are sensitive to the number of available
     * processors should therefore occasionally poll this property and adjust
     * their resource usage appropriately.
     *
     * @return the maximum number of processors available to the virtual
     *          machine; never smaller than one
     */
    public native int availableProcessors();

    /**
     * Returns the amount of free memory in the Java Virtual Machine.
     * Calling the
     * {@code gc} method may result in increasing the value returned
     * by {@code freeMemory.}
     *
     * @return an approximation to the total amount of memory currently
     *          available for future allocated objects, measured in bytes.
     */
    public native long freeMemory();

    /**
     * Returns the total amount of memory in the Java virtual machine.
     * The value returned by this method may vary over time, depending on
     * the host environment.
     *
     * Note that the amount of memory required to hold an object of any
     * given type may be implementation-dependent.
     *
     * @return the total amount of memory currently available for current
     *          and future objects, measured in bytes.
     */
    public native long totalMemory();

    /**
     * Returns the maximum amount of memory that the Java virtual machine
     * will attempt to use.  If there is no inherent limit then the value
     * {@link java.lang.Long#MAX_VALUE} will be returned.
     *
     * @return the maximum amount of memory that the virtual machine will
     *          attempt to use, measured in bytes
     */
    public native long maxMemory();

    /**
     * Runs the garbage collector.
     * Calling this method suggests that the Java virtual machine expend
     * effort toward recycling unused objects in order to make the memory
     * they currently occupy available for quick reuse. When control
     * returns from the method call, the virtual machine has made
     * its best effort to recycle all discarded objects.
     *
     * The name {@code gc} stands for "garbage
     * collector". The virtual machine performs this recycling
     * process automatically as needed, in a separate thread, even if the
     * {@code gc} method is not invoked explicitly.
     *
     * The method {@link System#gc()} is the conventional and convenient
     * means of invoking this method.
     */
    public native void gc();

    /**
     * Runs the finalization methods of any objects pending finalization.
     * Calling this method suggests that the Java virtual machine expend
     * effort toward running the {@code finalize} methods of objects
     * that have been found to be discarded but whose {@code finalize}
     * methods have not yet been run. When control returns from the
     * method call, the virtual machine has made a best effort to
     * complete all outstanding finalizations.
     *
     * The virtual machine performs the finalization process
     * automatically as needed, in a separate thread, if the
     * {@code runFinalization} method is not invoked explicitly.
     *
     * The method {@link System#runFinalization()} is the conventional
     * and convenient means of invoking this method.
     */
    public void runFinalization() {
        Finalizer.runFinalization();
    }
}

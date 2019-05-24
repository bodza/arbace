package java.nio.channels.spi;

import java.nio.channels.*;
import java.io.IOException;
import java.util.Iterator;
import java.util.concurrent.*;

/**
 * Service-provider class for asynchronous channels.
 *
 * An asynchronous channel provider is a concrete subclass of this class that
 * has a zero-argument constructor and implements the abstract methods specified
 * below.  A given invocation of the Java virtual machine maintains a single
 * system-wide default provider instance, which is returned by the {@link
 * #provider() provider} method.  The first invocation of that method will locate
 * the default provider as specified below.
 *
 * All of the methods in this class are safe for use by multiple concurrent
 * threads.
 */
public abstract class AsynchronousChannelProvider {
    private AsynchronousChannelProvider(Void ignore) { }

    /**
     * Initializes a new instance of this class.
     */
    protected AsynchronousChannelProvider() {
        this(null);
    }

    // lazy initialization of default provider
    private static class ProviderHolder {
        static final AsynchronousChannelProvider provider = sun.nio.ch.DefaultAsynchronousChannelProvider.create();
    }

    /**
     * Returns the system-wide default asynchronous channel provider for this
     * invocation of the Java virtual machine.
     *
     * The first invocation of this method locates the default provider
     * object as follows:
     *
     * <ol>
     *   <li>If the system property
     *   {@code java.nio.channels.spi.AsynchronousChannelProvider} is defined
     *   then it is taken to be the fully-qualified name of a concrete provider class.
     *   The class is loaded and instantiated; if this process fails then an
     *   unspecified error is thrown.</li>
     *
     *   <li>If a provider class has been installed in a jar file that is
     *   visible to the system class loader, and that jar file contains a
     *   provider-configuration file named
     *   {@code java.nio.channels.spi.AsynchronousChannelProvider} in the resource
     *   directory {@code META-INF/services}, then the first class name
     *   specified in that file is taken.  The class is loaded and
     *   instantiated; if this process fails then an unspecified error is
     *   thrown.</li>
     *
     *   <li>Finally, if no provider has been specified by any of the above
     *   means then the system-default provider class is instantiated and the
     *   result is returned.</li>
     * </ol>
     *
     * Subsequent invocations of this method return the provider that was
     * returned by the first invocation.
     *
     * @return The system-wide default AsynchronousChannel provider
     */
    public static AsynchronousChannelProvider provider() {
        return ProviderHolder.provider;
    }

    /**
     * Constructs a new asynchronous channel group with a fixed thread pool.
     *
     * @param nThreads
     *          The number of threads in the pool
     * @param threadFactory
     *          The factory to use when creating new threads
     *
     * @return A new asynchronous channel group
     *
     * @throws IllegalArgumentException
     *          If {@code nThreads <= 0}
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract AsynchronousChannelGroup
        openAsynchronousChannelGroup(int nThreads, ThreadFactory threadFactory) throws IOException;

    /**
     * Constructs a new asynchronous channel group with the given thread pool.
     *
     * @param executor
     *          The thread pool
     * @param initialSize
     *          A value {@code >=0} or a negative value for implementation
     *          specific default
     *
     * @return A new asynchronous channel group
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract AsynchronousChannelGroup
        openAsynchronousChannelGroup(ExecutorService executor, int initialSize) throws IOException;

    /**
     * Opens an asynchronous server-socket channel.
     *
     * @param group
     *          The group to which the channel is bound, or {@code null} to
     *          bind to the default group
     *
     * @return The new channel
     *
     * @throws IllegalChannelGroupException
     *          If the provider that created the group differs from this provider
     * @throws ShutdownChannelGroupException
     *          The group is shutdown
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract AsynchronousServerSocketChannel openAsynchronousServerSocketChannel
        (AsynchronousChannelGroup group) throws IOException;

    /**
     * Opens an asynchronous socket channel.
     *
     * @param group
     *          The group to which the channel is bound, or {@code null} to
     *          bind to the default group
     *
     * @return The new channel
     *
     * @throws IllegalChannelGroupException
     *          If the provider that created the group differs from this provider
     * @throws ShutdownChannelGroupException
     *          The group is shutdown
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract AsynchronousSocketChannel openAsynchronousSocketChannel
        (AsynchronousChannelGroup group) throws IOException;
}

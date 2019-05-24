package java.nio.channels.spi;

import java.io.IOException;
import java.net.ProtocolFamily;
import java.nio.channels.*;
import java.util.Iterator;

/**
 * Service-provider class for selectors and selectable channels.
 *
 * A selector provider is a concrete subclass of this class that has a
 * zero-argument constructor and implements the abstract methods specified
 * below.  A given invocation of the Java virtual machine maintains a single
 * system-wide default provider instance, which is returned by the {@link
 * #provider() provider} method.  The first invocation of that method will locate
 * the default provider as specified below.
 *
 * The system-wide default provider is used by the static {@code open}
 * methods of the {@link java.nio.channels.DatagramChannel#open
 * DatagramChannel}, {@link java.nio.channels.Pipe#open Pipe}, {@link
 * java.nio.channels.Selector#open Selector}, {@link
 * java.nio.channels.ServerSocketChannel#open ServerSocketChannel}, and {@link
 * java.nio.channels.SocketChannel#open SocketChannel} classes.
 * A program may make use of a provider other than the default provider
 * by instantiating that provider and then directly invoking the {@code open}
 * methods defined in this class.
 *
 * All of the methods in this class are safe for use by multiple concurrent
 * threads.
 */
public abstract class SelectorProvider {
    private static final Object lock = new Object();
    private static SelectorProvider provider = null;

    private SelectorProvider(Void ignore) { }

    /**
     * Initializes a new instance of this class.
     */
    protected SelectorProvider() {
        this(null);
    }

    /**
     * Returns the system-wide default selector provider for this invocation of
     * the Java virtual machine.
     *
     * The first invocation of this method locates the default provider
     * object as follows:
     *
     * <ol>
     *   <li>If the system property
     *   {@code java.nio.channels.spi.SelectorProvider} is defined then it is
     *   taken to be the fully-qualified name of a concrete provider class.
     *   The class is loaded and instantiated; if this process fails then an
     *   unspecified error is thrown.</li>
     *
     *   <li>If a provider class has been installed in a jar file that is
     *   visible to the system class loader, and that jar file contains a
     *   provider-configuration file named
     *   {@code java.nio.channels.spi.SelectorProvider} in the resource
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
     * @return The system-wide default selector provider
     */
    public static SelectorProvider provider() {
        synchronized (lock) {
            if (provider == null)
                provider = sun.nio.ch.DefaultSelectorProvider.create();
            return provider;
        }
    }

    /**
     * Opens a datagram channel.
     *
     * @return The new channel
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract DatagramChannel openDatagramChannel() throws IOException;

    /**
     * Opens a datagram channel.
     *
     * @param family
     *          The protocol family
     *
     * @return A new datagram channel
     *
     * @throws UnsupportedOperationException
     *          If the specified protocol family is not supported
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract DatagramChannel openDatagramChannel(ProtocolFamily family) throws IOException;

    /**
     * Opens a pipe.
     *
     * @return The new pipe
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract Pipe openPipe() throws IOException;

    /**
     * Opens a selector.
     *
     * @return The new selector
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract AbstractSelector openSelector() throws IOException;

    /**
     * Opens a server-socket channel.
     *
     * @return The new channel
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract ServerSocketChannel openServerSocketChannel() throws IOException;

    /**
     * Opens a socket channel.
     *
     * @return The new channel
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public abstract SocketChannel openSocketChannel() throws IOException;
}

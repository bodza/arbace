package java.nio.channels;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.SocketOption;
import java.net.SocketAddress;
import java.nio.channels.spi.AbstractSelectableChannel;
import java.nio.channels.spi.SelectorProvider;

/**
 * A selectable channel for stream-oriented listening sockets.
 *
 * A server-socket channel is created by invoking the {@link #open() open}
 * method of this class.  It is not possible to create a channel for an arbitrary,
 * pre-existing {@link ServerSocket}. A newly-created server-socket channel is
 * open but not yet bound.  An attempt to invoke the {@link #accept() accept}
 * method of an unbound server-socket channel will cause a {@link NotYetBoundException}
 * to be thrown. A server-socket channel can be bound by invoking one of the
 * {@link #bind(java.net.SocketAddress,int) bind} methods defined by this class.
 *
 * Socket options are configured using the {@link #setOption(SocketOption,Object)
 * setOption} method. Server-socket channels support the following options:
 * <blockquote>
 * <table class="striped">
 * <caption style="display:none">Socket options</caption>
 * <thead>
 *   <tr>
 *     <th scope="col">Option Name</th>
 *     <th scope="col">Description</th>
 *   </tr>
 * </thead>
 * <tbody>
 *   <tr>
 *     <th scope="row"> {@link java.net.StandardSocketOptions#SO_RCVBUF SO_RCVBUF} </th>
 *     <td> The size of the socket receive buffer </td>
 *   </tr>
 *   <tr>
 *     <th scope="row"> {@link java.net.StandardSocketOptions#SO_REUSEADDR SO_REUSEADDR} </th>
 *     <td> Re-use address </td>
 *   </tr>
 * </tbody>
 * </table>
 * </blockquote>
 * Additional (implementation specific) options may also be supported.
 *
 * Server-socket channels are safe for use by multiple concurrent threads.
 */
public abstract class ServerSocketChannel extends AbstractSelectableChannel implements NetworkChannel {
    /**
     * Initializes a new instance of this class.
     *
     * @param provider
     *         The provider that created this channel
     */
    protected ServerSocketChannel(SelectorProvider provider) {
        super(provider);
    }

    /**
     * Opens a server-socket channel.
     *
     * The new channel is created by invoking the {@link
     * java.nio.channels.spi.SelectorProvider#openServerSocketChannel
     * openServerSocketChannel} method of the system-wide default {@link
     * java.nio.channels.spi.SelectorProvider} object.
     *
     * The new channel's socket is initially unbound; it must be bound to a
     * specific address via one of its socket's {@link
     * java.net.ServerSocket#bind(SocketAddress) bind} methods before
     * connections can be accepted.
     *
     * @return A new socket channel
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public static ServerSocketChannel open() throws IOException {
        return SelectorProvider.provider().openServerSocketChannel();
    }

    /**
     * Returns an operation set identifying this channel's supported
     * operations.
     *
     * Server-socket channels only support the accepting of new
     * connections, so this method returns {@link SelectionKey#OP_ACCEPT}.
     *
     * @return The valid-operation set
     */
    public final int validOps() {
        return SelectionKey.OP_ACCEPT;
    }

    // -- ServerSocket-specific operations --

    /**
     * Binds the channel's socket to a local address and configures the socket
     * to listen for connections.
     *
     * An invocation of this method is equivalent to the following:
     * <blockquote><pre>
     * bind(local, 0);
     * </pre></blockquote>
     *
     * @param local
     *          The local address to bind the socket, or {@code null} to bind
     *          to an automatically assigned socket address
     *
     * @return This channel
     *
     * @throws AlreadyBoundException               {@inheritDoc}
     * @throws UnsupportedAddressTypeException     {@inheritDoc}
     * @throws ClosedChannelException              {@inheritDoc}
     * @throws IOException                         {@inheritDoc}
     */
    public final ServerSocketChannel bind(SocketAddress local) throws IOException
    {
        return bind(local, 0);
    }

    /**
     * Binds the channel's socket to a local address and configures the socket to
     * listen for connections.
     *
     * This method is used to establish an association between the socket and
     * a local address. Once an association is established then the socket remains
     * bound until the channel is closed.
     *
     * The {@code backlog} parameter is the maximum number of pending
     * connections on the socket. Its exact semantics are implementation specific.
     * In particular, an implementation may impose a maximum length or may choose
     * to ignore the parameter altogther. If the {@code backlog} parameter has
     * the value {@code 0}, or a negative value, then an implementation specific
     * default is used.
     *
     * @param local
     *          The address to bind the socket, or {@code null} to bind to an
     *          automatically assigned socket address
     * @param backlog
     *          The maximum number of pending connections
     *
     * @return This channel
     *
     * @throws AlreadyBoundException
     *          If the socket is already bound
     * @throws UnsupportedAddressTypeException
     *          If the type of the given address is not supported
     * @throws ClosedChannelException
     *          If this channel is closed
     * @throws IOException
     *          If some other I/O error occurs
     */
    public abstract ServerSocketChannel bind(SocketAddress local, int backlog) throws IOException;

    /**
     * @throws UnsupportedOperationException           {@inheritDoc}
     * @throws IllegalArgumentException                {@inheritDoc}
     * @throws ClosedChannelException                  {@inheritDoc}
     * @throws IOException                             {@inheritDoc}
     */
    public abstract <T> ServerSocketChannel setOption(SocketOption<T> name, T value) throws IOException;

    /**
     * Retrieves a server socket associated with this channel.
     *
     * The returned object will not declare any public methods that are not
     * declared in the {@link java.net.ServerSocket} class.
     *
     * @return A server socket associated with this channel
     */
    public abstract ServerSocket socket();

    /**
     * Accepts a connection made to this channel's socket.
     *
     * If this channel is in non-blocking mode then this method will
     * immediately return {@code null} if there are no pending connections.
     * Otherwise it will block indefinitely until a new connection is available
     * or an I/O error occurs.
     *
     * The socket channel returned by this method, if any, will be in
     * blocking mode regardless of the blocking mode of this channel.
     *
     * @return The socket channel for the new connection,
     *          or {@code null} if this channel is in non-blocking mode
     *          and no connection is available to be accepted
     *
     * @throws ClosedChannelException
     *          If this channel is closed
     *
     * @throws AsynchronousCloseException
     *          If another thread closes this channel
     *          while the accept operation is in progress
     *
     * @throws ClosedByInterruptException
     *          If another thread interrupts the current thread
     *          while the accept operation is in progress, thereby
     *          closing the channel and setting the current thread's
     *          interrupt status
     *
     * @throws NotYetBoundException
     *          If this channel's socket has not yet been bound
     *
     * @throws IOException
     *          If some other I/O error occurs
     */
    public abstract SocketChannel accept() throws IOException;

    /**
     * {@inheritDoc}
     *
     * @return The {@code SocketAddress} that the socket is bound to, or the
     *          {@code SocketAddress} representing the loopback address if
     *          denied by the security manager, or {@code null} if the
     *          channel's socket is not bound
     *
     * @throws ClosedChannelException     {@inheritDoc}
     * @throws IOException                {@inheritDoc}
     */
    @Override
    public abstract SocketAddress getLocalAddress() throws IOException;
}

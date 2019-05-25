package java.net;

import java.io.FileDescriptor;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.nio.channels.ServerSocketChannel;
import java.util.Set;
import java.util.Collections;

/**
 * This class implements server sockets. A server socket waits for
 * requests to come in over the network. It performs some operation
 * based on that request, and then possibly returns a result to the requester.
 *
 * The actual work of the server socket is performed by an instance
 * of the {@code SocketImpl} class. An application can
 * change the socket factory that creates the socket
 * implementation to configure itself to create sockets
 * appropriate to the local firewall.
 */
public class ServerSocket implements java.io.Closeable {
    /**
     * Various states of this socket.
     */
    private boolean created = false;
    private boolean bound = false;
    private boolean closed = false;
    private Object closeLock = new Object();

    /**
     * The implementation of this Socket.
     */
    private SocketImpl impl;

    /**
     * Package-private constructor to create a ServerSocket associated with
     * the given SocketImpl.
     */
    ServerSocket(SocketImpl impl) {
        this.impl = impl;
        impl.setServerSocket(this);
    }

    /**
     * Creates an unbound server socket.
     *
     * @throws IOException IO error when opening the socket.
     */
    public ServerSocket() throws IOException {
        setImpl();
    }

    /**
     * Creates a server socket, bound to the specified port. A port number
     * of {@code 0} means that the port number is automatically
     * allocated, typically from an ephemeral port range. This port
     * number can then be retrieved by calling {@link #getLocalPort getLocalPort}.
     *
     * The maximum queue length for incoming connection indications (a
     * request to connect) is set to {@code 50}. If a connection
     * indication arrives when the queue is full, the connection is refused.
     *
     * If the application has specified a server socket factory, that
     * factory's {@code createSocketImpl} method is called to create
     * the actual socket implementation. Otherwise a "plain" socket is created.
     *
     * @param port  the port number, or {@code 0} to use a port
     *                   number that is automatically allocated.
     *
     * @throws IOException  if an I/O error occurs when opening the socket.
     * @throws IllegalArgumentException if the port parameter is outside
     *             the specified range of valid port values, which is between
     *             0 and 65535, inclusive.
     */
    public ServerSocket(int port) throws IOException {
        this(port, 50, null);
    }

    /**
     * Creates a server socket and binds it to the specified local port
     * number, with the specified backlog.
     * A port number of {@code 0} means that the port number is
     * automatically allocated, typically from an ephemeral port range.
     * This port number can then be retrieved by calling
     * {@link #getLocalPort getLocalPort}.
     *
     * The maximum queue length for incoming connection indications (a
     * request to connect) is set to the {@code backlog} parameter. If
     * a connection indication arrives when the queue is full, the
     * connection is refused.
     *
     * If the application has specified a server socket factory, that
     * factory's {@code createSocketImpl} method is called to create
     * the actual socket implementation. Otherwise a "plain" socket is created.
     *
     * The {@code backlog} argument is the requested maximum number of
     * pending connections on the socket. Its exact semantics are implementation
     * specific. In particular, an implementation may impose a maximum length
     * or may choose to ignore the parameter altogther. The value provided
     * should be greater than {@code 0}. If it is less than or equal to
     * {@code 0}, then an implementation specific default will be used.
     *
     * @param port     the port number, or {@code 0} to use a port
     *                      number that is automatically allocated.
     * @param backlog  requested maximum length of the queue of incoming
     *                      connections.
     *
     * @throws IOException  if an I/O error occurs when opening the socket.
     * @throws IllegalArgumentException if the port parameter is outside
     *             the specified range of valid port values, which is between
     *             0 and 65535, inclusive.
     */
    public ServerSocket(int port, int backlog) throws IOException {
        this(port, backlog, null);
    }

    /**
     * Create a server with the specified port, listen backlog, and
     * local IP address to bind to.  The <i>bindAddr</i> argument
     * can be used on a multi-homed host for a ServerSocket that
     * will only accept connect requests to one of its addresses.
     * If <i>bindAddr</i> is null, it will default accepting
     * connections on any/all local addresses.
     * The port must be between 0 and 65535, inclusive.
     * A port number of {@code 0} means that the port number is
     * automatically allocated, typically from an ephemeral port range.
     * This port number can then be retrieved by calling
     * {@link #getLocalPort getLocalPort}.
     *
     * The {@code backlog} argument is the requested maximum number of
     * pending connections on the socket. Its exact semantics are implementation
     * specific. In particular, an implementation may impose a maximum length
     * or may choose to ignore the parameter altogther. The value provided
     * should be greater than {@code 0}. If it is less than or equal to
     * {@code 0}, then an implementation specific default will be used.
     *
     * @param port  the port number, or {@code 0} to use a port
     *              number that is automatically allocated.
     * @param backlog requested maximum length of the queue of incoming
     *                connections.
     * @param bindAddr the local InetAddress the server will bind to
     *
     * @throws IOException if an I/O error occurs when opening the socket.
     * @throws IllegalArgumentException if the port parameter is outside
     *             the specified range of valid port values, which is between
     *             0 and 65535, inclusive.
     */
    public ServerSocket(int port, int backlog, InetAddress bindAddr) throws IOException {
        setImpl();
        if (port < 0 || port > 0xFFFF)
            throw new IllegalArgumentException("Port value out of range: " + port);
        if (backlog < 1)
          backlog = 50;
        try {
            bind(new InetSocketAddress(bindAddr, port), backlog);
        } catch (SecurityException e) {
            close();
            throw e;
        } catch (IOException e) {
            close();
            throw e;
        }
    }

    /**
     * Get the {@code SocketImpl} attached to this socket, creating
     * it if necessary.
     *
     * @return the {@code SocketImpl} attached to that ServerSocket.
     * @throws SocketException if creation fails.
     */
    SocketImpl getImpl() throws SocketException {
        if (!created)
            createImpl();
        return impl;
    }

    private void setImpl() {
        if (factory != null) {
            impl = factory.createSocketImpl();
        } else {
            impl = new SocksSocketImpl();
        }
        if (impl != null)
            impl.setServerSocket(this);
    }

    /**
     * Creates the socket implementation.
     *
     * @throws IOException if creation fails
     */
    void createImpl() throws SocketException {
        if (impl == null)
            setImpl();
        try {
            impl.create(true);
            created = true;
        } catch (IOException e) {
            throw new SocketException(e.getMessage());
        }
    }

    /**
     * Binds the {@code ServerSocket} to a specific address
     * (IP address and port number).
     *
     * If the address is {@code null}, then the system will pick up
     * an ephemeral port and a valid local address to bind the socket.
     *
     * @param endpoint        The IP address and port number to bind to.
     * @throws IOException if the bind operation fails, or if the socket
     *                     is already bound.
     * @throws IllegalArgumentException if endpoint is a
     *          SocketAddress subclass not supported by this socket
     */
    public void bind(SocketAddress endpoint) throws IOException {
        bind(endpoint, 50);
    }

    /**
     * Binds the {@code ServerSocket} to a specific address
     * (IP address and port number).
     *
     * If the address is {@code null}, then the system will pick up
     * an ephemeral port and a valid local address to bind the socket.
     *
     * The {@code backlog} argument is the requested maximum number of
     * pending connections on the socket. Its exact semantics are implementation
     * specific. In particular, an implementation may impose a maximum length
     * or may choose to ignore the parameter altogther. The value provided
     * should be greater than {@code 0}. If it is less than or equal to
     * {@code 0}, then an implementation specific default will be used.
     * @param endpoint        The IP address and port number to bind to.
     * @param backlog         requested maximum length of the queue of
     *                          incoming connections.
     * @throws IOException if the bind operation fails, or if the socket
     *                     is already bound.
     * @throws IllegalArgumentException if endpoint is a
     *          SocketAddress subclass not supported by this socket
     */
    public void bind(SocketAddress endpoint, int backlog) throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (isBound())
            throw new SocketException("Already bound");
        if (endpoint == null)
            endpoint = new InetSocketAddress(0);
        if (!(endpoint instanceof InetSocketAddress))
            throw new IllegalArgumentException("Unsupported address type");
        InetSocketAddress epoint = (InetSocketAddress) endpoint;
        if (epoint.isUnresolved())
            throw new SocketException("Unresolved address");
        if (backlog < 1)
          backlog = 50;
        try {
            getImpl().bind(epoint.getAddress(), epoint.getPort());
            getImpl().listen(backlog);
            bound = true;
        } catch (SecurityException e) {
            bound = false;
            throw e;
        } catch (IOException e) {
            bound = false;
            throw e;
        }
    }

    /**
     * Returns the local address of this server socket.
     *
     * If the socket was bound prior to being {@link #close closed},
     * then this method will continue to return the local address
     * after the socket is closed.
     *
     * @return the address to which this socket is bound,
     *          or the loopback address if denied by the security manager,
     *          or {@code null} if the socket is unbound.
     */
    public InetAddress getInetAddress() {
        if (!isBound())
            return null;
        try {
            InetAddress in = getImpl().getInetAddress();
            return in;
        } catch (SecurityException e) {
            return InetAddress.getLoopbackAddress();
        } catch (SocketException e) {
            // nothing
            // If we're bound, the impl has been created
            // so we shouldn't get here
        }
        return null;
    }

    /**
     * Returns the port number on which this socket is listening.
     *
     * If the socket was bound prior to being {@link #close closed},
     * then this method will continue to return the port number
     * after the socket is closed.
     *
     * @return the port number to which this socket is listening or
     *          -1 if the socket is not bound yet.
     */
    public int getLocalPort() {
        if (!isBound())
            return -1;
        try {
            return getImpl().getLocalPort();
        } catch (SocketException e) {
            // nothing
            // If we're bound, the impl has been created
            // so we shouldn't get here
        }
        return -1;
    }

    /**
     * Returns the address of the endpoint this socket is bound to.
     *
     * If the socket was bound prior to being {@link #close closed},
     * then this method will continue to return the address of the endpoint
     * after the socket is closed.
     *
     * @return a {@code SocketAddress} representing the local endpoint of
     *         this socket, or a {@code SocketAddress} representing the
     *         loopback address if denied by the security manager,
     *         or {@code null} if the socket is not bound yet.
     */
    public SocketAddress getLocalSocketAddress() {
        if (!isBound())
            return null;
        return new InetSocketAddress(getInetAddress(), getLocalPort());
    }

    /**
     * Listens for a connection to be made to this socket and accepts
     * it. The method blocks until a connection is made.
     *
     * @throws IOException  if an I/O error occurs when waiting for a
     *               connection.
     * @throws SocketTimeoutException if a timeout was previously set with setSoTimeout and
     *             the timeout has been reached.
     * @throws java.nio.channels.IllegalBlockingModeException
     *             if this socket has an associated channel, the channel is in
     *             non-blocking mode, and there is no connection ready to be
     *             accepted
     *
     * @return the new Socket
     */
    public Socket accept() throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (!isBound())
            throw new SocketException("Socket is not bound yet");
        Socket s = new Socket((SocketImpl) null);
        implAccept(s);
        return s;
    }

    /**
     * Subclasses of ServerSocket use this method to override accept()
     * to return their own subclass of socket.  So a FooServerSocket
     * will typically hand this method an <i>empty</i> FooSocket.  On
     * return from implAccept the FooSocket will be connected to a client.
     *
     * @param s the Socket
     * @throws java.nio.channels.IllegalBlockingModeException
     *         if this socket has an associated channel,
     *         and the channel is in non-blocking mode
     * @throws IOException if an I/O error occurs when waiting
     * for a connection.
     */
    protected final void implAccept(Socket s) throws IOException {
        SocketImpl si = null;
        try {
            if (s.impl == null)
              s.setImpl();
            else {
                s.impl.reset();
            }
            si = s.impl;
            s.impl = null;
            si.address = new InetAddress();
            si.fd = new FileDescriptor();
            getImpl().accept(si);
            SocketCleanable.register(si.fd); // raw fd has been set
        } catch (IOException e) {
            if (si != null)
                si.reset();
            s.impl = si;
            throw e;
        } catch (SecurityException e) {
            if (si != null)
                si.reset();
            s.impl = si;
            throw e;
        }
        s.impl = si;
        s.postAccept();
    }

    /**
     * Closes this socket.
     *
     * Any thread currently blocked in {@link #accept()} will throw
     * a {@link SocketException}.
     *
     * If this socket has an associated channel then the channel is closed
     * as well.
     *
     * @throws IOException  if an I/O error occurs when closing the socket.
     */
    public void close() throws IOException {
        synchronized (closeLock) {
            if (isClosed())
                return;
            if (created)
                impl.close();
            closed = true;
        }
    }

    /**
     * Returns the binding state of the ServerSocket.
     *
     * @return true if the ServerSocket successfully bound to an address
     */
    public boolean isBound() {
        return bound;
    }

    /**
     * Returns the closed state of the ServerSocket.
     *
     * @return true if the socket has been closed
     */
    public boolean isClosed() {
        synchronized (closeLock) {
            return closed;
        }
    }

    /**
     * Enable/disable {@link SocketOptions#SO_TIMEOUT SO_TIMEOUT} with the
     * specified timeout, in milliseconds.  With this option set to a non-zero
     * timeout, a call to accept() for this ServerSocket
     * will block for only this amount of time.  If the timeout expires,
     * a <b>java.net.SocketTimeoutException</b> is raised, though the
     * ServerSocket is still valid.  The option <b>must</b> be enabled
     * prior to entering the blocking operation to have effect.  The
     * timeout must be {@code > 0}.
     * A timeout of zero is interpreted as an infinite timeout.
     * @param timeout the specified timeout, in milliseconds
     * @throws SocketException if there is an error in
     * the underlying protocol, such as a TCP error.
     */
    public synchronized void setSoTimeout(int timeout) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_TIMEOUT, timeout);
    }

    /**
     * Retrieve setting for {@link SocketOptions#SO_TIMEOUT SO_TIMEOUT}.
     * 0 returns implies that the option is disabled (i.e., timeout of infinity).
     * @return the {@link SocketOptions#SO_TIMEOUT SO_TIMEOUT} value
     * @throws IOException if an I/O error occurs
     */
    public synchronized int getSoTimeout() throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        Object o = getImpl().getOption(SocketOptions.SO_TIMEOUT);
        /* extra type safety */
        if (o instanceof Integer) {
            return ((Integer) o).intValue();
        } else {
            return 0;
        }
    }

    /**
     * Enable/disable the {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR}
     * socket option.
     *
     * When a TCP connection is closed the connection may remain
     * in a timeout state for a period of time after the connection
     * is closed (typically known as the {@code TIME_WAIT} state
     * or {@code 2MSL} wait state).
     * For applications using a well known socket address or port
     * it may not be possible to bind a socket to the required
     * {@code SocketAddress} if there is a connection in the
     * timeout state involving the socket address or port.
     *
     * Enabling {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR} prior to
     * binding the socket using {@link #bind(SocketAddress)} allows the socket
     * to be bound even though a previous connection is in a timeout state.
     *
     * When a {@code ServerSocket} is created the initial setting
     * of {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR} is not defined.
     * Applications can use {@link #getReuseAddress()} to determine the initial
     * setting of {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR}.
     *
     * The behaviour when {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR} is
     * enabled or disabled after a socket is bound (See {@link #isBound()})
     * is not defined.
     *
     * @param on  whether to enable or disable the socket option
     * @throws SocketException if an error occurs enabling or
     *            disabling the {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR}
     *            socket option, or the socket is closed.
     */
    public void setReuseAddress(boolean on) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_REUSEADDR, Boolean.valueOf(on));
    }

    /**
     * Tests if {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR} is enabled.
     *
     * @return a {@code boolean} indicating whether or not
     *         {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR} is enabled.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public boolean getReuseAddress() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        return ((Boolean) (getImpl().getOption(SocketOptions.SO_REUSEADDR))).booleanValue();
    }

    /**
     * Returns the implementation address and implementation port of
     * this socket as a {@code String}.
     *
     * @return a string representation of this socket.
     */
    public String toString() {
        if (!isBound())
            return "ServerSocket[unbound]";
        InetAddress in = impl.getInetAddress();
        return "ServerSocket[addr=" + in + ",localport=" + impl.getLocalPort()  + "]";
    }

    void setBound() {
        bound = true;
    }

    void setCreated() {
        created = true;
    }

    /**
     * The factory for all server sockets.
     */
    private static SocketImplFactory factory = null;

    /**
     * Sets the server socket implementation factory for the
     * application. The factory can be specified only once.
     *
     * When an application creates a new server socket, the socket
     * implementation factory's {@code createSocketImpl} method is
     * called to create the actual socket implementation.
     *
     * Passing {@code null} to the method is a no-op unless the factory
     * was already set.
     *
     * @param fac   the desired factory.
     * @throws IOException  if an I/O error occurs when setting the
     *               socket factory.
     * @throws SocketException  if the factory has already been defined.
     */
    public static synchronized void setSocketFactory(SocketImplFactory fac) throws IOException {
        if (factory != null) {
            throw new SocketException("factory already defined");
        }
        factory = fac;
    }

    /**
     * Sets a default proposed value for the
     * {@link SocketOptions#SO_RCVBUF SO_RCVBUF} option for sockets
     * accepted from this {@code ServerSocket}. The value actually set
     * in the accepted socket must be determined by calling
     * {@link Socket#getReceiveBufferSize()} after the socket
     * is returned by {@link #accept()}.
     *
     * The value of {@link SocketOptions#SO_RCVBUF SO_RCVBUF} is used both to
     * set the size of the internal socket receive buffer, and to set the size
     * of the TCP receive window that is advertized to the remote peer.
     *
     * It is possible to change the value subsequently, by calling
     * {@link Socket#setReceiveBufferSize(int)}. However, if the application
     * wishes to allow a receive window larger than 64K bytes, as defined by RFC1323
     * then the proposed value must be set in the ServerSocket <b>before</b>
     * it is bound to a local address. This implies, that the ServerSocket must be
     * created with the no-argument constructor, then setReceiveBufferSize() must
     * be called and lastly the ServerSocket is bound to an address by calling bind().
     *
     * Failure to do this will not cause an error, and the buffer size may be set to the
     * requested value but the TCP receive window in sockets accepted from
     * this ServerSocket will be no larger than 64K bytes.
     *
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     *
     * @param size the size to which to set the receive buffer
     * size. This value must be greater than 0.
     *
     * @throws IllegalArgumentException if the
     * value is 0 or is negative.
     */
     public synchronized void setReceiveBufferSize(int size) throws SocketException {
        if (!(size > 0)) {
            throw new IllegalArgumentException("negative receive size");
        }
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_RCVBUF, size);
    }

    /**
     * Gets the value of the {@link SocketOptions#SO_RCVBUF SO_RCVBUF} option
     * for this {@code ServerSocket}, that is the proposed buffer size that
     * will be used for Sockets accepted from this {@code ServerSocket}.
     *
     * Note, the value actually set in the accepted socket is determined by
     * calling {@link Socket#getReceiveBufferSize()}.
     * @return the value of the {@link SocketOptions#SO_RCVBUF SO_RCVBUF}
     *         option for this {@code Socket}.
     * @throws SocketException if there is an error
     *            in the underlying protocol, such as a TCP error.
     */
    public synchronized int getReceiveBufferSize() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        int result = 0;
        Object o = getImpl().getOption(SocketOptions.SO_RCVBUF);
        if (o instanceof Integer) {
            result = ((Integer)o).intValue();
        }
        return result;
    }

    /**
     * Sets performance preferences for this ServerSocket.
     *
     * Sockets use the TCP/IP protocol by default.  Some implementations
     * may offer alternative protocols which have different performance
     * characteristics than TCP/IP.  This method allows the application to
     * express its own preferences as to how these tradeoffs should be made
     * when the implementation chooses from the available protocols.
     *
     * Performance preferences are described by three integers
     * whose values indicate the relative importance of short connection time,
     * low latency, and high bandwidth.  The absolute values of the integers
     * are irrelevant; in order to choose a protocol the values are simply
     * compared, with larger values indicating stronger preferences.  If the
     * application prefers short connection time over both low latency and high
     * bandwidth, for example, then it could invoke this method with the values
     * {@code (1, 0, 0)}.  If the application prefers high bandwidth above low
     * latency, and low latency above short connection time, then it could
     * invoke this method with the values {@code (0, 1, 2)}.
     *
     * Invoking this method after this socket has been bound
     * will have no effect. This implies that in order to use this capability
     * requires the socket to be created with the no-argument constructor.
     *
     * @param connectionTime
     *         An {@code int} expressing the relative importance of a short
     *         connection time
     *
     * @param latency
     *         An {@code int} expressing the relative importance of low
     *         latency
     *
     * @param bandwidth
     *         An {@code int} expressing the relative importance of high
     *         bandwidth
     */
    public void setPerformancePreferences(int connectionTime, int latency, int bandwidth) {
        /* Not implemented yet */
    }

    /**
     * Sets the value of a socket option.
     *
     * @param <T> The type of the socket option value
     * @param name The socket option
     * @param value The value of the socket option. A value of {@code null}
     *              may be valid for some options.
     * @return this ServerSocket
     *
     * @throws UnsupportedOperationException if the server socket does not
     *         support the option.
     *
     * @throws IllegalArgumentException if the value is not valid for
     *         the option.
     *
     * @throws IOException if an I/O error occurs, or if the socket is closed.
     *
     * @throws NullPointerException if name is {@code null}
     */
    public <T> ServerSocket setOption(SocketOption<T> name, T value) throws IOException {
        getImpl().setOption(name, value);
        return this;
    }

    /**
     * Returns the value of a socket option.
     *
     * @param <T> The type of the socket option value
     * @param name The socket option
     *
     * @return The value of the socket option.
     *
     * @throws UnsupportedOperationException if the server socket does not
     *         support the option.
     *
     * @throws IOException if an I/O error occurs, or if the socket is closed.
     *
     * @throws NullPointerException if name is {@code null}
     */
    public <T> T getOption(SocketOption<T> name) throws IOException {
        return getImpl().getOption(name);
    }

    private static Set<SocketOption<?>> options;
    private static boolean optionsSet = false;

    /**
     * Returns a set of the socket options supported by this server socket.
     *
     * This method will continue to return the set of options even after
     * the socket has been closed.
     *
     * @return A set of the socket options supported by this socket. This set
     *         may be empty if the socket's SocketImpl cannot be created.
     */
    public Set<SocketOption<?>> supportedOptions() {
        synchronized (ServerSocket.class) {
            if (optionsSet) {
                return options;
            }
            try {
                SocketImpl impl = getImpl();
                options = Collections.unmodifiableSet(impl.supportedOptions());
            } catch (IOException e) {
                options = Collections.emptySet();
            }
            optionsSet = true;
            return options;
        }
    }
}

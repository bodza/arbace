package java.net;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;
import java.util.Set;
import java.util.Collections;

/**
 * This class implements client sockets (also called just
 * "sockets"). A socket is an endpoint for communication
 * between two machines.
 *
 * The actual work of the socket is performed by an instance of the
 * {@code SocketImpl} class. An application, by changing
 * the socket factory that creates the socket implementation,
 * can configure itself to create sockets appropriate to the local
 * firewall.
 */
public class Socket implements java.io.Closeable {
    /**
     * Various states of this socket.
     */
    private boolean created = false;
    private boolean bound = false;
    private boolean connected = false;
    private boolean closed = false;
    private Object closeLock = new Object();
    private boolean shutIn = false;
    private boolean shutOut = false;

    /**
     * The implementation of this Socket.
     */
    SocketImpl impl;

    /**
     * Creates an unconnected socket, with the
     * system-default type of SocketImpl.
     */
    public Socket() {
        setImpl();
    }

    /**
     * Creates an unconnected Socket with a user-specified
     * SocketImpl.
     *
     * @param impl an instance of a <b>SocketImpl</b>
     * the subclass wishes to use on the Socket.
     *
     * @throws SocketException if there is an error in the underlying protocol,
     * such as a TCP error.
     */
    protected Socket(SocketImpl impl) throws SocketException {
        this.impl = impl;
        if (impl != null) {
            this.impl.setSocket(this);
        }
    }

    /**
     * Creates a stream socket and connects it to the specified port
     * number on the named host.
     *
     * If the specified host is {@code null} it is the equivalent of
     * specifying the address as
     * {@link java.net.InetAddress#getByName InetAddress.getByName}{@code (null)}.
     * In other words, it is equivalent to specifying an address of the
     * loopback interface.
     *
     * If the application has specified a server socket factory, that
     * factory's {@code createSocketImpl} method is called to create
     * the actual socket implementation. Otherwise a "plain" socket is created.
     *
     * @param host   the host name, or {@code null} for the loopback address.
     * @param port   the port number.
     *
     * @throws UnknownHostException if the IP address of
     * the host could not be determined.
     *
     * @throws IOException  if an I/O error occurs when creating the socket.
     * @throws IllegalArgumentException if the port parameter is outside
     *             the specified range of valid port values, which is between
     *             0 and 65535, inclusive.
     */
    public Socket(String host, int port) throws UnknownHostException, IOException {
        this(host != null ? new InetSocketAddress(host, port) : new InetSocketAddress(InetAddress.getByName(null), port), (SocketAddress) null, true);
    }

    /**
     * Creates a stream socket and connects it to the specified port
     * number at the specified IP address.
     *
     * If the application has specified a socket factory, that factory's
     * {@code createSocketImpl} method is called to create the
     * actual socket implementation. Otherwise a "plain" socket is created.
     *
     * @param address   the IP address.
     * @param port      the port number.
     * @throws IOException  if an I/O error occurs when creating the socket.
     * @throws IllegalArgumentException if the port parameter is outside
     *             the specified range of valid port values, which is between
     *             0 and 65535, inclusive.
     * @throws NullPointerException if {@code address} is null.
     */
    public Socket(InetAddress address, int port) throws IOException {
        this(address != null ? new InetSocketAddress(address, port) : null,
             (SocketAddress) null, true);
    }

    /**
     * Creates a socket and connects it to the specified remote host on
     * the specified remote port. The Socket will also bind() to the local
     * address and port supplied.
     *
     * If the specified host is {@code null} it is the equivalent of
     * specifying the address as
     * {@link java.net.InetAddress#getByName InetAddress.getByName}{@code (null)}.
     * In other words, it is equivalent to specifying an address of the
     * loopback interface.
     *
     * A local port number of {@code zero} will let the system pick up a
     * free port in the {@code bind} operation.
     *
     * @param host the name of the remote host, or {@code null} for the loopback address.
     * @param port the remote port
     * @param localAddr the local address the socket is bound to, or
     *        {@code null} for the {@code anyLocal} address.
     * @param localPort the local port the socket is bound to, or
     *        {@code zero} for a system selected free port.
     * @throws IOException  if an I/O error occurs when creating the socket.
     * @throws IllegalArgumentException if the port parameter or localPort
     *             parameter is outside the specified range of valid port values,
     *             which is between 0 and 65535, inclusive.
     */
    public Socket(String host, int port, InetAddress localAddr, int localPort) throws IOException {
        this(host != null ? new InetSocketAddress(host, port) : new InetSocketAddress(InetAddress.getByName(null), port), new InetSocketAddress(localAddr, localPort), true);
    }

    /**
     * Creates a socket and connects it to the specified remote address on
     * the specified remote port. The Socket will also bind() to the local
     * address and port supplied.
     *
     * If the specified local address is {@code null} it is the equivalent of
     * specifying the address as the AnyLocal address
     * (see {@link java.net.InetAddress#isAnyLocalAddress InetAddress.isAnyLocalAddress}{@code ()}).
     *
     * A local port number of {@code zero} will let the system pick up a
     * free port in the {@code bind} operation.
     *
     * @param address the remote address
     * @param port the remote port
     * @param localAddr the local address the socket is bound to, or
     *        {@code null} for the {@code anyLocal} address.
     * @param localPort the local port the socket is bound to or
     *        {@code zero} for a system selected free port.
     * @throws IOException  if an I/O error occurs when creating the socket.
     * @throws IllegalArgumentException if the port parameter or localPort
     *             parameter is outside the specified range of valid port values,
     *             which is between 0 and 65535, inclusive.
     * @throws NullPointerException if {@code address} is null.
     */
    public Socket(InetAddress address, int port, InetAddress localAddr, int localPort) throws IOException {
        this(address != null ? new InetSocketAddress(address, port) : null,
             new InetSocketAddress(localAddr, localPort), true);
    }

    /**
     * Creates a stream socket and connects it to the specified port
     * number on the named host.
     *
     * If the specified host is {@code null} it is the equivalent of
     * specifying the address as
     * {@link java.net.InetAddress#getByName InetAddress.getByName}{@code (null)}.
     * In other words, it is equivalent to specifying an address of the
     * loopback interface.
     *
     * If the stream argument is {@code true}, this creates a
     * stream socket. If the stream argument is {@code false}, it
     * creates a datagram socket.
     *
     * If the application has specified a server socket factory, that
     * factory's {@code createSocketImpl} method is called to create
     * the actual socket implementation. Otherwise a "plain" socket is created.
     *
     * If a UDP socket is used, TCP/IP related socket options will not apply.
     *
     * @param host     the host name, or {@code null} for the loopback address.
     * @param port     the port number.
     * @param stream   a {@code boolean} indicating whether this is
     *                      a stream socket or a datagram socket.
     * @throws IOException  if an I/O error occurs when creating the socket.
     * @throws IllegalArgumentException if the port parameter is outside
     *             the specified range of valid port values, which is between
     *             0 and 65535, inclusive.
     * @deprecated Use DatagramSocket instead for UDP transport.
     */
    // @Deprecated
    public Socket(String host, int port, boolean stream) throws IOException {
        this(host != null ? new InetSocketAddress(host, port) : new InetSocketAddress(InetAddress.getByName(null), port), (SocketAddress) null, stream);
    }

    /**
     * Creates a socket and connects it to the specified port number at
     * the specified IP address.
     *
     * If the stream argument is {@code true}, this creates a
     * stream socket. If the stream argument is {@code false}, it
     * creates a datagram socket.
     *
     * If the application has specified a server socket factory, that
     * factory's {@code createSocketImpl} method is called to create
     * the actual socket implementation. Otherwise a "plain" socket is created.
     *
     * If UDP socket is used, TCP/IP related socket options will not apply.
     *
     * @param host     the IP address.
     * @param port      the port number.
     * @param stream    if {@code true}, create a stream socket;
     *                       otherwise, create a datagram socket.
     * @throws IOException  if an I/O error occurs when creating the socket.
     * @throws IllegalArgumentException if the port parameter is outside
     *             the specified range of valid port values, which is between
     *             0 and 65535, inclusive.
     * @throws NullPointerException if {@code host} is null.
     * @deprecated Use DatagramSocket instead for UDP transport.
     */
    // @Deprecated
    public Socket(InetAddress host, int port, boolean stream) throws IOException {
        this(host != null ? new InetSocketAddress(host, port) : null,
             new InetSocketAddress(0), stream);
    }

    private Socket(SocketAddress address, SocketAddress localAddr, boolean stream) throws IOException {
        setImpl();

        // backward compatibility
        if (address == null)
            throw new NullPointerException();

        try {
            createImpl(stream);
            if (localAddr != null)
                bind(localAddr);
            connect(address);
        } catch (IOException | IllegalArgumentException | SecurityException e) {
            try {
                close();
            } catch (IOException ce) {
                e.addSuppressed(ce);
            }
            throw e;
        }
    }

    /**
     * Creates the socket implementation.
     *
     * @param stream a {@code boolean} value : {@code true} for a TCP socket,
     *               {@code false} for UDP.
     * @throws IOException if creation fails
     */
     void createImpl(boolean stream) throws SocketException {
        if (impl == null)
            setImpl();
        try {
            impl.create(stream);
            created = true;
        } catch (IOException e) {
            throw new SocketException(e.getMessage());
        }
    }

    /**
     * Sets impl to the system-default type of SocketImpl.
     */
    void setImpl() {
        if (factory != null) {
            impl = factory.createSocketImpl();
        } else {
            impl = new SocksSocketImpl();
        }
        if (impl != null)
            impl.setSocket(this);
    }

    /**
     * Get the {@code SocketImpl} attached to this socket, creating
     * it if necessary.
     *
     * @return the {@code SocketImpl} attached to that ServerSocket.
     * @throws SocketException if creation fails
     */
    SocketImpl getImpl() throws SocketException {
        if (!created)
            createImpl(true);
        return impl;
    }

    /**
     * Connects this socket to the server.
     *
     * @param endpoint the {@code SocketAddress}
     * @throws IOException if an error occurs during the connection
     * @throws IllegalArgumentException if endpoint is null or is a
     *          SocketAddress subclass not supported by this socket
     */
    public void connect(SocketAddress endpoint) throws IOException {
        connect(endpoint, 0);
    }

    /**
     * Connects this socket to the server with a specified timeout value.
     * A timeout of zero is interpreted as an infinite timeout. The connection
     * will then block until established or an error occurs.
     *
     * @param endpoint the {@code SocketAddress}
     * @param timeout  the timeout value to be used in milliseconds.
     * @throws IOException if an error occurs during the connection
     * @throws SocketTimeoutException if timeout expires before connecting
     * @throws IllegalArgumentException if endpoint is null or is a
     *          SocketAddress subclass not supported by this socket
     */
    public void connect(SocketAddress endpoint, int timeout) throws IOException {
        if (endpoint == null)
            throw new IllegalArgumentException("connect: The address can't be null");

        if (timeout < 0)
          throw new IllegalArgumentException("connect: timeout can't be negative");

        if (isClosed())
            throw new SocketException("Socket is closed");

        if (isConnected())
            throw new SocketException("already connected");

        if (!(endpoint instanceof InetSocketAddress))
            throw new IllegalArgumentException("Unsupported address type");

        InetSocketAddress epoint = (InetSocketAddress) endpoint;
        InetAddress addr = epoint.getAddress ();
        int port = epoint.getPort();
        checkAddress(addr, "connect");

        if (!created)
            createImpl(true);
        impl.connect(epoint, timeout);
        connected = true;
        /*
         * If the socket was not bound before the connect, it is now because
         * the kernel will have picked an ephemeral port & a local address
         */
        bound = true;
    }

    /**
     * Binds the socket to a local address.
     *
     * If the address is {@code null}, then the system will pick up
     * an ephemeral port and a valid local address to bind the socket.
     *
     * @param bindpoint the {@code SocketAddress} to bind to
     * @throws IOException if the bind operation fails, or if the socket
     *                     is already bound.
     * @throws IllegalArgumentException if bindpoint is a
     *          SocketAddress subclass not supported by this socket
     */
    public void bind(SocketAddress bindpoint) throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (isBound())
            throw new SocketException("Already bound");

        if (bindpoint != null && (!(bindpoint instanceof InetSocketAddress)))
            throw new IllegalArgumentException("Unsupported address type");
        InetSocketAddress epoint = (InetSocketAddress) bindpoint;
        if (epoint != null && epoint.isUnresolved())
            throw new SocketException("Unresolved address");
        if (epoint == null) {
            epoint = new InetSocketAddress(0);
        }
        InetAddress addr = epoint.getAddress();
        int port = epoint.getPort();
        checkAddress (addr, "bind");
        getImpl().bind (addr, port);
        bound = true;
    }

    private void checkAddress(InetAddress addr, String op) {
        if (addr == null) {
            return;
        }
        if (!(addr instanceof Inet4Address || addr instanceof Inet6Address)) {
            throw new IllegalArgumentException(String.str(op, ": invalid address type"));
        }
    }

    /**
     * set the flags after an accept() call.
     */
    final void postAccept() {
        connected = true;
        created = true;
        bound = true;
    }

    void setCreated() {
        created = true;
    }

    void setBound() {
        bound = true;
    }

    void setConnected() {
        connected = true;
    }

    /**
     * Returns the address to which the socket is connected.
     *
     * If the socket was connected prior to being {@link #close closed},
     * then this method will continue to return the connected address
     * after the socket is closed.
     *
     * @return the remote IP address to which this socket is connected,
     *          or {@code null} if the socket is not connected.
     */
    public InetAddress getInetAddress() {
        if (!isConnected())
            return null;
        try {
            return getImpl().getInetAddress();
        } catch (SocketException e) {
        }
        return null;
    }

    /**
     * Gets the local address to which the socket is bound.
     *
     * @return the local address to which the socket is bound,
     *         the loopback address if denied by the security manager, or
     *         the wildcard address if the socket is closed or not bound yet.
     */
    public InetAddress getLocalAddress() {
        // This is for backward compatibility
        if (!isBound())
            return InetAddress.anyLocalAddress();
        InetAddress in = null;
        try {
            in = (InetAddress) getImpl().getOption(SocketOptions.SO_BINDADDR);
            if (in.isAnyLocalAddress()) {
                in = InetAddress.anyLocalAddress();
            }
        } catch (SecurityException e) {
            in = InetAddress.getLoopbackAddress();
        } catch (Exception e) {
            in = InetAddress.anyLocalAddress(); // "0.0.0.0"
        }
        return in;
    }

    /**
     * Returns the remote port number to which this socket is connected.
     *
     * If the socket was connected prior to being {@link #close closed},
     * then this method will continue to return the connected port number
     * after the socket is closed.
     *
     * @return the remote port number to which this socket is connected, or
     *          0 if the socket is not connected yet.
     */
    public int getPort() {
        if (!isConnected())
            return 0;
        try {
            return getImpl().getPort();
        } catch (SocketException e) {
            // Shouldn't happen as we're connected
        }
        return -1;
    }

    /**
     * Returns the local port number to which this socket is bound.
     *
     * If the socket was bound prior to being {@link #close closed},
     * then this method will continue to return the local port number
     * after the socket is closed.
     *
     * @return the local port number to which this socket is bound or -1
     *          if the socket is not bound yet.
     */
    public int getLocalPort() {
        if (!isBound())
            return -1;
        try {
            return getImpl().getLocalPort();
        } catch (SocketException e) {
            // shouldn't happen as we're bound
        }
        return -1;
    }

    /**
     * Returns the address of the endpoint this socket is connected to, or
     * {@code null} if it is unconnected.
     *
     * If the socket was connected prior to being {@link #close closed},
     * then this method will continue to return the connected address
     * after the socket is closed.
     *
     * @return a {@code SocketAddress} representing the remote endpoint of this
     *         socket, or {@code null} if it is not connected yet.
     */
    public SocketAddress getRemoteSocketAddress() {
        if (!isConnected())
            return null;
        return new InetSocketAddress(getInetAddress(), getPort());
    }

    /**
     * Returns the address of the endpoint this socket is bound to.
     *
     * If a socket bound to an endpoint represented by an
     * {@code InetSocketAddress } is {@link #close closed},
     * then this method will continue to return an {@code InetSocketAddress}
     * after the socket is closed. In that case the returned
     * {@code InetSocketAddress}'s address is the
     * {@link InetAddress#isAnyLocalAddress wildcard} address
     * and its port is the local port that it was bound to.
     *
     * @return a {@code SocketAddress} representing the local endpoint of
     *         this socket, or a {@code SocketAddress} representing the
     *         loopback address if denied by the security manager, or
     *         {@code null} if the socket is not bound yet.
     */
    public SocketAddress getLocalSocketAddress() {
        if (!isBound())
            return null;
        return new InetSocketAddress(getLocalAddress(), getLocalPort());
    }

    /**
     * Returns an input stream for this socket.
     *
     * Under abnormal conditions the underlying connection may be
     * broken by the remote host or the network software (for example
     * a connection reset in the case of TCP connections). When a
     * broken connection is detected by the network software the
     * following applies to the returned input stream :-
     *
     * <ul>
     *   <li>The network software may discard bytes that are buffered
     *   by the socket. Bytes that aren't discarded by the network
     *   software can be read using {@link java.io.InputStream#read read}.
     *
     *   <li>If there are no bytes buffered on the socket, or all
     *   buffered bytes have been consumed by
     *   {@link java.io.InputStream#read read}, then all subsequent
     *   calls to {@link java.io.InputStream#read read} will throw an
     *   {@link java.io.IOException IOException}.
     *
     *   <li>If there are no bytes buffered on the socket, and the
     *   socket has not been closed using {@link #close close}, then
     *   {@link java.io.InputStream#available available} will
     *   return {@code 0}.
     * </ul>
     *
     * Closing the returned {@link java.io.InputStream InputStream}
     * will close the associated socket.
     *
     * @return an input stream for reading bytes from this socket.
     * @throws IOException  if an I/O error occurs when creating the
     *             input stream, the socket is closed, the socket is
     *             not connected, or the socket input has been shutdown
     *             using {@link #shutdownInput()}
     */
    public InputStream getInputStream() throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (!isConnected())
            throw new SocketException("Socket is not connected");
        if (isInputShutdown())
            throw new SocketException("Socket input is shutdown");
        return impl.getInputStream();
    }

    /**
     * Returns an output stream for this socket.
     *
     * Closing the returned {@link java.io.OutputStream OutputStream}
     * will close the associated socket.
     *
     * @return an output stream for writing bytes to this socket.
     * @throws IOException  if an I/O error occurs when creating the
     *               output stream or if the socket is not connected.
     */
    public OutputStream getOutputStream() throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (!isConnected())
            throw new SocketException("Socket is not connected");
        if (isOutputShutdown())
            throw new SocketException("Socket output is shutdown");
        return impl.getOutputStream();
    }

    /**
     * Enable/disable {@link SocketOptions#TCP_NODELAY TCP_NODELAY}
     * (disable/enable Nagle's algorithm).
     *
     * @param on {@code true} to enable TCP_NODELAY,
     * {@code false} to disable.
     *
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public void setTcpNoDelay(boolean on) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.TCP_NODELAY, Boolean.valueOf(on));
    }

    /**
     * Tests if {@link SocketOptions#TCP_NODELAY TCP_NODELAY} is enabled.
     *
     * @return a {@code boolean} indicating whether or not
     *         {@link SocketOptions#TCP_NODELAY TCP_NODELAY} is enabled.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public boolean getTcpNoDelay() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        return ((Boolean) getImpl().getOption(SocketOptions.TCP_NODELAY)).booleanValue();
    }

    /**
     * Enable/disable {@link SocketOptions#SO_LINGER SO_LINGER} with the
     * specified linger time in seconds. The maximum timeout value is platform
     * specific.
     *
     * The setting only affects socket close.
     *
     * @param on     whether or not to linger on.
     * @param linger how long to linger for, if on is true.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     * @throws IllegalArgumentException if the linger value is negative.
     */
    public void setSoLinger(boolean on, int linger) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (!on) {
            getImpl().setOption(SocketOptions.SO_LINGER, on);
        } else {
            if (linger < 0) {
                throw new IllegalArgumentException("invalid value for SO_LINGER");
            }
            if (linger > 65535)
                linger = 65535;
            getImpl().setOption(SocketOptions.SO_LINGER, linger);
        }
    }

    /**
     * Returns setting for {@link SocketOptions#SO_LINGER SO_LINGER}.
     * -1 returns implies that the
     * option is disabled.
     *
     * The setting only affects socket close.
     *
     * @return the setting for {@link SocketOptions#SO_LINGER SO_LINGER}.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public int getSoLinger() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        Object o = getImpl().getOption(SocketOptions.SO_LINGER);
        if (o instanceof Integer) {
            return ((Integer) o).intValue();
        } else {
            return -1;
        }
    }

    /**
     * Send one byte of urgent data on the socket. The byte to be sent is the lowest eight
     * bits of the data parameter. The urgent byte is
     * sent after any preceding writes to the socket OutputStream
     * and before any future writes to the OutputStream.
     * @param data The byte of data to send
     * @throws IOException if there is an error
     *  sending the data.
     */
    public void sendUrgentData(int data) throws IOException {
        if (!getImpl().supportsUrgentData ()) {
            throw new SocketException("Urgent data not supported");
        }
        getImpl().sendUrgentData (data);
    }

    /**
     * Enable/disable {@link SocketOptions#SO_OOBINLINE SO_OOBINLINE}
     * (receipt of TCP urgent data)
     *
     * By default, this option is disabled and TCP urgent data received on a
     * socket is silently discarded. If the user wishes to receive urgent data, then
     * this option must be enabled. When enabled, urgent data is received
     * inline with normal data.
     *
     * Note, only limited support is provided for handling incoming urgent
     * data. In particular, no notification of incoming urgent data is provided
     * and there is no capability to distinguish between normal data and urgent
     * data unless provided by a higher level protocol.
     *
     * @param on {@code true} to enable
     *           {@link SocketOptions#SO_OOBINLINE SO_OOBINLINE},
     *           {@code false} to disable.
     *
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public void setOOBInline(boolean on) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_OOBINLINE, Boolean.valueOf(on));
    }

    /**
     * Tests if {@link SocketOptions#SO_OOBINLINE SO_OOBINLINE} is enabled.
     *
     * @return a {@code boolean} indicating whether or not
     *         {@link SocketOptions#SO_OOBINLINE SO_OOBINLINE}is enabled.
     *
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public boolean getOOBInline() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        return ((Boolean) getImpl().getOption(SocketOptions.SO_OOBINLINE)).booleanValue();
    }

    /**
     *  Enable/disable {@link SocketOptions#SO_TIMEOUT SO_TIMEOUT}
     *  with the specified timeout, in milliseconds. With this option set
     *  to a non-zero timeout, a read() call on the InputStream associated with
     *  this Socket will block for only this amount of time.  If the timeout
     *  expires, a <b>java.net.SocketTimeoutException</b> is raised, though the
     *  Socket is still valid. The option <b>must</b> be enabled
     *  prior to entering the blocking operation to have effect. The
     *  timeout must be {@code > 0}.
     *  A timeout of zero is interpreted as an infinite timeout.
     *
     * @param timeout the specified timeout, in milliseconds.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public synchronized void setSoTimeout(int timeout) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (timeout < 0)
          throw new IllegalArgumentException("timeout can't be negative");

        getImpl().setOption(SocketOptions.SO_TIMEOUT, timeout);
    }

    /**
     * Returns setting for {@link SocketOptions#SO_TIMEOUT SO_TIMEOUT}.
     * 0 returns implies that the option is disabled (i.e., timeout of infinity).
     *
     * @return the setting for {@link SocketOptions#SO_TIMEOUT SO_TIMEOUT}
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public synchronized int getSoTimeout() throws SocketException {
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
     * Sets the {@link SocketOptions#SO_SNDBUF SO_SNDBUF} option to the
     * specified value for this {@code Socket}.
     * The {@link SocketOptions#SO_SNDBUF SO_SNDBUF} option is used by the
     * platform's networking code as a hint for the size to set the underlying
     * network I/O buffers.
     *
     * Because {@link SocketOptions#SO_SNDBUF SO_SNDBUF} is a hint,
     * applications that want to verify what size the buffers were set to
     * should call {@link #getSendBufferSize()}.
     *
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     *
     * @param size the size to which to set the send buffer
     * size. This value must be greater than 0.
     *
     * @throws IllegalArgumentException if the
     * value is 0 or is negative.
     */
    public synchronized void setSendBufferSize(int size) throws SocketException {
        if (!(size > 0)) {
            throw new IllegalArgumentException("negative send size");
        }
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_SNDBUF, size);
    }

    /**
     * Get value of the {@link SocketOptions#SO_SNDBUF SO_SNDBUF} option
     * for this {@code Socket}, that is the buffer size used by the platform
     * for output on this {@code Socket}.
     * @return the value of the {@link SocketOptions#SO_SNDBUF SO_SNDBUF}
     *         option for this {@code Socket}.
     *
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public synchronized int getSendBufferSize() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        int result = 0;
        Object o = getImpl().getOption(SocketOptions.SO_SNDBUF);
        if (o instanceof Integer) {
            result = ((Integer)o).intValue();
        }
        return result;
    }

    /**
     * Sets the {@link SocketOptions#SO_RCVBUF SO_RCVBUF} option to the
     * specified value for this {@code Socket}. The
     * {@link SocketOptions#SO_RCVBUF SO_RCVBUF} option is
     * used by the platform's networking code as a hint for the size to set
     * the underlying network I/O buffers.
     *
     * Increasing the receive buffer size can increase the performance of
     * network I/O for high-volume connection, while decreasing it can
     * help reduce the backlog of incoming data.
     *
     * Because {@link SocketOptions#SO_RCVBUF SO_RCVBUF} is a hint,
     * applications that want to verify what size the buffers were set to
     * should call {@link #getReceiveBufferSize()}.
     *
     * The value of {@link SocketOptions#SO_RCVBUF SO_RCVBUF} is also used
     * to set the TCP receive window that is advertized to the remote peer.
     * Generally, the window size can be modified at any time when a socket is
     * connected. However, if a receive window larger than 64K is required then
     * this must be requested <b>before</b> the socket is connected to the
     * remote peer. There are two cases to be aware of:
     * <ol>
     * <li>For sockets accepted from a ServerSocket, this must be done by calling
     * {@link ServerSocket#setReceiveBufferSize(int)} before the ServerSocket
     * is bound to a local address.</li>
     * <li>For client sockets, setReceiveBufferSize() must be called before
     * connecting the socket to its remote peer.</li></ol>
     * @param size the size to which to set the receive buffer
     * size. This value must be greater than 0.
     *
     * @throws IllegalArgumentException if the value is 0 or is
     * negative.
     *
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public synchronized void setReceiveBufferSize(int size) throws SocketException {
        if (size <= 0) {
            throw new IllegalArgumentException("invalid receive size");
        }
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_RCVBUF, size);
    }

    /**
     * Gets the value of the {@link SocketOptions#SO_RCVBUF SO_RCVBUF} option
     * for this {@code Socket}, that is the buffer size used by the platform
     * for input on this {@code Socket}.
     *
     * @return the value of the {@link SocketOptions#SO_RCVBUF SO_RCVBUF}
     *         option for this {@code Socket}.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
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
     * Enable/disable {@link SocketOptions#SO_KEEPALIVE SO_KEEPALIVE}.
     *
     * @param on  whether or not to have socket keep alive turned on.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public void setKeepAlive(boolean on) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_KEEPALIVE, Boolean.valueOf(on));
    }

    /**
     * Tests if {@link SocketOptions#SO_KEEPALIVE SO_KEEPALIVE} is enabled.
     *
     * @return a {@code boolean} indicating whether or not
     *         {@link SocketOptions#SO_KEEPALIVE SO_KEEPALIVE} is enabled.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as a TCP error.
     */
    public boolean getKeepAlive() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        return ((Boolean) getImpl().getOption(SocketOptions.SO_KEEPALIVE)).booleanValue();
    }

    /**
     * Sets traffic class or type-of-service octet in the IP
     * header for packets sent from this Socket.
     * As the underlying network implementation may ignore this
     * value applications should consider it a hint.
     *
     * The tc <b>must</b> be in the range {@code 0 <= tc <=
     * 255} or an IllegalArgumentException will be thrown.
     *
     * Notes:
     *
     * For Internet Protocol v4 the value consists of an
     * {@code integer}, the least significant 8 bits of which
     * represent the value of the TOS octet in IP packets sent by
     * the socket.
     * RFC 1349 defines the TOS values as follows:
     *
     * <ul>
     * <li><code>IPTOS_LOWCOST (0x02)</code></li>
     * <li><code>IPTOS_RELIABILITY (0x04)</code></li>
     * <li><code>IPTOS_THROUGHPUT (0x08)</code></li>
     * <li><code>IPTOS_LOWDELAY (0x10)</code></li>
     * </ul>
     * The last low order bit is always ignored as this
     * corresponds to the MBZ (must be zero) bit.
     *
     * Setting bits in the precedence field may result in a
     * SocketException indicating that the operation is not
     * permitted.
     *
     * As RFC 1122 section 4.2.4.2 indicates, a compliant TCP
     * implementation should, but is not required to, let application
     * change the TOS field during the lifetime of a connection.
     * So whether the type-of-service field can be changed after the
     * TCP connection has been established depends on the implementation
     * in the underlying platform. Applications should not assume that
     * they can change the TOS field after the connection.
     *
     * For Internet Protocol v6 {@code tc} is the value that
     * would be placed into the sin6_flowinfo field of the IP header.
     *
     * @param tc        an {@code int} value for the bitset.
     * @throws SocketException if there is an error setting the
     * traffic class or type-of-service
     */
    public void setTrafficClass(int tc) throws SocketException {
        if (tc < 0 || tc > 255)
            throw new IllegalArgumentException("tc is not in range 0 -- 255");

        if (isClosed())
            throw new SocketException("Socket is closed");
        try {
            getImpl().setOption(SocketOptions.IP_TOS, tc);
        } catch (SocketException se) {
            // not supported if socket already connected
            // Solaris returns error in such cases
            if (!isConnected())
                throw se;
        }
    }

    /**
     * Gets traffic class or type-of-service in the IP header
     * for packets sent from this Socket
     *
     * As the underlying network implementation may ignore the
     * traffic class or type-of-service set using {@link #setTrafficClass(int)}
     * this method may return a different value than was previously
     * set using the {@link #setTrafficClass(int)} method on this Socket.
     *
     * @return the traffic class or type-of-service already set
     * @throws SocketException if there is an error obtaining the
     * traffic class or type-of-service value.
     */
    public int getTrafficClass() throws SocketException {
        return ((Integer) (getImpl().getOption(SocketOptions.IP_TOS))).intValue();
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
     * Enabling {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR}
     * prior to binding the socket using {@link #bind(SocketAddress)} allows
     * the socket to be bound even though a previous connection is in a timeout
     * state.
     *
     * When a {@code Socket} is created the initial setting
     * of {@link SocketOptions#SO_REUSEADDR SO_REUSEADDR} is disabled.
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
     * Closes this socket.
     *
     * Any thread currently blocked in an I/O operation upon this socket
     * will throw a {@link SocketException}.
     *
     * Once a socket has been closed, it is not available for further networking
     * use (i.e. can't be reconnected or rebound). A new socket needs to be
     * created.
     *
     * Closing this socket will also close the socket's
     * {@link java.io.InputStream InputStream} and
     * {@link java.io.OutputStream OutputStream}.
     *
     * If this socket has an associated channel then the channel is closed
     * as well.
     *
     * @throws IOException  if an I/O error occurs when closing this socket.
     */
    public synchronized void close() throws IOException {
        synchronized (closeLock) {
            if (isClosed())
                return;
            if (created)
                impl.close();
            closed = true;
        }
    }

    /**
     * Places the input stream for this socket at "end of stream".
     * Any data sent to the input stream side of the socket is acknowledged
     * and then silently discarded.
     *
     * If you read from a socket input stream after invoking this method on the
     * socket, the stream's {@code available} method will return 0, and its
     * {@code read} methods will return {@code -1} (end of stream).
     *
     * @throws IOException if an I/O error occurs when shutting down this
     * socket.
     */
    public void shutdownInput() throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (!isConnected())
            throw new SocketException("Socket is not connected");
        if (isInputShutdown())
            throw new SocketException("Socket input is already shutdown");
        getImpl().shutdownInput();
        shutIn = true;
    }

    /**
     * Disables the output stream for this socket.
     * For a TCP socket, any previously written data will be sent
     * followed by TCP's normal connection termination sequence.
     *
     * If you write to a socket output stream after invoking
     * shutdownOutput() on the socket, the stream will throw
     * an IOException.
     *
     * @throws IOException if an I/O error occurs when shutting down this
     * socket.
     */
    public void shutdownOutput() throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (!isConnected())
            throw new SocketException("Socket is not connected");
        if (isOutputShutdown())
            throw new SocketException("Socket output is already shutdown");
        getImpl().shutdownOutput();
        shutOut = true;
    }

    /**
     * Converts this socket to a {@code String}.
     *
     * @return a string representation of this socket.
     */
    public String toString() {
        try {
            if (isConnected())
                return String.str("Socket[addr=", getImpl().getInetAddress(), ",port=", getImpl().getPort(), ",localport=", getImpl().getLocalPort(), "]");
        } catch (SocketException e) {
        }
        return "Socket[unconnected]";
    }

    /**
     * Returns the connection state of the socket.
     *
     * Note: Closing a socket doesn't clear its connection state, which means
     * this method will return {@code true} for a closed socket
     * (see {@link #isClosed()}) if it was successfuly connected prior
     * to being closed.
     *
     * @return true if the socket was successfuly connected to a server
     */
    public boolean isConnected() {
        return connected;
    }

    /**
     * Returns the binding state of the socket.
     *
     * Note: Closing a socket doesn't clear its binding state, which means
     * this method will return {@code true} for a closed socket
     * (see {@link #isClosed()}) if it was successfuly bound prior
     * to being closed.
     *
     * @return true if the socket was successfuly bound to an address
     */
    public boolean isBound() {
        return bound;
    }

    /**
     * Returns the closed state of the socket.
     *
     * @return true if the socket has been closed
     */
    public boolean isClosed() {
        synchronized (closeLock) {
            return closed;
        }
    }

    /**
     * Returns whether the read-half of the socket connection is closed.
     *
     * @return true if the input of the socket has been shutdown
     */
    public boolean isInputShutdown() {
        return shutIn;
    }

    /**
     * Returns whether the write-half of the socket connection is closed.
     *
     * @return true if the output of the socket has been shutdown
     */
    public boolean isOutputShutdown() {
        return shutOut;
    }

    /**
     * The factory for all client sockets.
     */
    private static SocketImplFactory factory = null;

    /**
     * Sets the client socket implementation factory for the
     * application. The factory can be specified only once.
     *
     * When an application creates a new client socket, the socket
     * implementation factory's {@code createSocketImpl} method is
     * called to create the actual socket implementation.
     *
     * Passing {@code null} to the method is a no-op unless the factory
     * was already set.
     *
     * @param fac   the desired factory.
     * @throws IOException  if an I/O error occurs when setting the
     *               socket factory.
     * @throws SocketException  if the factory is already defined.
     */
    public static synchronized void setSocketImplFactory(SocketImplFactory fac) throws IOException {
        if (factory != null) {
            throw new SocketException("factory already defined");
        }
        factory = fac;
    }

    /**
     * Sets performance preferences for this socket.
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
     * compared, with larger values indicating stronger preferences. Negative
     * values represent a lower priority than positive values. If the
     * application prefers short connection time over both low latency and high
     * bandwidth, for example, then it could invoke this method with the values
     * {@code (1, 0, 0)}.  If the application prefers high bandwidth above low
     * latency, and low latency above short connection time, then it could
     * invoke this method with the values {@code (0, 1, 2)}.
     *
     * Invoking this method after this socket has been connected
     * will have no effect.
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
     * @return this Socket
     *
     * @throws UnsupportedOperationException if the socket does not support
     *         the option.
     *
     * @throws IllegalArgumentException if the value is not valid for
     *         the option.
     *
     * @throws IOException if an I/O error occurs, or if the socket is closed.
     *
     * @throws NullPointerException if name is {@code null}
     */
    public <T> Socket setOption(SocketOption<T> name, T value) throws IOException {
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
     * @throws UnsupportedOperationException if the socket does not support
     *         the option.
     *
     * @throws IOException if an I/O error occurs, or if the socket is closed.
     *
     * @throws NullPointerException if name is {@code null}
     */
    // @SuppressWarnings("unchecked")
    public <T> T getOption(SocketOption<T> name) throws IOException {
        return getImpl().getOption(name);
    }

    private static Set<SocketOption<?>> options;
    private static boolean optionsSet = false;

    /**
     * Returns a set of the socket options supported by this socket.
     *
     * This method will continue to return the set of options even after
     * the socket has been closed.
     *
     * @return A set of the socket options supported by this socket. This set
     *         may be empty if the socket's SocketImpl cannot be created.
     */
    public Set<SocketOption<?>> supportedOptions() {
        synchronized (Socket.class) {
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
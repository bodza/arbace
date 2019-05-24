package java.net;

import java.io.IOException;
import java.nio.channels.DatagramChannel;
import java.util.Set;
import java.util.Collections;

/**
 * This class represents a socket for sending and receiving datagram packets.
 *
 * A datagram socket is the sending or receiving point for a packet
 * delivery service. Each packet sent or received on a datagram socket
 * is individually addressed and routed. Multiple packets sent from
 * one machine to another may be routed differently, and may arrive in
 * any order.
 *
 * Where possible, a newly constructed {@code DatagramSocket} has the
 * {@link SocketOptions#SO_BROADCAST SO_BROADCAST} socket option enabled so as
 * to allow the transmission of broadcast datagrams. In order to receive
 * broadcast packets a DatagramSocket should be bound to the wildcard address.
 * In some implementations, broadcast packets may also be received when
 * a DatagramSocket is bound to a more specific address.
 *
 * Example:
 * {@code
 *              DatagramSocket s = new DatagramSocket(null);
 *              s.bind(new InetSocketAddress(8888));
 * }
 * Which is equivalent to:
 * {@code
 *              DatagramSocket s = new DatagramSocket(8888);
 * }
 * Both cases will create a DatagramSocket able to receive broadcasts on
 * UDP port 8888.
 */
public class DatagramSocket implements java.io.Closeable {
    /**
     * Various states of this socket.
     */
    private boolean created = false;
    private boolean bound = false;
    private boolean closed = false;
    private Object closeLock = new Object();

    /*
     * The implementation of this DatagramSocket.
     */
    DatagramSocketImpl impl;

    /**
     * Set when a socket is ST_CONNECTED until we are certain
     * that any packets which might have been received prior
     * to calling connect() but not read by the application
     * have been read. During this time we check the source
     * address of all packets received to be sure they are from
     * the connected destination. Other packets are read but
     * silently dropped.
     */
    private boolean explicitFilter = false;
    private int bytesLeftToFilter;
    /*
     * Connection state:
     * ST_NOT_CONNECTED = socket not connected
     * ST_CONNECTED = socket connected
     * ST_CONNECTED_NO_IMPL = socket connected but not at impl level
     */
    static final int ST_NOT_CONNECTED = 0;
    static final int ST_CONNECTED = 1;
    static final int ST_CONNECTED_NO_IMPL = 2;

    int connectState = ST_NOT_CONNECTED;

    /*
     * Connected address & port
     */
    InetAddress connectedAddress = null;
    int connectedPort = -1;

    /**
     * Connects this socket to a remote socket address (IP address + port number).
     * Binds socket if not already bound.
     *
     * @param address The remote address.
     * @param port    The remote port
     * @throws SocketException if binding the socket fails.
     */
    private synchronized void connectInternal(InetAddress address, int port) throws SocketException {
        if (port < 0 || port > 0xFFFF) {
            throw new IllegalArgumentException("connect: " + port);
        }
        if (address == null) {
            throw new IllegalArgumentException("connect: null address");
        }
        checkAddress (address, "connect");
        if (isClosed())
            return;

        if (!isBound())
          bind(new InetSocketAddress(0));

        if (impl instanceof AbstractPlainDatagramSocketImpl && ((AbstractPlainDatagramSocketImpl)impl).nativeConnectDisabled()) {
            connectState = ST_CONNECTED_NO_IMPL;
        } else {
            try {
                getImpl().connect(address, port);

                // socket is now connected by the impl
                connectState = ST_CONNECTED;
                // Do we need to filter some packets?
                int avail = getImpl().dataAvailable();
                if (avail == -1) {
                    throw new SocketException();
                }
                explicitFilter = avail > 0;
                if (explicitFilter) {
                    bytesLeftToFilter = getReceiveBufferSize();
                }
            } catch (SocketException se) {
                // connection will be emulated by DatagramSocket
                connectState = ST_CONNECTED_NO_IMPL;
            }
        }

        connectedAddress = address;
        connectedPort = port;
    }

    /**
     * Constructs a datagram socket and binds it to any available port
     * on the local host machine.  The socket will be bound to the
     * {@link InetAddress#isAnyLocalAddress wildcard} address,
     * an IP address chosen by the kernel.
     *
     * @throws SocketException  if the socket could not be opened,
     *               or the socket could not bind to the specified local port.
     */
    public DatagramSocket() throws SocketException {
        this(new InetSocketAddress(0));
    }

    /**
     * Creates an unbound datagram socket with the specified
     * DatagramSocketImpl.
     *
     * @param impl an instance of a <b>DatagramSocketImpl</b>
     *        the subclass wishes to use on the DatagramSocket.
     */
    protected DatagramSocket(DatagramSocketImpl impl) {
        if (impl == null)
            throw new NullPointerException();
        this.impl = impl;
    }

    /**
     * Creates a datagram socket, bound to the specified local
     * socket address.
     *
     * If, if the address is {@code null}, creates an unbound socket.
     *
     * @param bindaddr local socket address to bind, or {@code null}
     *                 for an unbound socket.
     *
     * @throws SocketException  if the socket could not be opened,
     *               or the socket could not bind to the specified local port.
     */
    public DatagramSocket(SocketAddress bindaddr) throws SocketException {
        // create a datagram socket.
        createImpl();
        if (bindaddr != null) {
            try {
                bind(bindaddr);
            } finally {
                if (!isBound())
                    close();
            }
        }
    }

    /**
     * Constructs a datagram socket and binds it to the specified port
     * on the local host machine.  The socket will be bound to the
     * {@link InetAddress#isAnyLocalAddress wildcard} address,
     * an IP address chosen by the kernel.
     *
     * @param port port to use.
     * @throws SocketException  if the socket could not be opened,
     *               or the socket could not bind to the specified local port.
     */
    public DatagramSocket(int port) throws SocketException {
        this(port, null);
    }

    /**
     * Creates a datagram socket, bound to the specified local
     * address.  The local port must be between 0 and 65535 inclusive.
     * If the IP address is 0.0.0.0, the socket will be bound to the
     * {@link InetAddress#isAnyLocalAddress wildcard} address,
     * an IP address chosen by the kernel.
     *
     * @param port local port to use
     * @param laddr local address to bind
     *
     * @throws SocketException  if the socket could not be opened,
     *               or the socket could not bind to the specified local port.
     */
    public DatagramSocket(int port, InetAddress laddr) throws SocketException {
        this(new InetSocketAddress(laddr, port));
    }

    static Class<?> implClass = null;

    void createImpl() throws SocketException {
        if (impl == null) {
            if (factory != null) {
                impl = factory.createDatagramSocketImpl();
            } else {
                boolean isMulticast = (this instanceof MulticastSocket) ? true : false;
                impl = DefaultDatagramSocketImplFactory.createDatagramSocketImpl(isMulticast);
            }
        }
        // creates a udp socket
        impl.create();
        impl.setDatagramSocket(this);
        created = true;
    }

    /**
     * Get the {@code DatagramSocketImpl} attached to this socket,
     * creating it if necessary.
     *
     * @return the {@code DatagramSocketImpl} attached to that
     *          DatagramSocket
     * @throws SocketException if creation fails.
     */
    DatagramSocketImpl getImpl() throws SocketException {
        if (!created)
            createImpl();
        return impl;
    }

    /**
     * Binds this DatagramSocket to a specific address and port.
     *
     * If the address is {@code null}, then the system will pick up
     * an ephemeral port and a valid local address to bind the socket.
     *
     * @param addr The address and port to bind to.
     * @throws SocketException if any error happens during the bind, or if the
     *          socket is already bound.
     * @throws IllegalArgumentException if addr is a SocketAddress subclass
     *         not supported by this socket.
     */
    public synchronized void bind(SocketAddress addr) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (isBound())
            throw new SocketException("already bound");
        if (addr == null)
            addr = new InetSocketAddress(0);
        if (!(addr instanceof InetSocketAddress))
            throw new IllegalArgumentException("Unsupported address type!");
        InetSocketAddress epoint = (InetSocketAddress) addr;
        if (epoint.isUnresolved())
            throw new SocketException("Unresolved address");
        InetAddress iaddr = epoint.getAddress();
        int port = epoint.getPort();
        checkAddress(iaddr, "bind");
        try {
            getImpl().bind(port, iaddr);
        } catch (SocketException e) {
            getImpl().close();
            throw e;
        }
        bound = true;
    }

    void checkAddress (InetAddress addr, String op) {
        if (addr == null) {
            return;
        }
        if (!(addr instanceof Inet4Address || addr instanceof Inet6Address)) {
            throw new IllegalArgumentException(op + ": invalid address type");
        }
    }

    /**
     * Connects the socket to a remote address for this socket. When a
     * socket is connected to a remote address, packets may only be
     * sent to or received from that address. By default a datagram
     * socket is not connected.
     *
     * If the remote destination to which the socket is connected does not
     * exist, or is otherwise unreachable, and if an ICMP destination unreachable
     * packet has been received for that address, then a subsequent call to
     * send or receive may throw a PortUnreachableException. Note, there is no
     * guarantee that the exception will be thrown.
     *
     * When a socket is connected, {@link #receive receive} and
     * {@link #send send} <b>will not perform any security checks</b>
     * on incoming and outgoing packets, other than matching the packet's
     * and the socket's address and port. On a send operation, if the
     * packet's address is set and the packet's address and the socket's
     * address do not match, an {@code IllegalArgumentException} will be
     * thrown. A socket connected to a multicast address may only be used
     * to send packets.
     *
     * @param address the remote address for the socket
     *
     * @param port the remote port for the socket.
     *
     * @throws IllegalArgumentException
     *         if the address is null, or the port is out of range.
     */
    public void connect(InetAddress address, int port) {
        try {
            connectInternal(address, port);
        } catch (SocketException se) {
            throw new Error("connect failed", se);
        }
    }

    /**
     * Connects this socket to a remote socket address (IP address + port number).
     *
     * If given an {@link InetSocketAddress InetSocketAddress}, this method
     * behaves as if invoking {@link #connect(InetAddress,int) connect(InetAddress,int)}
     * with the given socket addresses IP address and port number.
     *
     * @param addr    The remote address.
     *
     * @throws SocketException
     *          if the connect fails
     *
     * @throws IllegalArgumentException
     *         if {@code addr} is {@code null}, or {@code addr} is a SocketAddress
     *         subclass not supported by this socket
     */
    public void connect(SocketAddress addr) throws SocketException {
        if (addr == null)
            throw new IllegalArgumentException("Address can't be null");
        if (!(addr instanceof InetSocketAddress))
            throw new IllegalArgumentException("Unsupported address type");
        InetSocketAddress epoint = (InetSocketAddress) addr;
        if (epoint.isUnresolved())
            throw new SocketException("Unresolved address");
        connectInternal(epoint.getAddress(), epoint.getPort());
    }

    /**
     * Disconnects the socket. If the socket is closed or not connected,
     * then this method has no effect.
     */
    public void disconnect() {
        synchronized (this) {
            if (isClosed())
                return;
            if (connectState == ST_CONNECTED) {
                impl.disconnect ();
            }
            connectedAddress = null;
            connectedPort = -1;
            connectState = ST_NOT_CONNECTED;
            explicitFilter = false;
        }
    }

    /**
     * Returns the binding state of the socket.
     *
     * If the socket was bound prior to being {@link #close closed},
     * then this method will continue to return {@code true}
     * after the socket is closed.
     *
     * @return true if the socket successfully bound to an address
     */
    public boolean isBound() {
        return bound;
    }

    /**
     * Returns the connection state of the socket.
     *
     * If the socket was connected prior to being {@link #close closed},
     * then this method will continue to return {@code true}
     * after the socket is closed.
     *
     * @return true if the socket successfully connected to a server
     */
    public boolean isConnected() {
        return connectState != ST_NOT_CONNECTED;
    }

    /**
     * Returns the address to which this socket is connected. Returns
     * {@code null} if the socket is not connected.
     *
     * If the socket was connected prior to being {@link #close closed},
     * then this method will continue to return the connected address
     * after the socket is closed.
     *
     * @return the address to which this socket is connected.
     */
    public InetAddress getInetAddress() {
        return connectedAddress;
    }

    /**
     * Returns the port number to which this socket is connected.
     * Returns {@code -1} if the socket is not connected.
     *
     * If the socket was connected prior to being {@link #close closed},
     * then this method will continue to return the connected port number
     * after the socket is closed.
     *
     * @return the port number to which this socket is connected.
     */
    public int getPort() {
        return connectedPort;
    }

    /**
     * Returns the address of the endpoint this socket is connected to, or
     * {@code null} if it is unconnected.
     *
     * If the socket was connected prior to being {@link #close closed},
     * then this method will continue to return the connected address
     * after the socket is closed.
     *
     * @return a {@code SocketAddress} representing the remote
     *         endpoint of this socket, or {@code null} if it is
     *         not connected yet.
     */
    public SocketAddress getRemoteSocketAddress() {
        if (!isConnected())
            return null;
        return new InetSocketAddress(getInetAddress(), getPort());
    }

    /**
     * Returns the address of the endpoint this socket is bound to.
     *
     * @return a {@code SocketAddress} representing the local endpoint of this
     *         socket, or {@code null} if it is closed or not bound yet.
     */
    public SocketAddress getLocalSocketAddress() {
        if (isClosed())
            return null;
        if (!isBound())
            return null;
        return new InetSocketAddress(getLocalAddress(), getLocalPort());
    }

    /**
     * Sends a datagram packet from this socket. The
     * {@code DatagramPacket} includes information indicating the
     * data to be sent, its length, the IP address of the remote host,
     * and the port number on the remote host.
     *
     * @param p   the {@code DatagramPacket} to be sent.
     *
     * @throws IOException  if an I/O error occurs.
     * @throws PortUnreachableException may be thrown if the socket is connected
     *             to a currently unreachable destination. Note, there is no
     *             guarantee that the exception will be thrown.
     * @throws java.nio.channels.IllegalBlockingModeException
     *             if this socket has an associated channel,
     *             and the channel is in non-blocking mode.
     * @throws IllegalArgumentException if the socket is connected,
     *             and connected address and packet address differ.
     */
    public void send(DatagramPacket p) throws IOException {
        InetAddress packetAddress = null;
        synchronized (p) {
            if (isClosed())
                throw new SocketException("Socket is closed");
            checkAddress (p.getAddress(), "send");
            if (connectState == ST_NOT_CONNECTED) {
            } else {
                // we're connected
                packetAddress = p.getAddress();
                if (packetAddress == null) {
                    p.setAddress(connectedAddress);
                    p.setPort(connectedPort);
                } else if ((!packetAddress.equals(connectedAddress)) || p.getPort() != connectedPort) {
                    throw new IllegalArgumentException("connected address " + "and packet address" + " differ");
                }
            }
            // Check whether the socket is bound
            if (!isBound())
                bind(new InetSocketAddress(0));
            // call the  method to send
            getImpl().send(p);
        }
    }

    /**
     * Receives a datagram packet from this socket. When this method
     * returns, the {@code DatagramPacket}'s buffer is filled with
     * the data received. The datagram packet also contains the sender's
     * IP address, and the port number on the sender's machine.
     *
     * This method blocks until a datagram is received. The
     * {@code length} field of the datagram packet object contains
     * the length of the received message. If the message is longer than
     * the packet's length, the message is truncated.
     *
     * @param p   the {@code DatagramPacket} into which to place
     *                 the incoming data.
     * @throws IOException  if an I/O error occurs.
     * @throws SocketTimeoutException  if setSoTimeout was previously called
     *                 and the timeout has expired.
     * @throws PortUnreachableException may be thrown if the socket is connected
     *             to a currently unreachable destination. Note, there is no guarantee that the
     *             exception will be thrown.
     * @throws java.nio.channels.IllegalBlockingModeException
     *             if this socket has an associated channel,
     *             and the channel is in non-blocking mode.
     */
    public synchronized void receive(DatagramPacket p) throws IOException {
        synchronized (p) {
            if (!isBound())
                bind(new InetSocketAddress(0));
            DatagramPacket tmp = null;
            if ((connectState == ST_CONNECTED_NO_IMPL) || explicitFilter) {
                // We have to do the filtering the old fashioned way since
                // the native impl doesn't support connect or the connect
                // via the impl failed, or .. "explicitFilter" may be set when
                // a socket is connected via the impl, for a period of time
                // when packets from other sources might be queued on socket.
                boolean stop = false;
                while (!stop) {
                    InetAddress peekAddress = null;
                    int peekPort = -1;
                    // peek at the packet to see who it is from.
                    DatagramPacket peekPacket = new DatagramPacket(new byte[1], 1);
                    peekPort = getImpl().peekData(peekPacket);
                    peekAddress = peekPacket.getAddress();
                    if ((!connectedAddress.equals(peekAddress)) || (connectedPort != peekPort)) {
                        // throw the packet away and silently continue
                        tmp = new DatagramPacket(new byte[1024], 1024);
                        getImpl().receive(tmp);
                        if (explicitFilter) {
                            if (checkFiltering(tmp)) {
                                stop = true;
                            }
                        }
                    } else {
                        stop = true;
                    }
                }
            }
            // If the security check succeeds, or the datagram is
            // connected then receive the packet
            getImpl().receive(p);
            if (explicitFilter && tmp == null) {
                // packet was not filtered, account for it here
                checkFiltering(p);
            }
        }
    }

    private boolean checkFiltering(DatagramPacket p) throws SocketException {
        bytesLeftToFilter -= p.getLength();
        if (bytesLeftToFilter <= 0 || getImpl().dataAvailable() <= 0) {
            explicitFilter = false;
            return true;
        }
        return false;
    }

    /**
     * Gets the local address to which the socket is bound.
     *
     * @return the local address to which the socket is bound,
     *          {@code null} if the socket is closed, or
     *          an {@code InetAddress} representing
     *          {@link InetAddress#isAnyLocalAddress wildcard}
     *          address if either the socket is not bound, or
     *          the security manager {@code checkConnect}
     *          method does not allow the operation
     */
    public InetAddress getLocalAddress() {
        if (isClosed())
            return null;
        InetAddress in = null;
        try {
            in = (InetAddress) getImpl().getOption(SocketOptions.SO_BINDADDR);
            if (in.isAnyLocalAddress()) {
                in = InetAddress.anyLocalAddress();
            }
        } catch (Exception e) {
            in = InetAddress.anyLocalAddress(); // "0.0.0.0"
        }
        return in;
    }

    /**
     * Returns the port number on the local host to which this socket
     * is bound.
     *
     * @return the port number on the local host to which this socket is bound,
                {@code -1} if the socket is closed, or
                {@code 0} if it is not bound yet.
     */
    public int getLocalPort() {
        if (isClosed())
            return -1;
        try {
            return getImpl().getLocalPort();
        } catch (Exception e) {
            return 0;
        }
    }

    /** Enable/disable SO_TIMEOUT with the specified timeout, in
     *  milliseconds. With this option set to a non-zero timeout,
     *  a call to receive() for this DatagramSocket
     *  will block for only this amount of time.  If the timeout expires,
     *  a <b>java.net.SocketTimeoutException</b> is raised, though the
     *  DatagramSocket is still valid.  The option <b>must</b> be enabled
     *  prior to entering the blocking operation to have effect.  The
     *  timeout must be {@code > 0}.
     *  A timeout of zero is interpreted as an infinite timeout.
     *
     * @param timeout the specified timeout in milliseconds.
     * @throws SocketException if there is an error in the underlying protocol, such as an UDP error.
     */
    public synchronized void setSoTimeout(int timeout) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_TIMEOUT, timeout);
    }

    /**
     * Retrieve setting for SO_TIMEOUT.  0 returns implies that the
     * option is disabled (i.e., timeout of infinity).
     *
     * @return the setting for SO_TIMEOUT
     * @throws SocketException if there is an error in the underlying protocol, such as an UDP error.
     */
    public synchronized int getSoTimeout() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        if (getImpl() == null)
            return 0;
        Object o = getImpl().getOption(SocketOptions.SO_TIMEOUT);
        /* extra type safety */
        if (o instanceof Integer) {
            return ((Integer) o).intValue();
        } else {
            return 0;
        }
    }

    /**
     * Sets the SO_SNDBUF option to the specified value for this
     * {@code DatagramSocket}. The SO_SNDBUF option is used by the
     * network implementation as a hint to size the underlying
     * network I/O buffers. The SO_SNDBUF setting may also be used
     * by the network implementation to determine the maximum size
     * of the packet that can be sent on this socket.
     *
     * As SO_SNDBUF is a hint, applications that want to verify
     * what size the buffer is should call {@link #getSendBufferSize()}.
     *
     * Increasing the buffer size may allow multiple outgoing packets
     * to be queued by the network implementation when the send rate
     * is high.
     *
     * Note: If {@link #send(DatagramPacket)} is used to send a
     * {@code DatagramPacket} that is larger than the setting
     * of SO_SNDBUF then it is implementation specific if the
     * packet is sent or discarded.
     *
     * @param size the size to which to set the send buffer
     * size. This value must be greater than 0.
     *
     * @throws SocketException if there is an error
     * in the underlying protocol, such as an UDP error.
     * @throws IllegalArgumentException if the value is 0 or is
     * negative.
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
     * Get value of the SO_SNDBUF option for this {@code DatagramSocket}, that is the
     * buffer size used by the platform for output on this {@code DatagramSocket}.
     *
     * @return the value of the SO_SNDBUF option for this {@code DatagramSocket}
     * @throws SocketException if there is an error in
     * the underlying protocol, such as an UDP error.
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
     * Sets the SO_RCVBUF option to the specified value for this
     * {@code DatagramSocket}. The SO_RCVBUF option is used by
     * the network implementation as a hint to size the underlying
     * network I/O buffers. The SO_RCVBUF setting may also be used
     * by the network implementation to determine the maximum size
     * of the packet that can be received on this socket.
     *
     * Because SO_RCVBUF is a hint, applications that want to
     * verify what size the buffers were set to should call
     * {@link #getReceiveBufferSize()}.
     *
     * Increasing SO_RCVBUF may allow the network implementation
     * to buffer multiple packets when packets arrive faster than
     * are being received using {@link #receive(DatagramPacket)}.
     *
     * Note: It is implementation specific if a packet larger
     * than SO_RCVBUF can be received.
     *
     * @param size the size to which to set the receive buffer
     * size. This value must be greater than 0.
     *
     * @throws SocketException if there is an error in
     * the underlying protocol, such as an UDP error.
     * @throws IllegalArgumentException if the value is 0 or is
     * negative.
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
     * Get value of the SO_RCVBUF option for this {@code DatagramSocket}, that is the
     * buffer size used by the platform for input on this {@code DatagramSocket}.
     *
     * @return the value of the SO_RCVBUF option for this {@code DatagramSocket}
     * @throws SocketException if there is an error in the underlying protocol, such as an UDP error.
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
     * Enable/disable the SO_REUSEADDR socket option.
     *
     * For UDP sockets it may be necessary to bind more than one
     * socket to the same socket address. This is typically for the
     * purpose of receiving multicast packets
     * (See {@link java.net.MulticastSocket}). The
     * {@code SO_REUSEADDR} socket option allows multiple
     * sockets to be bound to the same socket address if the
     * {@code SO_REUSEADDR} socket option is enabled prior
     * to binding the socket using {@link #bind(SocketAddress)}.
     *
     * Note: This functionality is not supported by all existing platforms,
     * so it is implementation specific whether this option will be ignored
     * or not. However, if it is not supported then
     * {@link #getReuseAddress()} will always return {@code false}.
     *
     * When a {@code DatagramSocket} is created the initial setting
     * of {@code SO_REUSEADDR} is disabled.
     *
     * The behaviour when {@code SO_REUSEADDR} is enabled or
     * disabled after a socket is bound (See {@link #isBound()})
     * is not defined.
     *
     * @param on  whether to enable or disable the
     * @throws SocketException if an error occurs enabling or
     *            disabling the {@code SO_RESUEADDR} socket option,
     *            or the socket is closed.
     */
    public synchronized void setReuseAddress(boolean on) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_REUSEADDR, Boolean.valueOf(on));
    }

    /**
     * Tests if SO_REUSEADDR is enabled.
     *
     * @return a {@code boolean} indicating whether or not SO_REUSEADDR is enabled.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as an UDP error.
     */
    public synchronized boolean getReuseAddress() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        Object o = getImpl().getOption(SocketOptions.SO_REUSEADDR);
        return ((Boolean)o).booleanValue();
    }

    /**
     * Enable/disable SO_BROADCAST.
     *
     * Some operating systems may require that the Java virtual machine be
     * started with implementation specific privileges to enable this option or
     * send broadcast datagrams.
     *
     * @param on
     *         whether or not to have broadcast turned on.
     *
     * @throws SocketException
     *          if there is an error in the underlying protocol, such as an UDP
     *          error.
     */
    public synchronized void setBroadcast(boolean on) throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setOption(SocketOptions.SO_BROADCAST, Boolean.valueOf(on));
    }

    /**
     * Tests if SO_BROADCAST is enabled.
     * @return a {@code boolean} indicating whether or not SO_BROADCAST is enabled.
     * @throws SocketException if there is an error
     * in the underlying protocol, such as an UDP error.
     */
    public synchronized boolean getBroadcast() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        return ((Boolean)(getImpl().getOption(SocketOptions.SO_BROADCAST))).booleanValue();
    }

    /**
     * Sets traffic class or type-of-service octet in the IP
     * datagram header for datagrams sent from this DatagramSocket.
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
     * for Internet Protocol v6 {@code tc} is the value that
     * would be placed into the sin6_flowinfo field of the IP header.
     *
     * @param tc        an {@code int} value for the bitset.
     * @throws SocketException if there is an error setting the
     * traffic class or type-of-service
     */
    public synchronized void setTrafficClass(int tc) throws SocketException {
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
     * Gets traffic class or type-of-service in the IP datagram
     * header for packets sent from this DatagramSocket.
     *
     * As the underlying network implementation may ignore the
     * traffic class or type-of-service set using {@link #setTrafficClass(int)}
     * this method may return a different value than was previously
     * set using the {@link #setTrafficClass(int)} method on this
     * DatagramSocket.
     *
     * @return the traffic class or type-of-service already set
     * @throws SocketException if there is an error obtaining the
     * traffic class or type-of-service value.
     */
    public synchronized int getTrafficClass() throws SocketException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        return ((Integer)(getImpl().getOption(SocketOptions.IP_TOS))).intValue();
    }

    /**
     * Closes this datagram socket.
     *
     * Any thread currently blocked in {@link #receive} upon this socket
     * will throw a {@link SocketException}.
     *
     * If this socket has an associated channel then the channel is closed
     * as well.
     */
    public void close() {
        synchronized (closeLock) {
            if (isClosed())
                return;
            impl.close();
            closed = true;
        }
    }

    /**
     * Returns whether the socket is closed or not.
     *
     * @return true if the socket has been closed
     */
    public boolean isClosed() {
        synchronized (closeLock) {
            return closed;
        }
    }

    /**
     * User defined factory for all datagram sockets.
     */
    static DatagramSocketImplFactory factory;

    /**
     * Sets the datagram socket implementation factory for the
     * application. The factory can be specified only once.
     *
     * When an application creates a new datagram socket, the socket
     * implementation factory's {@code createDatagramSocketImpl} method is
     * called to create the actual datagram socket implementation.
     *
     * Passing {@code null} to the method is a no-op unless the factory
     * was already set.
     *
     * @param fac   the desired factory.
     * @throws IOException  if an I/O error occurs when setting the
     *              datagram socket factory.
     * @throws SocketException  if the factory is already defined.
     */
    public static synchronized void setDatagramSocketImplFactory(DatagramSocketImplFactory fac) throws IOException
    {
        if (factory != null) {
            throw new SocketException("factory already defined");
        }
        factory = fac;
    }

    /**
     * Sets the value of a socket option.
     *
     * @param <T> The type of the socket option value
     * @param name The socket option
     * @param value The value of the socket option. A value of {@code null}
     *              may be valid for some options.
     *
     * @return this DatagramSocket
     *
     * @throws UnsupportedOperationException if the datagram socket
     *         does not support the option.
     *
     * @throws IllegalArgumentException if the value is not valid for
     *         the option.
     *
     * @throws IOException if an I/O error occurs, or if the socket is closed.
     *
     * @throws NullPointerException if name is {@code null}
     */
    public <T> DatagramSocket setOption(SocketOption<T> name, T value) throws IOException
    {
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
     * @throws UnsupportedOperationException if the datagram socket
     *         does not support the option.
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
     * Returns a set of the socket options supported by this socket.
     *
     * This method will continue to return the set of options even after
     * the socket has been closed.
     *
     * @return A set of the socket options supported by this socket. This set
     *        may be empty if the socket's DatagramSocketImpl cannot be created.
     */
    public Set<SocketOption<?>> supportedOptions() {
        synchronized (DatagramSocket.class) {
            if (optionsSet) {
                return options;
            }
            try {
                DatagramSocketImpl impl = getImpl();
                options = Collections.unmodifiableSet(impl.supportedOptions());
            } catch (IOException e) {
                options = Collections.emptySet();
            }
            optionsSet = true;
            return options;
        }
    }
}

package java.net;

/**
 * Interface of methods to get/set socket options.  This interface is
 * implemented by: <b>SocketImpl</b> and  <b>DatagramSocketImpl</b>.
 * Subclasses of these should override the methods
 * of this interface in order to support their own options.
 *
 * The methods and constants which specify options in this interface are
 * for implementation only.  If you're not subclassing SocketImpl or
 * DatagramSocketImpl, <b>you won't use these directly.</b> There are
 * type-safe methods to get/set each of these options in Socket, ServerSocket,
 * DatagramSocket and MulticastSocket.
 */
public interface SocketOptions {
    /**
     * Enable/disable the option specified by <i>optID</i>.  If the option
     * is to be enabled, and it takes an option-specific "value",  this is
     * passed in <i>value</i>.  The actual type of value is option-specific,
     * and it is an error to pass something that isn't of the expected type:
     * <br><pre>
     * SocketImpl s;
     * ...
     * s.setOption(SO_LINGER, new Integer(10));
     *    // OK - set SO_LINGER w/ timeout of 10 sec.
     * s.setOption(SO_LINGER, new Double(10));
     *    // ERROR - expects java.lang.Integer
     *</pre>
     * If the requested option is binary, it can be set using this method by
     * a java.lang.Boolean:
     * <br><pre>
     * s.setOption(TCP_NODELAY, Boolean.TRUE);
     *    // OK - enables TCP_NODELAY, a binary option
     * </pre>
     * <br>
     * Any option can be disabled using this method with a Boolean.FALSE:
     * <br><pre>
     * s.setOption(TCP_NODELAY, Boolean.FALSE);
     *    // OK - disables TCP_NODELAY
     * s.setOption(SO_LINGER, Boolean.FALSE);
     *    // OK - disables SO_LINGER
     * </pre>
     * <br>
     * For an option that has a notion of on and off, and requires
     * a non-boolean parameter, setting its value to anything other than
     * <i>Boolean.FALSE</i> implicitly enables it.
     * <br>
     * Throws SocketException if the option is unrecognized,
     * the socket is closed, or some low-level error occurred
     * <br>
     * @param optID identifies the option
     * @param value the parameter of the socket option
     * @throws SocketException if the option is unrecognized,
     * the socket is closed, or some low-level error occurred
     */
    public void
        setOption(int optID, Object value) throws SocketException;

    /**
     * Fetch the value of an option.
     * Binary options will return java.lang.Boolean.TRUE
     * if enabled, java.lang.Boolean.FALSE if disabled, e.g.:
     * <br><pre>
     * SocketImpl s;
     * ...
     * Boolean noDelay = (Boolean)(s.getOption(TCP_NODELAY));
     * if (noDelay.booleanValue()) {
     *     // true if TCP_NODELAY is enabled...
     * ...
     * }
     * </pre>
     *
     * For options that take a particular type as a parameter,
     * getOption(int) will return the parameter's value, else
     * it will return java.lang.Boolean.FALSE:
     * <pre>
     * Object o = s.getOption(SO_LINGER);
     * if (o instanceof Integer) {
     *     System.out.print("Linger time is " + ((Integer)o).intValue());
     * } else {
     *   // the true type of o is java.lang.Boolean.FALSE;
     * }
     * </pre>
     *
     * @param optID an {@code int} identifying the option to fetch
     * @return the value of the option
     * @throws SocketException if the socket is closed
     * @throws SocketException if <i>optID</i> is unknown along the
     *         protocol stack (including the SocketImpl)
     */
    public Object getOption(int optID) throws SocketException;

    /**
     * The java-supported BSD-style options.
     */
    /**
     * Disable Nagle's algorithm for this connection.  Written data
     * to the network is not buffered pending acknowledgement of
     * previously written data.
     *
     * Valid for TCP only: SocketImpl.
     */
    // @Native
    public static final int TCP_NODELAY = 0x0001;

    /**
     * Fetch the local address binding of a socket (this option cannot
     * be "set" only "gotten", since sockets are bound at creation time,
     * and so the locally bound address cannot be changed).  The default local
     * address of a socket is INADDR_ANY, meaning any local address on a
     * multi-homed host.  A multi-homed host can use this option to accept
     * connections to only one of its addresses (in the case of a
     * ServerSocket or DatagramSocket), or to specify its return address
     * to the peer (for a Socket or DatagramSocket).  The parameter of
     * this option is an InetAddress.
     *
     * This option <b>must</b> be specified in the constructor.
     *
     * Valid for: SocketImpl, DatagramSocketImpl
     */
    // @Native
    public static final int SO_BINDADDR = 0x000F;

    /** Sets SO_REUSEADDR for a socket.  This is used only for MulticastSockets
     * in java, and it is set by default for MulticastSockets.
     *
     * Valid for: DatagramSocketImpl
     */
    // @Native
    public static final int SO_REUSEADDR = 0x04;

    /** Sets SO_REUSEPORT for a socket. This option enables and disables
     *  the ability to have multiple sockets listen to the same address
     *  and port.
     *
     * Valid for: SocketImpl, DatagramSocketImpl
     */
    // @Native
    public static final int SO_REUSEPORT = 0x0E;

    /**
     * Sets SO_BROADCAST for a socket. This option enables and disables
     * the ability of the process to send broadcast messages. It is supported
     * for only datagram sockets and only on networks that support
     * the concept of a broadcast message (e.g. Ethernet, token ring, etc.),
     * and it is set by default for DatagramSockets.
     */
    // @Native
    public static final int SO_BROADCAST = 0x0020;

    /** Set which outgoing interface on which to send multicast packets.
     * Useful on hosts with multiple network interfaces, where applications
     * want to use other than the system default.  Takes/returns an InetAddress.
     *
     * Valid for Multicast: DatagramSocketImpl
     */
    // @Native
    public static final int IP_MULTICAST_IF = 0x10;

    /** Same as above. This option is introduced so that the behaviour
     *  with IP_MULTICAST_IF will be kept the same as before, while
     *  this new option can support setting outgoing interfaces with either
     *  IPv4 and IPv6 addresses.
     *
     *  NOTE: make sure there is no conflict with this
     */
    // @Native
    public static final int IP_MULTICAST_IF2 = 0x1f;

    /**
     * This option enables or disables local loopback of multicast datagrams.
     * This option is enabled by default for Multicast Sockets.
     */
    // @Native
    public static final int IP_MULTICAST_LOOP = 0x12;

    /**
     * This option sets the type-of-service or traffic class field
     * in the IP header for a TCP or UDP socket.
     */
    // @Native
    public static final int IP_TOS = 0x3;

    /**
     * Specify a linger-on-close timeout.  This option disables/enables
     * immediate return from a <b>close()</b> of a TCP Socket.  Enabling
     * this option with a non-zero Integer <i>timeout</i> means that a
     * <b>close()</b> will block pending the transmission and acknowledgement
     * of all data written to the peer, at which point the socket is closed
     * <i>gracefully</i>.  Upon reaching the linger timeout, the socket is
     * closed <i>forcefully</i>, with a TCP RST. Enabling the option with a
     * timeout of zero does a forceful close immediately. If the specified
     * timeout value exceeds 65,535 it will be reduced to 65,535.
     *
     * Valid only for TCP: SocketImpl
     */
    // @Native
    public static final int SO_LINGER = 0x0080;

    /** Set a timeout on blocking Socket operations:
     * <pre>
     * ServerSocket.accept();
     * SocketInputStream.read();
     * DatagramSocket.receive();
     * </pre>
     *
     * The option must be set prior to entering a blocking
     * operation to take effect.  If the timeout expires and the
     * operation would continue to block,
     * <b>java.io.InterruptedIOException</b> is raised.  The Socket is
     * not closed in this case.
     *
     * Valid for all sockets: SocketImpl, DatagramSocketImpl
     */
    // @Native
    public static final int SO_TIMEOUT = 0x1006;

    /**
     * Set a hint the size of the underlying buffers used by the
     * platform for outgoing network I/O. When used in set, this is a
     * suggestion to the kernel from the application about the size of
     * buffers to use for the data to be sent over the socket. When
     * used in get, this must return the size of the buffer actually
     * used by the platform when sending out data on this socket.
     *
     * Valid for all sockets: SocketImpl, DatagramSocketImpl
     */
    // @Native
    public static final int SO_SNDBUF = 0x1001;

    /**
     * Set a hint the size of the underlying buffers used by the
     * platform for incoming network I/O. When used in set, this is a
     * suggestion to the kernel from the application about the size of
     * buffers to use for the data to be received over the
     * socket. When used in get, this must return the size of the
     * buffer actually used by the platform when receiving in data on
     * this socket.
     *
     * Valid for all sockets: SocketImpl, DatagramSocketImpl
     */
    // @Native
    public static final int SO_RCVBUF = 0x1002;

    /**
     * When the keepalive option is set for a TCP socket and no data
     * has been exchanged across the socket in either direction for
     * 2 hours (NOTE: the actual value is implementation dependent),
     * TCP automatically sends a keepalive probe to the peer. This probe is a
     * TCP segment to which the peer must respond.
     * One of three responses is expected:
     * 1. The peer responds with the expected ACK. The application is not
     *    notified (since everything is OK). TCP will send another probe
     *    following another 2 hours of inactivity.
     * 2. The peer responds with an RST, which tells the local TCP that
     *    the peer host has crashed and rebooted. The socket is closed.
     * 3. There is no response from the peer. The socket is closed.
     *
     * The purpose of this option is to detect if the peer host crashes.
     *
     * Valid only for TCP socket: SocketImpl
     */
    // @Native
    public static final int SO_KEEPALIVE = 0x0008;

    /**
     * When the OOBINLINE option is set, any TCP urgent data received on
     * the socket will be received through the socket input stream.
     * When the option is disabled (which is the default) urgent data
     * is silently discarded.
     */
    // @Native
    public static final int SO_OOBINLINE = 0x1003;
}

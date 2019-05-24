package java.net;

import java.io.IOException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Set;

/**
 * The multicast datagram socket class is useful for sending
 * and receiving IP multicast packets.  A MulticastSocket is
 * a (UDP) DatagramSocket, with additional capabilities for
 * joining "groups" of other multicast hosts on the internet.
 *
 * A multicast group is specified by a class D IP address
 * and by a standard UDP port number. Class D IP addresses
 * are in the range <code>224.0.0.0</code> to <code>239.255.255.255</code>,
 * inclusive. The address 224.0.0.0 is reserved and should not be used.
 *
 * One would join a multicast group by first creating a MulticastSocket
 * with the desired port, then invoking the
 * <code>joinGroup(InetAddress groupAddr)</code>
 * method:
 * <pre>
 * // join a Multicast group and send the group salutations
 * ...
 * String msg = "Hello";
 * InetAddress group = InetAddress.getByName("228.5.6.7");
 * MulticastSocket s = new MulticastSocket(6789);
 * s.joinGroup(group);
 * DatagramPacket hi = new DatagramPacket(msg.getBytes(), msg.length(), group, 6789);
 * s.send(hi);
 * // get their responses!
 * byte[] buf = new byte[1000];
 * DatagramPacket recv = new DatagramPacket(buf, buf.length);
 * s.receive(recv);
 * ...
 * // OK, I'm done talking - leave the group...
 * s.leaveGroup(group);
 * </pre>
 *
 * When one sends a message to a multicast group, <b>all</b> subscribing
 * recipients to that host and port receive the message (within the
 * time-to-live range of the packet, see below).  The socket needn't
 * be a member of the multicast group to send messages to it.
 *
 * When a socket subscribes to a multicast group/port, it receives
 * datagrams sent by other hosts to the group/port, as do all other
 * members of the group and port.  A socket relinquishes membership
 * in a group by the leaveGroup(InetAddress addr) method.  <b>
 * Multiple MulticastSocket's</b> may subscribe to a multicast group
 * and port concurrently, and they will all receive group datagrams.
 *
 * Currently applets are not allowed to use multicast sockets.
 */
public class MulticastSocket extends DatagramSocket {
    /**
     * Used on some platforms to record if an outgoing interface
     * has been set for this socket.
     */
    private boolean interfaceSet;

    /**
     * Create a multicast socket.
     *
     * When the socket is created the
     * {@link DatagramSocket#setReuseAddress(boolean)} method is called to
     * enable the SO_REUSEADDR socket option.
     *
     * @throws IOException if an I/O exception occurs while creating the
     * MulticastSocket
     */
    public MulticastSocket() throws IOException {
        this(new InetSocketAddress(0));
    }

    /**
     * Create a multicast socket and bind it to a specific port.
     *
     * When the socket is created the
     * {@link DatagramSocket#setReuseAddress(boolean)} method is
     * called to enable the SO_REUSEADDR socket option.
     *
     * @param port port to use
     * @throws IOException if an I/O exception occurs
     * while creating the MulticastSocket
     */
    public MulticastSocket(int port) throws IOException {
        this(new InetSocketAddress(port));
    }

    /**
     * Create a MulticastSocket bound to the specified socket address.
     *
     * Or, if the address is {@code null}, create an unbound socket.
     *
     * When the socket is created the
     * {@link DatagramSocket#setReuseAddress(boolean)} method is
     * called to enable the SO_REUSEADDR socket option.
     *
     * @param bindaddr Socket address to bind to, or {@code null} for
     *                 an unbound socket.
     * @throws IOException if an I/O exception occurs
     * while creating the MulticastSocket
     */
    public MulticastSocket(SocketAddress bindaddr) throws IOException {
        super((SocketAddress) null);

        // Enable SO_REUSEADDR before binding
        setReuseAddress(true);

        if (bindaddr != null) {
            try {
                bind(bindaddr);
            } finally {
                if (!isBound()) {
                    close();
                }
            }
        }
    }

    /**
     * The lock on the socket's TTL. This is for set/getTTL and
     * send(packet,ttl).
     */
    private Object ttlLock = new Object();

    /**
     * The lock on the socket's interface - used by setInterface
     * and getInterface
     */
    private Object infLock = new Object();

    /**
     * The "last" interface set by setInterface on this MulticastSocket
     */
    private InetAddress infAddress = null;

    /**
     * Set the default time-to-live for multicast packets sent out
     * on this {@code MulticastSocket} in order to control the
     * scope of the multicasts.
     *
     * The ttl is an <b>unsigned</b> 8-bit quantity, and so <b>must</b> be
     * in the range {@code 0 <= ttl <= 0xFF }.
     *
     * @param ttl the time-to-live
     * @throws IOException if an I/O exception occurs
     * while setting the default time-to-live value
     * @deprecated use the setTimeToLive method instead, which uses
     * <b>int</b> instead of <b>byte</b> as the type for ttl.
     */
    @Deprecated
    public void setTTL(byte ttl) throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setTTL(ttl);
    }

    /**
     * Set the default time-to-live for multicast packets sent out
     * on this {@code MulticastSocket} in order to control the
     * scope of the multicasts.
     *
     * The ttl <b>must</b> be in the range {@code  0 <= ttl <=
     * 255} or an {@code IllegalArgumentException} will be thrown.
     * Multicast packets sent with a TTL of {@code 0} are not transmitted
     * on the network but may be delivered locally.
     *
     * @param ttl
     *         the time-to-live
     *
     * @throws IOException
     *          if an I/O exception occurs while setting the
     *          default time-to-live value
     */
    public void setTimeToLive(int ttl) throws IOException {
        if (ttl < 0 || ttl > 255) {
            throw new IllegalArgumentException("ttl out of range");
        }
        if (isClosed())
            throw new SocketException("Socket is closed");
        getImpl().setTimeToLive(ttl);
    }

    /**
     * Get the default time-to-live for multicast packets sent out on
     * the socket.
     *
     * @throws IOException if an I/O exception occurs
     * while getting the default time-to-live value
     * @return the default time-to-live value
     * @deprecated use the getTimeToLive method instead, which returns
     * an <b>int</b> instead of a <b>byte</b>.
     */
    @Deprecated
    public byte getTTL() throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        return getImpl().getTTL();
    }

    /**
     * Get the default time-to-live for multicast packets sent out on
     * the socket.
     * @throws IOException if an I/O exception occurs while
     * getting the default time-to-live value
     * @return the default time-to-live value
     */
    public int getTimeToLive() throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        return getImpl().getTimeToLive();
    }

    /**
     * Joins a multicast group. Its behavior may be affected by
     * {@code setInterface} or {@code setNetworkInterface}.
     *
     * @param mcastaddr is the multicast address to join
     *
     * @throws IOException if there is an error joining, or when the address
     *            is not a multicast address, or the platform does not support
     *            multicasting
     */
    public void joinGroup(InetAddress mcastaddr) throws IOException {
        if (isClosed()) {
            throw new SocketException("Socket is closed");
        }

        checkAddress(mcastaddr, "joinGroup");

        if (!mcastaddr.isMulticastAddress()) {
            throw new SocketException("Not a multicast address");
        }

        /**
         * required for some platforms where it's not possible to join
         * a group without setting the interface first.
         */
        NetworkInterface defaultInterface = NetworkInterface.getDefault();

        if (!interfaceSet && defaultInterface != null) {
            setNetworkInterface(defaultInterface);
        }

        getImpl().join(mcastaddr);
    }

    /**
     * Leave a multicast group. Its behavior may be affected by
     * {@code setInterface} or {@code setNetworkInterface}.
     *
     * @param mcastaddr is the multicast address to leave
     * @throws IOException if there is an error leaving
     * or when the address is not a multicast address.
     */
    public void leaveGroup(InetAddress mcastaddr) throws IOException {
        if (isClosed()) {
            throw new SocketException("Socket is closed");
        }

        checkAddress(mcastaddr, "leaveGroup");

        if (!mcastaddr.isMulticastAddress()) {
            throw new SocketException("Not a multicast address");
        }

        getImpl().leave(mcastaddr);
    }

    /**
     * Joins the specified multicast group at the specified interface.
     *
     * @param mcastaddr is the multicast address to join
     * @param netIf specifies the local interface to receive multicast
     *        datagram packets, or <i>null</i> to defer to the interface set by
     *       {@link MulticastSocket#setInterface(InetAddress)} or
     *       {@link MulticastSocket#setNetworkInterface(NetworkInterface)}
     *
     * @throws IOException if there is an error joining, or when the address
     *            is not a multicast address, or the platform does not support
     *            multicasting
     * @throws IllegalArgumentException if mcastaddr is null or is a
     *          SocketAddress subclass not supported by this socket
     */
    public void joinGroup(SocketAddress mcastaddr, NetworkInterface netIf) throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");

        if (mcastaddr == null || !(mcastaddr instanceof InetSocketAddress))
            throw new IllegalArgumentException("Unsupported address type");

        checkAddress(((InetSocketAddress)mcastaddr).getAddress(), "joinGroup");

        if (!((InetSocketAddress)mcastaddr).getAddress().isMulticastAddress()) {
            throw new SocketException("Not a multicast address");
        }

        getImpl().joinGroup(mcastaddr, netIf);
    }

    /**
     * Leave a multicast group on a specified local interface.
     *
     * @param mcastaddr is the multicast address to leave
     * @param netIf specifies the local interface or <i>null</i> to defer
     *             to the interface set by
     *             {@link MulticastSocket#setInterface(InetAddress)} or
     *             {@link MulticastSocket#setNetworkInterface(NetworkInterface)}
     * @throws IOException if there is an error leaving
     * or when the address is not a multicast address.
     * @throws IllegalArgumentException if mcastaddr is null or is a
     *          SocketAddress subclass not supported by this socket
     */
    public void leaveGroup(SocketAddress mcastaddr, NetworkInterface netIf) throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");

        if (mcastaddr == null || !(mcastaddr instanceof InetSocketAddress))
            throw new IllegalArgumentException("Unsupported address type");

        checkAddress(((InetSocketAddress)mcastaddr).getAddress(), "leaveGroup");

        if (!((InetSocketAddress)mcastaddr).getAddress().isMulticastAddress()) {
            throw new SocketException("Not a multicast address");
        }

        getImpl().leaveGroup(mcastaddr, netIf);
     }

    /**
     * Set the multicast network interface used by methods
     * whose behavior would be affected by the value of the
     * network interface. Useful for multihomed hosts.
     * @param inf the InetAddress
     * @throws SocketException if there is an error in
     * the underlying protocol, such as a TCP error.
     */
    public void setInterface(InetAddress inf) throws SocketException {
        if (isClosed()) {
            throw new SocketException("Socket is closed");
        }
        checkAddress(inf, "setInterface");
        synchronized (infLock) {
            getImpl().setOption(SocketOptions.IP_MULTICAST_IF, inf);
            infAddress = inf;
            interfaceSet = true;
        }
    }

    /**
     * Retrieve the address of the network interface used for
     * multicast packets.
     *
     * @return An {@code InetAddress} representing
     *  the address of the network interface used for
     *  multicast packets.
     *
     * @throws SocketException if there is an error in
     * the underlying protocol, such as a TCP error.
     */
    public InetAddress getInterface() throws SocketException {
        if (isClosed()) {
            throw new SocketException("Socket is closed");
        }
        synchronized (infLock) {
            InetAddress ia = (InetAddress)getImpl().getOption(SocketOptions.IP_MULTICAST_IF);

            /**
             * No previous setInterface or interface can be
             * set using setNetworkInterface
             */
            if (infAddress == null) {
                return ia;
            }

            /**
             * Same interface set with setInterface?
             */
            if (ia.equals(infAddress)) {
                return ia;
            }

            /**
             * Different InetAddress from what we set with setInterface
             * so enumerate the current interface to see if the
             * address set by setInterface is bound to this interface.
             */
            try {
                NetworkInterface ni = NetworkInterface.getByInetAddress(ia);
                Enumeration<InetAddress> addrs = ni.getInetAddresses();
                while (addrs.hasMoreElements()) {
                    InetAddress addr = addrs.nextElement();
                    if (addr.equals(infAddress)) {
                        return infAddress;
                    }
                }

                /**
                 * No match so reset infAddress to indicate that the
                 * interface has changed via means
                 */
                infAddress = null;
                return ia;
            } catch (Exception e) {
                return ia;
            }
        }
    }

    /**
     * Specify the network interface for outgoing multicast datagrams
     * sent on this socket.
     *
     * @param netIf the interface
     * @throws SocketException if there is an error in
     * the underlying protocol, such as a TCP error.
     */
    public void setNetworkInterface(NetworkInterface netIf) throws SocketException {
        synchronized (infLock) {
            getImpl().setOption(SocketOptions.IP_MULTICAST_IF2, netIf);
            infAddress = null;
            interfaceSet = true;
        }
    }

    /**
     * Get the multicast network interface set.
     *
     * @throws SocketException if there is an error in
     * the underlying protocol, such as a TCP error.
     * @return the multicast {@code NetworkInterface} currently set
     */
    public NetworkInterface getNetworkInterface() throws SocketException {
        NetworkInterface ni = (NetworkInterface)getImpl().getOption(SocketOptions.IP_MULTICAST_IF2);
        if ((ni.getIndex() == 0) || (ni.getIndex() == -1)) {
            InetAddress[] addrs = new InetAddress[1];
            addrs[0] = InetAddress.anyLocalAddress();
            return new NetworkInterface(addrs[0].getHostName(), 0, addrs);
        } else {
            return ni;
        }
    }

    /**
     * Disable/Enable local loopback of multicast datagrams
     * The option is used by the platform's networking code as a hint
     * for setting whether multicast data will be looped back to
     * the local socket.
     *
     * Because this option is a hint, applications that want to
     * verify what loopback mode is set to should call
     * {@link #getLoopbackMode()}
     * @param disable {@code true} to disable the LoopbackMode
     * @throws SocketException if an error occurs while setting the value
     */
    public void setLoopbackMode(boolean disable) throws SocketException {
        getImpl().setOption(SocketOptions.IP_MULTICAST_LOOP, Boolean.valueOf(disable));
    }

    /**
     * Get the setting for local loopback of multicast datagrams.
     *
     * @throws SocketException  if an error occurs while getting the value
     * @return true if the LoopbackMode has been disabled
     */
    public boolean getLoopbackMode() throws SocketException {
        return ((Boolean)getImpl().getOption(SocketOptions.IP_MULTICAST_LOOP)).booleanValue();
    }

    /**
     * Sends a datagram packet to the destination, with a TTL (time-
     * to-live) other than the default for the socket.  This method
     * need only be used in instances where a particular TTL is desired;
     * otherwise it is preferable to set a TTL once on the socket, and
     * use that default TTL for all packets.  This method does <b>not
     * </b> alter the default TTL for the socket. Its behavior may be
     * affected by {@code setInterface}.
     *
     * @param p is the packet to be sent. The packet should contain
     * the destination multicast ip address and the data to be sent.
     * One does not need to be the member of the group to send
     * packets to a destination multicast address.
     * @param ttl optional time to live for multicast packet.
     * default ttl is 1.
     *
     * @throws IOException is raised if an error occurs i.e
     * error while setting ttl.
     *
     * @deprecated Use the following code or its equivalent instead:
     *  ......
     *  int ttl = mcastSocket.getTimeToLive();
     *  mcastSocket.setTimeToLive(newttl);
     *  mcastSocket.send(p);
     *  mcastSocket.setTimeToLive(ttl);
     *  ......
     */
    @Deprecated
    public void send(DatagramPacket p, byte ttl) throws IOException {
        if (isClosed())
            throw new SocketException("Socket is closed");
        checkAddress(p.getAddress(), "send");
        synchronized (ttlLock) {
            synchronized (p) {
                if (connectState == ST_NOT_CONNECTED) {
                } else {
                    // we're connected
                    InetAddress packetAddress = null;
                    packetAddress = p.getAddress();
                    if (packetAddress == null) {
                        p.setAddress(connectedAddress);
                        p.setPort(connectedPort);
                    } else if ((!packetAddress.equals(connectedAddress)) || p.getPort() != connectedPort) {
                        throw new SecurityException("connected address and packet address" + " differ");
                    }
                }
                byte dttl = getTTL();
                try {
                    if (ttl != dttl) {
                        // set the ttl
                        getImpl().setTTL(ttl);
                    }
                    // call the datagram method to send
                    getImpl().send(p);
                } finally {
                    // set it back to default
                    if (ttl != dttl) {
                        getImpl().setTTL(dttl);
                    }
                }
            }
        }
    }

    private static Set<SocketOption<?>> options;
    private static boolean optionsSet = false;

    @Override
    public Set<SocketOption<?>> supportedOptions() {
        synchronized (MulticastSocket.class) {
            if (optionsSet) {
                return options;
            }
            try {
                DatagramSocketImpl impl = getImpl();
                options = Collections.unmodifiableSet(impl.supportedOptions());
            } catch (SocketException ex) {
                options = Collections.emptySet();
            }
            optionsSet = true;
            return options;
        }
    }
}

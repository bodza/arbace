package java.net;

/**
 * This class represents a datagram packet.
 *
 * Datagram packets are used to implement a connectionless packet
 * delivery service. Each message is routed from one machine to
 * another based solely on information contained within that packet.
 * Multiple packets sent from one machine to another might be routed
 * differently, and might arrive in any order. Packet delivery is
 * not guaranteed.
 */
public final class DatagramPacket {
    /**
     * Perform class initialization
     */
    static {
        System.loadLibrary("net");
        init();
    }

    /*
     * The fields of this class are package-private since DatagramSocketImpl
     * classes needs to access them.
     */
    byte[] buf;
    int offset;
    int length;
    int bufLength;
    InetAddress address;
    int port;

    /**
     * Constructs a {@code DatagramPacket} for receiving packets of
     * length {@code length}, specifying an offset into the buffer.
     *
     * The {@code length} argument must be less than or equal to
     * {@code buf.length}.
     *
     * @param buf      buffer for holding the incoming datagram.
     * @param offset   the offset for the buffer
     * @param length   the number of bytes to read.
     */
    public DatagramPacket(byte buf[], int offset, int length) {
        setData(buf, offset, length);
        this.address = null;
        this.port = -1;
    }

    /**
     * Constructs a {@code DatagramPacket} for receiving packets of
     * length {@code length}.
     *
     * The {@code length} argument must be less than or equal to
     * {@code buf.length}.
     *
     * @param buf      buffer for holding the incoming datagram.
     * @param length   the number of bytes to read.
     */
    public DatagramPacket(byte buf[], int length) {
        this (buf, 0, length);
    }

    /**
     * Constructs a datagram packet for sending packets of length
     * {@code length} with offset {@code ioffset}to the
     * specified port number on the specified host. The
     * {@code length} argument must be less than or equal to
     * {@code buf.length}.
     *
     * @param buf      the packet data.
     * @param offset   the packet data offset.
     * @param length   the packet data length.
     * @param address  the destination address.
     * @param port     the destination port number.
     */
    public DatagramPacket(byte buf[], int offset, int length, InetAddress address, int port) {
        setData(buf, offset, length);
        setAddress(address);
        setPort(port);
    }

    /**
     * Constructs a datagram packet for sending packets of length
     * {@code length} with offset {@code ioffset}to the
     * specified port number on the specified host. The
     * {@code length} argument must be less than or equal to
     * {@code buf.length}.
     *
     * @param buf      the packet data.
     * @param offset   the packet data offset.
     * @param length   the packet data length.
     * @param address  the destination socket address.
     * @throws IllegalArgumentException if address type is not supported
     */
    public DatagramPacket(byte buf[], int offset, int length, SocketAddress address) {
        setData(buf, offset, length);
        setSocketAddress(address);
    }

    /**
     * Constructs a datagram packet for sending packets of length
     * {@code length} to the specified port number on the specified
     * host. The {@code length} argument must be less than or equal
     * to {@code buf.length}.
     *
     * @param buf      the packet data.
     * @param length   the packet length.
     * @param address  the destination address.
     * @param port     the destination port number.
     */
    public DatagramPacket(byte buf[], int length, InetAddress address, int port) {
        this(buf, 0, length, address, port);
    }

    /**
     * Constructs a datagram packet for sending packets of length
     * {@code length} to the specified port number on the specified
     * host. The {@code length} argument must be less than or equal
     * to {@code buf.length}.
     *
     * @param buf      the packet data.
     * @param length   the packet length.
     * @param address  the destination address.
     * @throws IllegalArgumentException if address type is not supported
     */
    public DatagramPacket(byte buf[], int length, SocketAddress address) {
        this(buf, 0, length, address);
    }

    /**
     * Returns the IP address of the machine to which this datagram is being
     * sent or from which the datagram was received.
     *
     * @return the IP address of the machine to which this datagram is being
     *          sent or from which the datagram was received.
     */
    public synchronized InetAddress getAddress() {
        return address;
    }

    /**
     * Returns the port number on the remote host to which this datagram is
     * being sent or from which the datagram was received.
     *
     * @return the port number on the remote host to which this datagram is
     *          being sent or from which the datagram was received.
     */
    public synchronized int getPort() {
        return port;
    }

    /**
     * Returns the data buffer. The data received or the data to be sent
     * starts from the {@code offset} in the buffer,
     * and runs for {@code length} long.
     *
     * @return the buffer used to receive or  send data
     */
    public synchronized byte[] getData() {
        return buf;
    }

    /**
     * Returns the offset of the data to be sent or the offset of the
     * data received.
     *
     * @return the offset of the data to be sent or the offset of the
     *          data received.
     */
    public synchronized int getOffset() {
        return offset;
    }

    /**
     * Returns the length of the data to be sent or the length of the
     * data received.
     *
     * @return the length of the data to be sent or the length of the
     *          data received.
     */
    public synchronized int getLength() {
        return length;
    }

    /**
     * Set the data buffer for this packet. This sets the
     * data, length and offset of the packet.
     *
     * @param buf the buffer to set for this packet
     *
     * @param offset the offset into the data
     *
     * @param length the length of the data
     *       and/or the length of the buffer used to receive data
     *
     * @throws NullPointerException if the argument is null
     */
    public synchronized void setData(byte[] buf, int offset, int length) {
        /* this will check to see if buf is null */
        if (length < 0 || offset < 0 || (length + offset) < 0 || ((length + offset) > buf.length)) {
            throw new IllegalArgumentException("illegal length or offset");
        }
        this.buf = buf;
        this.length = length;
        this.bufLength = length;
        this.offset = offset;
    }

    /**
     * Sets the IP address of the machine to which this datagram
     * is being sent.
     * @param iaddr the {@code InetAddress}
     */
    public synchronized void setAddress(InetAddress iaddr) {
        address = iaddr;
    }

    /**
     * Sets the port number on the remote host to which this datagram
     * is being sent.
     * @param iport the port number
     */
    public synchronized void setPort(int iport) {
        if (iport < 0 || iport > 0xFFFF) {
            throw new IllegalArgumentException("Port out of range:"+ iport);
        }
        port = iport;
    }

    /**
     * Sets the SocketAddress (usually IP address + port number) of the remote
     * host to which this datagram is being sent.
     *
     * @param address the {@code SocketAddress}
     * @throws IllegalArgumentException if address is null or is a
     *          SocketAddress subclass not supported by this socket
     */
    public synchronized void setSocketAddress(SocketAddress address) {
        if (address == null || !(address instanceof InetSocketAddress))
            throw new IllegalArgumentException("unsupported address type");
        InetSocketAddress addr = (InetSocketAddress) address;
        if (addr.isUnresolved())
            throw new IllegalArgumentException("unresolved address");
        setAddress(addr.getAddress());
        setPort(addr.getPort());
    }

    /**
     * Gets the SocketAddress (usually IP address + port number) of the remote
     * host that this packet is being sent to or is coming from.
     *
     * @return the {@code SocketAddress}
     */
    public synchronized SocketAddress getSocketAddress() {
        return new InetSocketAddress(getAddress(), getPort());
    }

    /**
     * Set the data buffer for this packet. With the offset of
     * this DatagramPacket set to 0, and the length set to
     * the length of {@code buf}.
     *
     * @param buf the buffer to set for this packet.
     *
     * @throws NullPointerException if the argument is null.
     */
    public synchronized void setData(byte[] buf) {
        if (buf == null) {
            throw new NullPointerException("null packet buffer");
        }
        this.buf = buf;
        this.offset = 0;
        this.length = buf.length;
        this.bufLength = buf.length;
    }

    /**
     * Set the length for this packet. The length of the packet is
     * the number of bytes from the packet's data buffer that will be
     * sent, or the number of bytes of the packet's data buffer that
     * will be used for receiving data. The length must be lesser or
     * equal to the offset plus the length of the packet's buffer.
     *
     * @param length the length to set for this packet.
     *
     * @throws IllegalArgumentException if the length is negative
     * of if the length is greater than the packet's data buffer
     * length.
     */
    public synchronized void setLength(int length) {
        if ((length + offset) > buf.length || length < 0 || (length + offset) < 0) {
            throw new IllegalArgumentException("illegal length");
        }
        this.length = length;
        this.bufLength = this.length;
    }

    /**
     * Perform class load-time initializations.
     */
    private static native void init();
}

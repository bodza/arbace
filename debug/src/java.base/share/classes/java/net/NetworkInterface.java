package java.net;

import java.util.Arrays;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;

/**
 * This class represents a Network Interface made up of a name,
 * and a list of IP addresses assigned to this interface.
 * It is used to identify the local interface on which a multicast group
 * is joined.
 *
 * Interfaces are normally known by names such as "le0".
 */
public final class NetworkInterface {
    private String name;
    private String displayName;
    private int index;
    private InetAddress addrs[];
    private InterfaceAddress bindings[];
    private NetworkInterface childs[];
    private NetworkInterface parent = null;
    private boolean virtual = false;
    private static final NetworkInterface defaultInterface;
    private static final int defaultIndex; /* index of defaultInterface */

    static {
        System.loadLibrary("net");

        init();
        defaultInterface = DefaultInterface.getDefault();
        if (defaultInterface != null) {
            defaultIndex = defaultInterface.getIndex();
        } else {
            defaultIndex = 0;
        }
    }

    /**
     * Returns an NetworkInterface object with index set to 0 and name to null.
     * Setting such an interface on a MulticastSocket will cause the
     * kernel to choose one interface for sending multicast packets.
     */
    NetworkInterface() {
    }

    NetworkInterface(String name, int index, InetAddress[] addrs) {
        this.name = name;
        this.index = index;
        this.addrs = addrs;
    }

    /**
     * Get the name of this network interface.
     *
     * @return the name of this network interface
     */
    public String getName() {
            return name;
    }

    /**
     * Get an Enumeration with all or a subset of the InetAddresses bound to
     * this network interface.
     *
     * @return an Enumeration object with all or a subset of the InetAddresses
     * bound to this network interface
     */
    public Enumeration<InetAddress> getInetAddresses() {
        return enumerationFromArray(getCheckedInetAddresses());
    }

    /**
     * Get a Stream of all or a subset of the InetAddresses bound to this
     * network interface.
     *
     * @return a Stream object with all or a subset of the InetAddresses
     * bound to this network interface
     */
    public Stream<InetAddress> inetAddresses() {
        return streamFromArray(getCheckedInetAddresses());
    }

    private InetAddress[] getCheckedInetAddresses() {
        InetAddress[] local_addrs = new InetAddress[addrs.length];
        boolean trusted = true;

        int i = 0;
        for (int j = 0; j < addrs.length; j++) {
            local_addrs[i++] = addrs[j];
        }
        return Arrays.copyOf(local_addrs, i);
    }

    /**
     * Get a List of all or a subset of the {@code InterfaceAddresses}
     * of this network interface.
     *
     * @return a {@code List} object with all or a subset of the
     *         InterfaceAddresss of this network interface
     */
    public java.util.List<InterfaceAddress> getInterfaceAddresses() {
        java.util.List<InterfaceAddress> lst = new java.util.ArrayList<>(1);
        if (bindings != null) {
            for (int j = 0; j<bindings.length; j++) {
                lst.add(bindings[j]);
            }
        }
        return lst;
    }

    /**
     * Get an Enumeration with all the subinterfaces (also known as virtual
     * interfaces) attached to this network interface.
     *
     * For instance eth0:1 will be a subinterface to eth0.
     *
     * @return an Enumeration object with all of the subinterfaces
     * of this network interface
     */
    public Enumeration<NetworkInterface> getSubInterfaces() {
        return enumerationFromArray(childs);
    }

    /**
     * Get a Stream of all subinterfaces (also known as virtual
     * interfaces) attached to this network interface.
     *
     * @return a Stream object with all of the subinterfaces
     * of this network interface
     */
    public Stream<NetworkInterface> subInterfaces() {
        return streamFromArray(childs);
    }

    /**
     * Returns the parent NetworkInterface of this interface if this is
     * a subinterface, or {@code null} if it is a physical
     * (non virtual) interface or has no parent.
     *
     * @return The {@code NetworkInterface} this interface is attached to.
     */
    public NetworkInterface getParent() {
        return parent;
    }

    /**
     * Returns the index of this network interface. The index is an integer greater
     * or equal to zero, or {@code -1} for unknown. This is a system specific value
     * and interfaces with the same name can have different indexes on different
     * machines.
     *
     * @return the index of this network interface or {@code -1} if the index is
     *         unknown
     */
    public int getIndex() {
        return index;
    }

    /**
     * Get the display name of this network interface.
     * A display name is a human readable String describing the network
     * device.
     *
     * @return a non-empty string representing the display name of this network
     *         interface, or null if no display name is available.
     */
    public String getDisplayName() {
        /* strict TCK conformance */
        return "".equals(displayName) ? null : displayName;
    }

    /**
     * Searches for the network interface with the specified name.
     *
     * @param name
     *          The name of the network interface.
     *
     * @return A {@code NetworkInterface} with the specified name,
     *          or {@code null} if there is no network interface
     *          with the specified name.
     *
     * @throws SocketException
     *          If an I/O error occurs.
     *
     * @throws NullPointerException
     *          If the specified name is {@code null}.
     */
    public static NetworkInterface getByName(String name) throws SocketException {
        if (name == null)
            throw new NullPointerException();
        return getByName0(name);
    }

    /**
     * Get a network interface given its index.
     *
     * @param index an integer, the index of the interface
     * @return the NetworkInterface obtained from its index, or {@code null} if
     *         there is no interface with such an index on the system
     * @throws SocketException  if an I/O error occurs.
     * @throws IllegalArgumentException if index has a negative value
     */
    public static NetworkInterface getByIndex(int index) throws SocketException {
        if (index < 0)
            throw new IllegalArgumentException("Interface index can't be negative");
        return getByIndex0(index);
    }

    /**
     * Convenience method to search for a network interface that
     * has the specified Internet Protocol (IP) address bound to
     * it.
     *
     * If the specified IP address is bound to multiple network
     * interfaces it is not defined which network interface is
     * returned.
     *
     * @param addr
     *          The {@code InetAddress} to search with.
     *
     * @return A {@code NetworkInterface}
     *          or {@code null} if there is no network interface
     *          with the specified IP address.
     *
     * @throws SocketException
     *          If an I/O error occurs.
     *
     * @throws NullPointerException
     *          If the specified address is {@code null}.
     */
    public static NetworkInterface getByInetAddress(InetAddress addr) throws SocketException {
        if (addr == null) {
            throw new NullPointerException();
        }
        if (addr instanceof Inet4Address) {
            Inet4Address inet4Address = (Inet4Address) addr;
            if (inet4Address.holder.family != InetAddress.IPv4) {
                throw new IllegalArgumentException("invalid family type: " + inet4Address.holder.family);
            }
        } else if (addr instanceof Inet6Address) {
            Inet6Address inet6Address = (Inet6Address) addr;
            if (inet6Address.holder.family != InetAddress.IPv6) {
                throw new IllegalArgumentException("invalid family type: " + inet6Address.holder.family);
            }
        } else {
            throw new IllegalArgumentException("invalid address type: " + addr);
        }
        return getByInetAddress0(addr);
    }

    /**
     * Returns an {@code Enumeration} of all the interfaces on this machine. The
     * {@code Enumeration} contains at least one element, possibly representing
     * a loopback interface that only supports communication between entities on
     * this machine.
     *
     * @apiNote this method can be used in combination with
     * {@link #getInetAddresses()} to obtain all IP addresses for this node
     *
     * @return an Enumeration of NetworkInterfaces found on this machine
     * @throws SocketException  if an I/O error occurs,
     *             or if the platform does not have at least one configured
     *             network interface.
     */
    public static Enumeration<NetworkInterface> getNetworkInterfaces() throws SocketException {
        NetworkInterface[] netifs = getAll();
        if (netifs != null && netifs.length > 0) {
            return enumerationFromArray(netifs);
        } else {
            throw new SocketException("No network interfaces configured");
        }
    }

    /**
     * Returns a {@code Stream} of all the interfaces on this machine.  The
     * {@code Stream} contains at least one interface, possibly representing a
     * loopback interface that only supports communication between entities on
     * this machine.
     *
     * @apiNote this method can be used in combination with
     * {@link #inetAddresses()}} to obtain a stream of all IP addresses for
     * this node, for example:
     * <pre> {@code
     * Stream<InetAddress> addrs = NetworkInterface.networkInterfaces()
     *     .flatMap(NetworkInterface::inetAddresses);
     * }</pre>
     *
     * @return a Stream of NetworkInterfaces found on this machine
     * @throws SocketException  if an I/O error occurs,
     *             or if the platform does not have at least one configured
     *             network interface.
     */
    public static Stream<NetworkInterface> networkInterfaces() throws SocketException {
        NetworkInterface[] netifs = getAll();
        if (netifs != null && netifs.length > 0) {
            return streamFromArray(netifs);
        }  else {
            throw new SocketException("No network interfaces configured");
        }
    }

    private static <T> Enumeration<T> enumerationFromArray(T[] a) {
        return new Enumeration<>() {
            int i = 0;

            @Override
            public T nextElement() {
                if (i < a.length) {
                    return a[i++];
                } else {
                    throw new NoSuchElementException();
                }
            }

            @Override
            public boolean hasMoreElements() {
                return i < a.length;
            }
        };
    }

    private static <T> Stream<T> streamFromArray(T[] a) {
        return StreamSupport.stream(Spliterators.spliterator(a, Spliterator.DISTINCT | Spliterator.IMMUTABLE | Spliterator.NONNULL), false);
    }

    private static native NetworkInterface[] getAll() throws SocketException;

    private static native NetworkInterface getByName0(String name) throws SocketException;

    private static native NetworkInterface getByIndex0(int index) throws SocketException;

    private static native NetworkInterface getByInetAddress0(InetAddress addr) throws SocketException;

    /**
     * Returns whether a network interface is up and running.
     *
     * @return {@code true} if the interface is up and running.
     * @throws SocketException if an I/O error occurs.
     */
    public boolean isUp() throws SocketException {
        return isUp0(name, index);
    }

    /**
     * Returns whether a network interface is a loopback interface.
     *
     * @return {@code true} if the interface is a loopback interface.
     * @throws SocketException if an I/O error occurs.
     */
    public boolean isLoopback() throws SocketException {
        return isLoopback0(name, index);
    }

    /**
     * Returns whether a network interface is a point to point interface.
     * A typical point to point interface would be a PPP connection through
     * a modem.
     *
     * @return {@code true} if the interface is a point to point
     *          interface.
     * @throws SocketException if an I/O error occurs.
     */
    public boolean isPointToPoint() throws SocketException {
        return isP2P0(name, index);
    }

    /**
     * Returns whether a network interface supports multicasting or not.
     *
     * @return {@code true} if the interface supports Multicasting.
     * @throws SocketException if an I/O error occurs.
     */
    public boolean supportsMulticast() throws SocketException {
        return supportsMulticast0(name, index);
    }

    /**
     * Returns the hardware address (usually MAC) of the interface if it
     * has one and if it can be accessed given the current privileges.
     *
     * @return a byte array containing the address, or {@code null} if
     *          the address doesn't exist, or is not accessible
     *
     * @throws SocketException if an I/O error occurs.
     */
    public byte[] getHardwareAddress() throws SocketException {
        for (InetAddress addr : addrs) {
            if (addr instanceof Inet4Address) {
                return getMacAddr0(((Inet4Address)addr).getAddress(), name, index);
            }
        }
        return getMacAddr0(null, name, index);
    }

    /**
     * Returns the Maximum Transmission Unit (MTU) of this interface.
     *
     * @return the value of the MTU for that interface.
     * @throws SocketException if an I/O error occurs.
     */
    public int getMTU() throws SocketException {
        return getMTU0(name, index);
    }

    /**
     * Returns whether this interface is a virtual interface (also called
     * subinterface).
     * Virtual interfaces are, on some systems, interfaces created as a child
     * of a physical interface and given different settings (like address or
     * MTU). Usually the name of the interface will the name of the parent
     * followed by a colon (:) and a number identifying the child since there
     * can be several virtual interfaces attached to a single physical
     * interface.
     *
     * @return {@code true} if this interface is a virtual interface.
     */
    public boolean isVirtual() {
        return virtual;
    }

    private static native boolean isUp0(String name, int ind) throws SocketException;
    private static native boolean isLoopback0(String name, int ind) throws SocketException;
    private static native boolean supportsMulticast0(String name, int ind) throws SocketException;
    private static native boolean isP2P0(String name, int ind) throws SocketException;
    private static native byte[] getMacAddr0(byte[] inAddr, String name, int ind) throws SocketException;
    private static native int getMTU0(String name, int ind) throws SocketException;

    /**
     * Compares this object against the specified object.
     * The result is {@code true} if and only if the argument is
     * not {@code null} and it represents the same NetworkInterface
     * as this object.
     *
     * Two instances of {@code NetworkInterface} represent the same
     * NetworkInterface if both name and addrs are the same for both.
     *
     * @param obj   the object to compare against.
     * @return {@code true} if the objects are the same;
     *          {@code false} otherwise.
     */
    public boolean equals(Object obj) {
        if (!(obj instanceof NetworkInterface)) {
            return false;
        }
        NetworkInterface that = (NetworkInterface)obj;
        if (this.name != null) {
            if (!this.name.equals(that.name)) {
                return false;
            }
        } else {
            if (that.name != null) {
                return false;
            }
        }

        if (this.addrs == null) {
            return that.addrs == null;
        } else if (that.addrs == null) {
            return false;
        }

        /* Both addrs not null. Compare number of addresses */

        if (this.addrs.length != that.addrs.length) {
            return false;
        }

        InetAddress[] thatAddrs = that.addrs;
        int count = thatAddrs.length;

        for (int i = 0; i < count; i++) {
            boolean found = false;
            for (int j = 0; j<count; j++) {
                if (addrs[i].equals(thatAddrs[j])) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return true;
    }

    public int hashCode() {
        return name == null ? 0 : name.hashCode();
    }

    public String toString() {
        String result = "name:";
        result += name == null ? "null" : name;
        if (displayName != null) {
            result += " (" + displayName + ")";
        }
        return result;
    }

    private static native void init();

    /**
     * Returns the default network interface of this system
     *
     * @return the default interface
     */
    static NetworkInterface getDefault() {
        return defaultInterface;
    }
}

package java.net;

import java.util.NavigableSet;
import java.util.ArrayList;
import java.util.Objects;
import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicLong;

import sun.net.InetAddressCachePolicy;
import sun.net.util.IPAddressUtil;

/**
 * This class represents an Internet Protocol (IP) address.
 *
 * An IP address is either a 32-bit or 128-bit unsigned number
 * used by IP, a lower-level protocol on which protocols like UDP and
 * TCP are built. The IP address architecture is defined by
 * <a href="http://www.ietf.org/rfc/rfc790.txt"><i>RFC&nbsp;790: Assigned Numbers</i></a>,
 * <a href="http://www.ietf.org/rfc/rfc1918.txt"> <i>RFC&nbsp;1918: Address Allocation for Private Internets</i></a>,
 * <a href="http://www.ietf.org/rfc/rfc2365.txt"><i>RFC&nbsp;2365: Administratively Scoped IP Multicast</i></a>, and
 * <a href="http://www.ietf.org/rfc/rfc2373.txt"><i>RFC&nbsp;2373: IP Version 6 Addressing Architecture</i></a>.
 * An instance of an InetAddress consists of an IP address and possibly its
 * corresponding host name (depending on whether it is constructed
 * with a host name or whether it has already done reverse host name
 * resolution).
 *
 * <h3>Address types</h3>
 *
 * <table class="striped" style="margin-left:2em">
 *   <caption style="display:none">Description of unicast and multicast address types</caption>
 *   <thead>
 *   <tr><th scope="col">Address Type</th><th scope="col">Description</th></tr>
 *   </thead>
 *   <tbody>
 *   <tr><th scope="row" style="vertical-align:top">unicast</th>
 *       <td>An identifier for a single interface. A packet sent to
 *         a unicast address is delivered to the interface identified by
 *         that address.
 *
 *         The Unspecified Address -- Also called anylocal or wildcard
 *         address. It must never be assigned to any node. It indicates the
 *         absence of an address. One example of its use is as the target of
 *         bind, which allows a server to accept a client connection on any
 *         interface, in case the server host has multiple interfaces.
 *
 *         The <i>unspecified</i> address must not be used as
 *         the destination address of an IP packet.
 *
 *         The <i>Loopback</i> Addresses -- This is the address
 *         assigned to the loopback interface. Anything sent to this
 *         IP address loops around and becomes IP input on the local
 *         host. This address is often used when testing a
 *         client.</td></tr>
 *   <tr><th scope="row" style="vertical-align:top">multicast</th>
 *       <td>An identifier for a set of interfaces (typically belonging
 *         to different nodes). A packet sent to a multicast address is
 *         delivered to all interfaces identified by that address.</td></tr>
 * </tbody>
 * </table>
 *
 * <h4>IP address scope</h4>
 *
 * <i>Link-local</i> addresses are designed to be used for addressing
 * on a single link for purposes such as auto-address configuration,
 * neighbor discovery, or when no routers are present.
 *
 * <i>Site-local</i> addresses are designed to be used for addressing
 * inside of a site without the need for a global prefix.
 *
 * <i>Global</i> addresses are unique across the internet.
 *
 * <h4>Textual representation of IP addresses</h4>
 *
 * The textual representation of an IP address is address family specific.
 *
 * For IPv4 address format, please refer to <a HREF="Inet4Address.html#format">Inet4Address#format</a>;
 * For IPv6 address format, please refer to <a HREF="Inet6Address.html#format">Inet6Address#format</a>.
 *
 * <h4>Host Name Resolution</h4>
 *
 * Host name-to-IP address <i>resolution</i> is accomplished through
 * the use of a combination of local machine configuration information
 * and network naming services such as the Domain Name System (DNS)
 * and Network Information Service(NIS). The particular naming
 * services(s) being used is by default the local machine configured
 * one. For any host name, its corresponding IP address is returned.
 *
 * <i>Reverse name resolution</i> means that for any IP address,
 * the host associated with the IP address is returned.
 *
 * The InetAddress class provides methods to resolve host names to
 * their IP addresses and vice versa.
 *
 * <h4>InetAddress Caching</h4>
 *
 * The InetAddress class has a cache to store successful as well as
 * unsuccessful host name resolutions.
 *
 * By default, when a security manager is installed, in order to
 * protect against DNS spoofing attacks,
 * the result of positive host name resolutions are
 * cached forever. When a security manager is not installed, the default
 * behavior is to cache entries for a finite (implementation dependent)
 * period of time. The result of unsuccessful host
 * name resolution is cached for a very short period of time (10
 * seconds) to improve performance.
 *
 * If the default behavior is not desired, then a Java security property
 * can be set to a different Time-to-live (TTL) value for positive
 * caching. Likewise, a system admin can configure a different
 * negative caching TTL value when needed.
 *
 * Two Java security properties control the TTL values used for
 *  positive and negative host name resolution caching:
 *
 * <dl style="margin-left:2em">
 * <dt><b>networkaddress.cache.ttl</b></dt>
 * <dd>Indicates the caching policy for successful name lookups from
 * the name service. The value is specified as an integer to indicate
 * the number of seconds to cache the successful lookup. The default
 * setting is to cache for an implementation specific period of time.
 *
 * A value of -1 indicates "cache forever".
 * </dd>
 * <dt><b>networkaddress.cache.negative.ttl</b> (default: 10)</dt>
 * <dd>Indicates the caching policy for un-successful name lookups
 * from the name service. The value is specified as an integer to
 * indicate the number of seconds to cache the failure for
 * un-successful lookups.
 *
 * A value of 0 indicates "never cache".
 * A value of -1 indicates "cache forever".
 * </dd>
 * </dl>
 */
public class InetAddress {
    // @Native
    static final int PREFER_IPV4_VALUE = 0;
    // @Native
    static final int PREFER_IPV6_VALUE = 1;
    // @Native
    static final int PREFER_SYSTEM_VALUE = 2;

    /**
     * Specify the address family: Internet Protocol, Version 4
     */
    // @Native
    static final int IPv4 = 1;

    /**
     * Specify the address family: Internet Protocol, Version 6
     */
    // @Native
    static final int IPv6 = 2;

    /* Specify address family preference */
    static transient final int preferIPv6Address;

    static class InetAddressHolder {
        /**
         * Reserve the original application specified hostname.
         *
         * The original hostname is useful for domain-based endpoint
         * identification (see RFC 2818 and RFC 6125).  If an address
         * was created with a raw IP address, a reverse name lookup
         * may introduce endpoint identification security issue via
         * DNS forging.
         *
         * Oracle JSSE provider is using this original hostname, via
         * jdk.internal.misc.JavaNetAccess, for SSL/TLS endpoint identification.
         *
         * Note: May define a new public method in the future if necessary.
         */
        String originalHostName;

        InetAddressHolder() { }

        InetAddressHolder(String hostName, int address, int family) {
            this.originalHostName = hostName;
            this.hostName = hostName;
            this.address = address;
            this.family = family;
        }

        void init(String hostName, int family) {
            this.originalHostName = hostName;
            this.hostName = hostName;
            if (family != -1) {
                this.family = family;
            }
        }

        String hostName;

        String getHostName() {
            return hostName;
        }

        String getOriginalHostName() {
            return originalHostName;
        }

        /**
         * Holds a 32-bit IPv4 address.
         */
        int address;

        int getAddress() {
            return address;
        }

        /**
         * Specifies the address family type, for instance, '1' for IPv4
         * addresses, and '2' for IPv6 addresses.
         */
        int family;

        int getFamily() {
            return family;
        }
    }

    /* Used to store the serializable fields of InetAddress */
    final transient InetAddressHolder holder;

    InetAddressHolder holder() {
        return holder;
    }

    /* Used to store the name service provider */
    private static transient NameService nameService = null;

    /**
     * Used to store the best available hostname.
     * Lazily initialized via a data race; safe because Strings are immutable.
     */
    private transient String canonicalHostName = null;

    /*
     * Load net library into runtime, and perform initializations.
     */
    static {
        String str = null; // "java.net.preferIPv6Addresses"
        if (str == null) {
            preferIPv6Address = PREFER_IPV4_VALUE;
        } else if (str.equalsIgnoreCase("true")) {
            preferIPv6Address = PREFER_IPV6_VALUE;
        } else if (str.equalsIgnoreCase("false")) {
            preferIPv6Address = PREFER_IPV4_VALUE;
        } else if (str.equalsIgnoreCase("system")) {
            preferIPv6Address = PREFER_SYSTEM_VALUE;
        } else {
            preferIPv6Address = PREFER_IPV4_VALUE;
        }
        System.loadLibrary("net");
        init();
    }

    /**
     * Constructor for the Socket.accept() method.
     * This creates an empty InetAddress, which is filled in by
     * the accept() method.  This InetAddress, however, is not
     * put in the address cache, since it is not created by name.
     */
    InetAddress() {
        holder = new InetAddressHolder();
    }

    /**
     * Utility routine to check if the InetAddress is an
     * IP multicast address.
     * @return a {@code boolean} indicating if the InetAddress is
     * an IP multicast address
     */
    public boolean isMulticastAddress() {
        return false;
    }

    /**
     * Utility routine to check if the InetAddress is a wildcard address.
     * @return a {@code boolean} indicating if the Inetaddress is
     *         a wildcard address.
     */
    public boolean isAnyLocalAddress() {
        return false;
    }

    /**
     * Utility routine to check if the InetAddress is a loopback address.
     *
     * @return a {@code boolean} indicating if the InetAddress is
     * a loopback address; or false otherwise.
     */
    public boolean isLoopbackAddress() {
        return false;
    }

    /**
     * Utility routine to check if the InetAddress is an link local address.
     *
     * @return a {@code boolean} indicating if the InetAddress is
     * a link local address; or false if address is not a link local unicast address.
     */
    public boolean isLinkLocalAddress() {
        return false;
    }

    /**
     * Utility routine to check if the InetAddress is a site local address.
     *
     * @return a {@code boolean} indicating if the InetAddress is
     * a site local address; or false if address is not a site local unicast address.
     */
    public boolean isSiteLocalAddress() {
        return false;
    }

    /**
     * Utility routine to check if the multicast address has global scope.
     *
     * @return a {@code boolean} indicating if the address has
     *         is a multicast address of global scope, false if it is not
     *         of global scope or it is not a multicast address
     */
    public boolean isMCGlobal() {
        return false;
    }

    /**
     * Utility routine to check if the multicast address has node scope.
     *
     * @return a {@code boolean} indicating if the address has
     *         is a multicast address of node-local scope, false if it is not
     *         of node-local scope or it is not a multicast address
     */
    public boolean isMCNodeLocal() {
        return false;
    }

    /**
     * Utility routine to check if the multicast address has link scope.
     *
     * @return a {@code boolean} indicating if the address has
     *         is a multicast address of link-local scope, false if it is not
     *         of link-local scope or it is not a multicast address
     */
    public boolean isMCLinkLocal() {
        return false;
    }

    /**
     * Utility routine to check if the multicast address has site scope.
     *
     * @return a {@code boolean} indicating if the address has
     *         is a multicast address of site-local scope, false if it is not
     *         of site-local scope or it is not a multicast address
     */
    public boolean isMCSiteLocal() {
        return false;
    }

    /**
     * Utility routine to check if the multicast address has organization scope.
     *
     * @return a {@code boolean} indicating if the address has
     *         is a multicast address of organization-local scope,
     *         false if it is not of organization-local scope
     *         or it is not a multicast address
     */
    public boolean isMCOrgLocal() {
        return false;
    }

    /**
     * Test whether that address is reachable. Best effort is made by the
     * implementation to try to reach the host, but firewalls and server
     * configuration may block requests resulting in a unreachable status
     * while some specific ports may be accessible.
     * A typical implementation will use ICMP ECHO REQUESTs if the
     * privilege can be obtained, otherwise it will try to establish
     * a TCP connection on port 7 (Echo) of the destination host.
     *
     * The timeout value, in milliseconds, indicates the maximum amount of time
     * the try should take. If the operation times out before getting an
     * answer, the host is deemed unreachable. A negative value will result
     * in an IllegalArgumentException being thrown.
     *
     * @param timeout the time, in milliseconds, before the call aborts
     * @return a {@code boolean} indicating if the address is reachable.
     * @throws IOException if a network error occurs
     * @throws IllegalArgumentException if {@code timeout} is negative.
     */
    public boolean isReachable(int timeout) throws IOException {
        return isReachable(null, 0, timeout);
    }

    /**
     * Test whether that address is reachable. Best effort is made by the
     * implementation to try to reach the host, but firewalls and server
     * configuration may block requests resulting in a unreachable status
     * while some specific ports may be accessible.
     * A typical implementation will use ICMP ECHO REQUESTs if the
     * privilege can be obtained, otherwise it will try to establish
     * a TCP connection on port 7 (Echo) of the destination host.
     *
     * The {@code network interface} and {@code ttl} parameters
     * let the caller specify which network interface the test will go through
     * and the maximum number of hops the packets should go through.
     * A negative value for the {@code ttl} will result in an
     * IllegalArgumentException being thrown.
     *
     * The timeout value, in milliseconds, indicates the maximum amount of time
     * the try should take. If the operation times out before getting an
     * answer, the host is deemed unreachable. A negative value will result
     * in an IllegalArgumentException being thrown.
     *
     * @param netif   the NetworkInterface through which the
     *                    test will be done, or null for any interface
     * @param ttl     the maximum numbers of hops to try or 0 for the
     *                  default
     * @param timeout the time, in milliseconds, before the call aborts
     * @throws IllegalArgumentException if either {@code timeout}
     *                          or {@code ttl} are negative.
     * @return a {@code boolean}indicating if the address is reachable.
     * @throws IOException if a network error occurs
     */
    public boolean isReachable(NetworkInterface netif, int ttl, int timeout) throws IOException {
        if (ttl < 0)
            throw new IllegalArgumentException("ttl can't be negative");
        if (timeout < 0)
            throw new IllegalArgumentException("timeout can't be negative");

        return impl.isReachable(this, timeout, netif, ttl);
    }

    /**
     * Gets the host name for this IP address.
     *
     * If this InetAddress was created with a host name,
     * this host name will be remembered and returned;
     * otherwise, a reverse name lookup will be performed
     * and the result will be returned based on the system
     * configured name lookup service. If a lookup of the name service
     * is required, call
     * {@link #getCanonicalHostName() getCanonicalHostName}.
     *
     * @return the host name for this IP address, or if the operation
     *    is not allowed by the security check, the textual
     *    representation of the IP address.
     */
    public String getHostName() {
        return getHostName(true);
    }

    /**
     * Returns the hostname for this address.
     * If the host is equal to null, then this address refers to any
     * of the local machine's available network addresses.
     *
     * @return the host name for this IP address, or if the operation
     *    is not allowed by the security check, the textual
     *    representation of the IP address.
     *
     * @param check make security check if true
     */
    String getHostName(boolean check) {
        if (holder().getHostName() == null) {
            holder().hostName = InetAddress.getHostFromNameService(this, check);
        }
        return holder().getHostName();
    }

    /**
     * Gets the fully qualified domain name for this IP address.
     * Best effort method, meaning we may not be able to return
     * the FQDN depending on the underlying system configuration.
     *
     * @return the fully qualified domain name for this IP address,
     *    or if the operation is not allowed by the security check,
     *    the textual representation of the IP address.
     */
    public String getCanonicalHostName() {
        String value = canonicalHostName;
        if (value == null)
            canonicalHostName = value = InetAddress.getHostFromNameService(this, true);
        return value;
    }

    /**
     * Returns the hostname for this address.
     *
     * @return the host name for this IP address, or if the operation
     *    is not allowed by the security check, the textual
     *    representation of the IP address.
     *
     * @param check make security check if true
     */
    private static String getHostFromNameService(InetAddress addr, boolean check) {
        String host = null;
            try {
                // first lookup the hostname
                host = nameService.getHostByAddr(addr.getAddress());

                /* now get all the IP addresses for this hostname,
                 * and make sure one of them matches the original IP
                 * address. We do this to try and prevent spoofing.
                 */

                InetAddress[] arr = InetAddress.getAllByName0(host, check);
                boolean ok = false;

                if (arr != null) {
                    for (int i = 0; !ok && i < arr.length; i++) {
                        ok = addr.equals(arr[i]);
                    }
                }

                // XXX: if it looks a spoof just return the address?
                if (!ok) {
                    host = addr.getHostAddress();
                    return host;
                }
            } catch (SecurityException e) {
                host = addr.getHostAddress();
            } catch (UnknownHostException e) {
                host = addr.getHostAddress();
                // let next provider resolve the hostname
            }
        return host;
    }

    /**
     * Returns the raw IP address of this {@code InetAddress}
     * object. The result is in network byte order: the highest order
     * byte of the address is in {@code getAddress()[0]}.
     *
     * @return the raw IP address of this object.
     */
    public byte[] getAddress() {
        return null;
    }

    /**
     * Returns the IP address string in textual presentation.
     *
     * @return the raw IP address in a string format.
     */
    public String getHostAddress() {
        return null;
     }

    /**
     * Returns a hashcode for this IP address.
     *
     * @return a hash code value for this IP address.
     */
    public int hashCode() {
        return -1;
    }

    /**
     * Compares this object against the specified object.
     * The result is {@code true} if and only if the argument is
     * not {@code null} and it represents the same IP address as
     * this object.
     *
     * Two instances of {@code InetAddress} represent the same IP
     * address if the length of the byte arrays returned by
     * {@code getAddress} is the same for both, and each of the
     * array components is the same for the byte arrays.
     *
     * @param obj   the object to compare against.
     * @return {@code true} if the objects are the same;
     *          {@code false} otherwise.
     */
    public boolean equals(Object obj) {
        return false;
    }

    /**
     * Converts this IP address to a {@code String}. The
     * string returned is of the form: hostname / literal IP
     * address.
     *
     * If the host name is unresolved, no reverse name service lookup
     * is performed. The hostname part will be represented by an empty string.
     *
     * @return a string representation of this IP address.
     */
    public String toString() {
        String hostName = holder().getHostName();
        return Objects.toString(hostName, "") + "/" + getHostAddress();
    }

    // mapping from host name to Addresses - either NameServiceAddresses (while
    // still being looked-up by NameService(s)) or CachedAddresses when cached
    private static final ConcurrentMap<String, Addresses> cache = new ConcurrentHashMap<>();

    // CachedAddresses that have to expire are kept ordered in this NavigableSet
    // which is scanned on each access
    private static final NavigableSet<CachedAddresses> expirySet = new ConcurrentSkipListSet<>();

    // common interface
    private interface Addresses {
        InetAddress[] get() throws UnknownHostException;
    }

    // a holder for cached addresses with required metadata
    private static final class CachedAddresses  implements Addresses, Comparable<CachedAddresses> {
        private static final AtomicLong seq = new AtomicLong();
        final String host;
        final InetAddress[] inetAddresses;
        final long expiryTime; // time of expiry (in terms of System.nanoTime())
        final long id = seq.incrementAndGet(); // each instance is unique

        CachedAddresses(String host, InetAddress[] inetAddresses, long expiryTime) {
            this.host = host;
            this.inetAddresses = inetAddresses;
            this.expiryTime = expiryTime;
        }

        @Override
        public InetAddress[] get() throws UnknownHostException {
            if (inetAddresses == null) {
                throw new UnknownHostException(host);
            }
            return inetAddresses;
        }

        @Override
        public int compareTo(CachedAddresses other) {
            // natural order is expiry time -
            // compare difference of expiry times rather than
            // expiry times directly, to avoid possible overflow.
            // (see System.nanoTime() recommendations...)
            long diff = this.expiryTime - other.expiryTime;
            if (diff < 0L) return -1;
            if (diff > 0L) return 1;
            // ties are broken using unique id
            return Long.compare(this.id, other.id);
        }
    }

    // a name service lookup based Addresses implementation which replaces itself
    // in cache when the result is obtained
    private static final class NameServiceAddresses implements Addresses {
        private final String host;
        private final InetAddress reqAddr;

        NameServiceAddresses(String host, InetAddress reqAddr) {
            this.host = host;
            this.reqAddr = reqAddr;
        }

        @Override
        public InetAddress[] get() throws UnknownHostException {
            Addresses addresses;
            // only one thread is doing lookup to name service
            // for particular host at any time.
            synchronized (this) {
                // re-check that we are still us + re-install us if slot empty
                addresses = cache.putIfAbsent(host, this);
                if (addresses == null) {
                    // this can happen when we were replaced by CachedAddresses in
                    // some other thread, then CachedAddresses expired and were
                    // removed from cache while we were waiting for lock...
                    addresses = this;
                }
                // still us ?
                if (addresses == this) {
                    // lookup name services
                    InetAddress[] inetAddresses;
                    UnknownHostException ex;
                    int cachePolicy;
                    try {
                        inetAddresses = getAddressesFromNameService(host, reqAddr);
                        ex = null;
                        cachePolicy = InetAddressCachePolicy.get();
                    } catch (UnknownHostException uhe) {
                        inetAddresses = null;
                        ex = uhe;
                        cachePolicy = InetAddressCachePolicy.getNegative();
                    }
                    // remove or replace us with cached addresses according to cachePolicy
                    if (cachePolicy == InetAddressCachePolicy.NEVER) {
                        cache.remove(host, this);
                    } else {
                        CachedAddresses cachedAddresses = new CachedAddresses(
                            host,
                            inetAddresses,
                            cachePolicy == InetAddressCachePolicy.FOREVER ? 0L : System.nanoTime() + 1000_000_000L * cachePolicy // cachePolicy is in [s] - we need [ns]
                        );
                        if (cache.replace(host, this, cachedAddresses) && cachePolicy != InetAddressCachePolicy.FOREVER) {
                            // schedule expiry
                            expirySet.add(cachedAddresses);
                        }
                    }
                    if (inetAddresses == null) {
                        throw ex == null ? new UnknownHostException(host) : ex;
                    }
                    return inetAddresses;
                }
                // else addresses != this
            }
            // delegate to different addresses when we are already replaced
            // but outside of synchronized block to avoid any chance of dead-locking
            return addresses.get();
        }
    }

    /**
     * NameService provides host and address lookup service
     */
    private interface NameService {
        /**
         * Lookup a host mapping by name. Retrieve the IP addresses
         * associated with a host
         *
         * @param host the specified hostname
         * @return array of IP addresses for the requested host
         * @throws UnknownHostException
         *             if no IP address for the {@code host} could be found
         */
        InetAddress[] lookupAllHostAddr(String host) throws UnknownHostException;

        /**
         * Lookup the host corresponding to the IP address provided
         *
         * @param addr byte array representing an IP address
         * @return {@code String} representing the host name mapping
         * @throws UnknownHostException
         *             if no host found for the specified IP address
         */
        String getHostByAddr(byte[] addr) throws UnknownHostException;
    }

    /**
     * The default NameService implementation, which delegates to the underlying
     * OS network libraries to resolve host address mappings.
     */
    private static final class PlatformNameService implements NameService {
        public InetAddress[] lookupAllHostAddr(String host) throws UnknownHostException {
            return impl.lookupAllHostAddr(host);
        }

        public String getHostByAddr(byte[] addr) throws UnknownHostException {
            return impl.getHostByAddr(addr);
        }
    }

    static final InetAddressImpl impl;

    static {
        // create the impl
        impl = InetAddressImplFactory.create();

        // create name service
        nameService = createNameService();
        }

    /**
     * Create an instance of the NameService interface.
     *
     * The default NameService is the PlatformNameService, which typically
     * delegates name and address resolution calls to the underlying
     * OS network libraries.
     *
     * @return a NameService
     */
    private static NameService createNameService() {
        return new PlatformNameService();
    }

    /**
     * Creates an InetAddress based on the provided host name and IP address.
     * No name service is checked for the validity of the address.
     *
     * The host name can either be a machine name, such as
     * "{@code java.sun.com}", or a textual representation of its IP
     * address.
     *
     * No validity checking is done on the host name either.
     *
     * If addr specifies an IPv4 address an instance of Inet4Address
     * will be returned; otherwise, an instance of Inet6Address
     * will be returned.
     *
     * IPv4 address byte array must be 4 bytes long and IPv6 byte array
     * must be 16 bytes long
     *
     * @param host the specified host
     * @param addr the raw IP address in network byte order
     * @return an InetAddress object created from the raw IP address.
     * @throws UnknownHostException  if IP address is of illegal length
     */
    public static InetAddress getByAddress(String host, byte[] addr) throws UnknownHostException {
        if (host != null && host.length() > 0 && host.charAt(0) == '[') {
            if (host.charAt(host.length()-1) == ']') {
                host = host.substring(1, host.length() -1);
            }
        }
        if (addr != null) {
            if (addr.length == Inet4Address.INADDRSZ) {
                return new Inet4Address(host, addr);
            } else if (addr.length == Inet6Address.INADDRSZ) {
                byte[] newAddr = IPAddressUtil.convertFromIPv4MappedAddress(addr);
                if (newAddr != null) {
                    return new Inet4Address(host, newAddr);
                } else {
                    return new Inet6Address(host, addr);
                }
            }
        }
        throw new UnknownHostException("addr is of illegal length");
    }

    /**
     * Determines the IP address of a host, given the host's name.
     *
     * The host name can either be a machine name, such as
     * "{@code java.sun.com}", or a textual representation of its
     * IP address. If a literal IP address is supplied, only the
     * validity of the address format is checked.
     *
     * For {@code host} specified in literal IPv6 address,
     * either the form defined in RFC 2732 or the literal IPv6 address
     * format defined in RFC 2373 is accepted. IPv6 scoped addresses are also
     * supported. See <a href="Inet6Address.html#scoped">here</a> for a description of IPv6
     * scoped addresses.
     *
     * If the host is {@code null} or {@code host.length()} is equal
     * to zero, then an {@code InetAddress} representing an address of the
     * loopback interface is returned.
     * See <a href="http://www.ietf.org/rfc/rfc3330.txt">RFC&nbsp;3330</a>
     * section&nbsp;2 and <a href="http://www.ietf.org/rfc/rfc2373.txt">RFC&nbsp;2373</a>
     * section&nbsp;2.5.3.
     *
     * @param host   the specified host, or {@code null}.
     * @return an IP address for the given host name.
     * @throws UnknownHostException  if no IP address for the
     *               {@code host} could be found, or if a scope_id was specified
     *               for a global IPv6 address.
     */
    public static InetAddress getByName(String host) throws UnknownHostException {
        return InetAddress.getAllByName(host)[0];
    }

    // called from deployment cache manager
    private static InetAddress getByName(String host, InetAddress reqAddr) throws UnknownHostException {
        return InetAddress.getAllByName(host, reqAddr)[0];
    }

    /**
     * Given the name of a host, returns an array of its IP addresses,
     * based on the configured name service on the system.
     *
     * The host name can either be a machine name, such as
     * "{@code java.sun.com}", or a textual representation of its IP
     * address. If a literal IP address is supplied, only the
     * validity of the address format is checked.
     *
     * For {@code host} specified in <i>literal IPv6 address</i>,
     * either the form defined in RFC 2732 or the literal IPv6 address
     * format defined in RFC 2373 is accepted. A literal IPv6 address may
     * also be qualified by appending a scoped zone identifier or scope_id.
     * The syntax and usage of scope_ids is described
     * <a href="Inet6Address.html#scoped">here</a>.
     *
     * If the host is {@code null} or {@code host.length()} is equal
     * to zero, then an {@code InetAddress} representing an address of the
     * loopback interface is returned.
     * See <a href="http://www.ietf.org/rfc/rfc3330.txt">RFC&nbsp;3330</a>
     * section&nbsp;2 and <a href="http://www.ietf.org/rfc/rfc2373.txt">RFC&nbsp;2373</a>
     * section&nbsp;2.5.3.
     *
     * @param host   the name of the host, or {@code null}.
     * @return an array of all the IP addresses for a given host name.
     *
     * @throws UnknownHostException  if no IP address for the
     *               {@code host} could be found, or if a scope_id was specified
     *               for a global IPv6 address.
     */
    public static InetAddress[] getAllByName(String host) throws UnknownHostException {
        return getAllByName(host, null);
    }

    private static InetAddress[] getAllByName(String host, InetAddress reqAddr) throws UnknownHostException {
        if (host == null || host.length() == 0) {
            InetAddress[] ret = new InetAddress[1];
            ret[0] = impl.loopbackAddress();
            return ret;
        }

        boolean ipv6Expected = false;
        if (host.charAt(0) == '[') {
            // This is supposed to be an IPv6 literal
            if (host.length() > 2 && host.charAt(host.length()-1) == ']') {
                host = host.substring(1, host.length() -1);
                ipv6Expected = true;
            } else {
                // This was supposed to be a IPv6 address, but it's not!
                throw new UnknownHostException(host + ": invalid IPv6 address");
            }
        }

        // if host is an IP address, we won't do further lookup
        if (Character.digit(host.charAt(0), 16) != -1 || (host.charAt(0) == ':')) {
            byte[] addr = null;
            int numericZone = -1;
            String ifname = null;
            // see if it is IPv4 address
            addr = IPAddressUtil.textToNumericFormatV4(host);
            if (addr == null) {
                // This is supposed to be an IPv6 literal
                // Check if a numeric or string zone id is present
                int pos;
                if ((pos=host.indexOf ('%')) != -1) {
                    numericZone = checkNumericZone (host);
                    if (numericZone == -1) { /* remainder of string must be an ifname */
                        ifname = host.substring (pos+1);
                    }
                }
                if ((addr = IPAddressUtil.textToNumericFormatV6(host)) == null && host.contains(":")) {
                    throw new UnknownHostException(host + ": invalid IPv6 address");
                }
            } else if (ipv6Expected) {
                // Means an IPv4 litteral between brackets!
                throw new UnknownHostException("["+host+"]");
            }
            InetAddress[] ret = new InetAddress[1];
            if (addr != null) {
                if (addr.length == Inet4Address.INADDRSZ) {
                    ret[0] = new Inet4Address(null, addr);
                } else {
                    if (ifname != null) {
                        ret[0] = new Inet6Address(null, addr, ifname);
                    } else {
                        ret[0] = new Inet6Address(null, addr, numericZone);
                    }
                }
                return ret;
            }
        } else if (ipv6Expected) {
            // We were expecting an IPv6 Litteral, but got something else
            throw new UnknownHostException("["+host+"]");
        }
        return getAllByName0(host, reqAddr, true, true);
    }

    /**
     * Returns the loopback address.
     *
     * The InetAddress returned will represent the IPv4
     * loopback address, 127.0.0.1, or the IPv6 loopback
     * address, ::1. The IPv4 loopback address returned
     * is only one of many in the form 127.*.*.*
     *
     * @return the InetAddress loopback instance.
     */
    public static InetAddress getLoopbackAddress() {
        return impl.loopbackAddress();
    }

    /**
     * check if the literal address string has %nn appended
     * returns -1 if not, or the numeric value otherwise.
     *
     * %nn may also be a string that represents the displayName of
     * a currently available NetworkInterface.
     */
    private static int checkNumericZone(String s) throws UnknownHostException {
        int percent = s.indexOf ('%');
        int slen = s.length();
        int digit, zone = 0;
        if (percent == -1) {
            return -1;
        }
        for (int i=percent+1; i < slen; i++) {
            char c = s.charAt(i);
            if (c == ']') {
                if (i == percent+1) {
                    /* empty per-cent field */
                    return -1;
                }
                break;
            }
            if ((digit = Character.digit (c, 10)) < 0) {
                return -1;
            }
            zone = (zone * 10) + digit;
        }
        return zone;
    }

    private static InetAddress[] getAllByName0(String host) throws UnknownHostException {
        return getAllByName0(host, true);
    }

    private static InetAddress[] getAllByName0(String host, boolean check) throws UnknownHostException {
        return getAllByName0 (host, null, check, true);
    }

    /**
     * Designated lookup method.
     *
     * @param host host name to look up
     * @param reqAddr requested address to be the 1st in returned array
     * @param check perform security check
     * @param useCache use cached value if not expired else always
     *                 perform name service lookup (and cache the result)
     * @return array of InetAddress(es)
     * @throws UnknownHostException if host name is not found
     */
    private static InetAddress[] getAllByName0(String host, InetAddress reqAddr, boolean check, boolean useCache) throws UnknownHostException {
        /* If it gets here it is presumed to be a hostname */

        // remove expired addresses from cache - expirySet keeps them ordered
        // by expiry time so we only need to iterate the prefix of the NavigableSet...
        long now = System.nanoTime();
        for (CachedAddresses caddrs : expirySet) {
            // compare difference of time instants rather than
            // time instants directly, to avoid possible overflow.
            // (see System.nanoTime() recommendations...)
            if ((caddrs.expiryTime - now) < 0L) {
                // ConcurrentSkipListSet uses weakly consistent iterator,
                // so removing while iterating is OK...
                if (expirySet.remove(caddrs)) {
                    // ... remove from cache
                    cache.remove(caddrs.host, caddrs);
                }
            } else {
                // we encountered 1st element that expires in future
                break;
            }
        }

        // look-up or remove from cache
        Addresses addrs;
        if (useCache) {
            addrs = cache.get(host);
        } else {
            addrs = cache.remove(host);
            if (addrs != null) {
                if (addrs instanceof CachedAddresses) {
                    // try removing from expirySet too if CachedAddresses
                    expirySet.remove(addrs);
                }
                addrs = null;
            }
        }

        if (addrs == null) {
            // create a NameServiceAddresses instance which will look up
            // the name service and install it within cache...
            Addresses oldAddrs = cache.putIfAbsent(host, addrs = new NameServiceAddresses(host, reqAddr));
            if (oldAddrs != null) { // lost putIfAbsent race
                addrs = oldAddrs;
            }
        }

        // ask Addresses to get an array of InetAddress(es) and clone it
        return addrs.get().clone();
    }

    static InetAddress[] getAddressesFromNameService(String host, InetAddress reqAddr) throws UnknownHostException {
        InetAddress[] addresses = null;
        UnknownHostException ex = null;

            try {
                addresses = nameService.lookupAllHostAddr(host);
            } catch (UnknownHostException uhe) {
                if (host.equalsIgnoreCase("localhost")) {
                    addresses = new InetAddress[] { impl.loopbackAddress() };
                }
                else {
                    ex = uhe;
                }
            }

        if (addresses == null) {
            throw ex == null ? new UnknownHostException(host) : ex;
        }

        // More to do?
        if (reqAddr != null && addresses.length > 1 && !addresses[0].equals(reqAddr)) {
            // Find it?
            int i = 1;
            for ( ; i < addresses.length; i++) {
                if (addresses[i].equals(reqAddr)) {
                    break;
                }
            }
            // Rotate
            if (i < addresses.length) {
                InetAddress tmp, tmp2 = reqAddr;
                for (int j = 0; j < i; j++) {
                    tmp = addresses[j];
                    addresses[j] = tmp2;
                    tmp2 = tmp;
                }
                addresses[i] = tmp2;
            }
        }

        return addresses;
    }

    /**
     * Returns an {@code InetAddress} object given the raw IP address .
     * The argument is in network byte order: the highest order
     * byte of the address is in {@code getAddress()[0]}.
     *
     * This method doesn't block, i.e. no reverse name service lookup
     * is performed.
     *
     * IPv4 address byte array must be 4 bytes long and IPv6 byte array
     * must be 16 bytes long
     *
     * @param addr the raw IP address in network byte order
     * @return an InetAddress object created from the raw IP address.
     * @throws UnknownHostException  if IP address is of illegal length
     */
    public static InetAddress getByAddress(byte[] addr) throws UnknownHostException {
        return getByAddress(null, addr);
    }

    private static final class CachedLocalHost {
        final String host;
        final InetAddress addr;
        final long expiryTime = System.nanoTime() + 5000_000_000L; // now + 5s;

        CachedLocalHost(String host, InetAddress addr) {
            this.host = host;
            this.addr = addr;
        }
    }

    private static volatile CachedLocalHost cachedLocalHost;

    /**
     * Returns the address of the local host. This is achieved by retrieving
     * the name of the host from the system, then resolving that name into
     * an {@code InetAddress}.
     *
     * Note: The resolved address may be cached for a short period of time.
     *
     * @return the address of the local host.
     *
     * @throws UnknownHostException  if the local host name could not
     *             be resolved into an address.
     */
    public static InetAddress getLocalHost() throws UnknownHostException {
        try {
            // is cached data still valid?
            CachedLocalHost clh = cachedLocalHost;
            if (clh != null && (clh.expiryTime - System.nanoTime()) >= 0L) {
                return clh.addr;
            }

            String local = impl.getLocalHostName();

            InetAddress localAddr;
            if (local.equals("localhost")) {
                // shortcut for "localhost" host name
                localAddr = impl.loopbackAddress();
            } else {
                // call getAllByName0 without security checks and
                // without using cached data
                try {
                    localAddr = getAllByName0(local, null, false, false)[0];
                } catch (UnknownHostException uhe) {
                    // Rethrow with a more informative error message.
                    UnknownHostException uhe2 = new UnknownHostException(local + ": " + uhe.getMessage());
                    uhe2.initCause(uhe);
                    throw uhe2;
                }
            }
            cachedLocalHost = new CachedLocalHost(local, localAddr);
            return localAddr;
        } catch (SecurityException e) {
            return impl.loopbackAddress();
        }
    }

    /**
     * Perform class load-time initializations.
     */
    private static native void init();

    /*
     * Returns the InetAddress representing anyLocalAddress
     * (typically 0.0.0.0 or ::0)
     */
    static InetAddress anyLocalAddress() {
        return impl.anyLocalAddress();
    }

    /*
     * Load and instantiate an underlying impl class
     */
    static InetAddressImpl loadImpl(String implName) {
        Object impl = null;

        /*
         * Property "impl.prefix" will be prepended to the classname
         * of the implementation object we instantiate, to which we
         * delegate the real work (like native methods).  This
         * property can vary across implementations of the java.
         * classes.  The default is an empty String "".
         */
        String prefix = ""; // "impl.prefix"
        try {
            @SuppressWarnings("deprecation")
            Object tmp = Class.forName("java.net." + prefix + implName).newInstance();
            impl = tmp;
        } catch (ClassNotFoundException e) {
            System.err.println("Class not found: java.net." + prefix + implName + ":\ncheck impl.prefix property in your properties file.");
        } catch (InstantiationException e) {
            System.err.println("Could not instantiate: java.net." + prefix + implName + ":\ncheck impl.prefix property in your properties file.");
        } catch (IllegalAccessException e) {
            System.err.println("Cannot access class: java.net." + prefix + implName + ":\ncheck impl.prefix property in your properties file.");
        }

        if (impl == null) {
            try {
                @SuppressWarnings("deprecation")
                Object tmp = Class.forName(implName).newInstance();
                impl = tmp;
            } catch (Exception e) {
                throw new Error("System property impl.prefix incorrect");
            }
        }

        return (InetAddressImpl) impl;
    }
}

/*
 * Simple factory to create the impl
 */
class InetAddressImplFactory {
    static InetAddressImpl create() {
        return InetAddress.loadImpl(isIPv6Supported() ? "Inet6AddressImpl" : "Inet4AddressImpl");
    }

    static native boolean isIPv6Supported();
}

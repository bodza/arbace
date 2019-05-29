package java.net;
import java.io.IOException;
/*
 * Package private interface to "implementation" used by
 * {@link InetAddress}.
 *
 * See {@link java.net.Inet4AddressImp} and
 * {@link java.net.Inet6AddressImp}.
 */
interface InetAddressImpl {
    String getLocalHostName() throws UnknownHostException;
    InetAddress[] lookupAllHostAddr(String hostname) throws UnknownHostException;
    String getHostByAddr(byte[] addr) throws UnknownHostException;

    InetAddress anyLocalAddress();
    InetAddress loopbackAddress();
    boolean isReachable(InetAddress addr, int timeout, NetworkInterface netif, int ttl) throws IOException;
}

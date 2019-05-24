package java.net;

/**
 * Choose a network interface to be the default for
 * outgoing IPv6 traffic that does not specify a scope_id (and which needs one).
 * We choose the first interface that is up and is (in order of preference):
 * 1. neither loopback nor point to point
 * 2. point to point
 * 3. loopback
 * 4. none.
 * Platforms that do not require a default interface implement a dummy
 * that returns null.
 */
import java.util.Enumeration;
import java.io.IOException;

class DefaultInterface {
    private static final NetworkInterface defaultInterface = chooseDefaultInterface();

    static NetworkInterface getDefault() {
        return defaultInterface;
    }

    /**
     * Choose a default interface. This method returns the first interface that
     * is both "up" and supports multicast. This method chooses an interface in
     * order of preference:
     * 1. neither loopback nor point to point
     *    (prefer interfaces with dual IP support)
     * 2. point to point
     * 3. loopback
     *
     * @return the chosen interface or {@code null} if there isn't a suitable
     *          default
     */
    private static NetworkInterface chooseDefaultInterface() {
        Enumeration<NetworkInterface> nifs;

        try {
           nifs = NetworkInterface.getNetworkInterfaces();
        } catch (IOException ignore) {
            // unable to enumerate network interfaces
            return null;
        }

        NetworkInterface preferred = null;
        NetworkInterface ppp = null;
        NetworkInterface loopback = null;

        while (nifs.hasMoreElements()) {
            NetworkInterface ni = nifs.nextElement();
            try {
                if (!ni.isUp() || !ni.supportsMulticast())
                    continue;

                boolean ip4 = false, ip6 = false;
                Enumeration<InetAddress> addrs = ni.getInetAddresses();
                while (addrs.hasMoreElements()) {
                    InetAddress addr = addrs.nextElement();
                    if (!addr.isAnyLocalAddress()) {
                        if (addr instanceof Inet4Address) {
                            ip4 = true;
                        } else if (addr instanceof Inet6Address) {
                            ip6 = true;
                        }
                    }
                }

                boolean isLoopback = ni.isLoopback();
                boolean isPPP = ni.isPointToPoint();
                if (!isLoopback && !isPPP) {
                    // found an interface that is not the loopback or a
                    // point-to-point interface
                    if (preferred == null) {
                        preferred = ni;
                    } else if (ip4 && ip6) {
                        return ni;
                    }
                }
                if (ppp == null && isPPP)
                    ppp = ni;
                if (loopback == null && isLoopback)
                    loopback = ni;
            } catch (IOException skip) { }
        }

        if (preferred != null) {
            return preferred;
        } else {
            return (ppp != null) ? ppp : loopback;
        }
    }
}

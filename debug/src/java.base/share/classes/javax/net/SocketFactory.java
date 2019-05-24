package javax.net;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;

/**
 * This class creates sockets.  It may be subclassed by other factories,
 * which create particular subclasses of sockets and thus provide a general
 * framework for the addition of public socket-level functionality.
 *
 * Socket factories are a simple way to capture a variety of policies
 * related to the sockets being constructed, producing such sockets in
 * a way which does not require special configuration of the code which
 * asks for the sockets:
 *
 *  <ul>
 *      <li>Due to polymorphism of both factories and sockets, different
 *      kinds of sockets can be used by the same application code just
 *      by passing it different kinds of factories.
 *
 *      <li>Factories can themselves be customized with parameters used
 *      in socket construction.  So for example, factories could be
 *      customized to return sockets with different networking timeouts
 *      or security parameters already configured.
 *
 *      <li>The sockets returned to the application can be subclasses
 *      of java.net.Socket, so that they can directly expose new APIs
 *      for features such as compression, security, record marking,
 *      statistics collection, or firewall tunneling.
 *  </ul>
 *
 * Factory classes are specified by environment-specific configuration
 * mechanisms.  For example, the <em>getDefault</em> method could return
 * a factory that was appropriate for a particular user or applet, and a
 * framework could use a factory customized to its own purposes.
 */
public abstract class SocketFactory {
    //
    // NOTE:  JDK 1.1 bug in class GC, this can get collected
    // even though it's always accessible via getDefault().
    //
    private static SocketFactory theFactory;

    /**
     * Creates a <code>SocketFactory</code>.
     */
    protected SocketFactory() { /* NOTHING */ }

    /**
     * Returns a copy of the environment's default socket factory.
     *
     * @return the default <code>SocketFactory</code>
     */
    public static SocketFactory getDefault()
    {
        synchronized (SocketFactory.class) {
            if (theFactory == null) {
                //
                // Different implementations of this method SHOULD
                // work rather differently.  For example, driving
                // this from a system property, or using a different
                // implementation than JavaSoft's.
                //
                theFactory = new DefaultSocketFactory();
            }
        }

        return theFactory;
    }

    /**
     * Creates an unconnected socket.
     *
     * @return the unconnected socket
     * @throws IOException if the socket cannot be created
     */
    public Socket createSocket() throws IOException {
        //
        // bug 6771432:
        // The Exception is used by HttpsClient to signal that
        // unconnected sockets have not been implemented.
        //
        UnsupportedOperationException uop = new UnsupportedOperationException();
        SocketException se =  new SocketException("Unconnected sockets not implemented");
        se.initCause(uop);
        throw se;
    }

    /**
     * Creates a socket and connects it to the specified remote host
     * at the specified remote port.  This socket is configured using
     * the socket options established for this factory.
     *
     * @param host the server host name with which to connect, or
     *        <code>null</code> for the loopback address.
     * @param port the server port
     * @return the <code>Socket</code>
     * @throws IOException if an I/O error occurs when creating the socket
     * @throws UnknownHostException if the host is not known
     * @throws IllegalArgumentException if the port parameter is outside the
     *         specified range of valid port values, which is between 0 and
     *         65535, inclusive.
     */
    public abstract Socket createSocket(String host, int port) throws IOException, UnknownHostException;

    /**
     * Creates a socket and connects it to the specified remote host
     * on the specified remote port.
     * The socket will also be bound to the local address and port supplied.
     * This socket is configured using
     * the socket options established for this factory.
     *
     * @param host the server host name with which to connect, or
     *        <code>null</code> for the loopback address.
     * @param port the server port
     * @param localHost the local address the socket is bound to
     * @param localPort the local port the socket is bound to
     * @return the <code>Socket</code>
     * @throws IOException if an I/O error occurs when creating the socket
     * @throws UnknownHostException if the host is not known
     * @throws IllegalArgumentException if the port parameter or localPort
     *         parameter is outside the specified range of valid port values,
     *         which is between 0 and 65535, inclusive.
     */
    public abstract Socket
    createSocket(String host, int port, InetAddress localHost, int localPort) throws IOException, UnknownHostException;

    /**
     * Creates a socket and connects it to the specified port number
     * at the specified address.  This socket is configured using
     * the socket options established for this factory.
     *
     * @param host the server host
     * @param port the server port
     * @return the <code>Socket</code>
     * @throws IOException if an I/O error occurs when creating the socket
     * @throws IllegalArgumentException if the port parameter is outside the
     *         specified range of valid port values, which is between 0 and
     *         65535, inclusive.
     * @throws NullPointerException if <code>host</code> is null.
     */
    public abstract Socket createSocket(InetAddress host, int port) throws IOException;

    /**
     * Creates a socket and connect it to the specified remote address
     * on the specified remote port.  The socket will also be bound
     * to the local address and port suplied.  The socket is configured using
     * the socket options established for this factory.
     *
     * @param address the server network address
     * @param port the server port
     * @param localAddress the client network address
     * @param localPort the client port
     * @return the <code>Socket</code>
     * @throws IOException if an I/O error occurs when creating the socket
     * @throws IllegalArgumentException if the port parameter or localPort
     *         parameter is outside the specified range of valid port values,
     *         which is between 0 and 65535, inclusive.
     * @throws NullPointerException if <code>address</code> is null.
     */
    public abstract Socket createSocket(InetAddress address, int port, InetAddress localAddress, int localPort) throws IOException;
}

//
// The default factory has NO intelligence about policies like tunneling
// out through firewalls (e.g. SOCKS V4 or V5) or in through them
// (e.g. using SSL), or that some ports are reserved for use with SSL.
//
// Note that at least JDK 1.1 has a low level "plainSocketImpl" that
// knows about SOCKS V4 tunneling, so this isn't a totally bogus default.
//
// ALSO:  we may want to expose this class somewhere so other folk
// can reuse it, particularly if we start to add highly useful features
// such as ability to set connect timeouts.
//
class DefaultSocketFactory extends SocketFactory {
    public Socket createSocket() {
        return new Socket();
    }

    public Socket createSocket(String host, int port) throws IOException, UnknownHostException
    {
        return new Socket(host, port);
    }

    public Socket createSocket(InetAddress address, int port) throws IOException
    {
        return new Socket(address, port);
    }

    public Socket createSocket(String host, int port, InetAddress clientAddress, int clientPort) throws IOException, UnknownHostException
    {
        return new Socket(host, port, clientAddress, clientPort);
    }

    public Socket createSocket(InetAddress address, int port, InetAddress clientAddress, int clientPort) throws IOException
    {
        return new Socket(address, port, clientAddress, clientPort);
    }
}

package java.net;

/**
 * This interface defines a factory for datagram socket implementations. It
 * is used by the classes {@code DatagramSocket} to create actual socket
 * implementations.
 */
public interface DatagramSocketImplFactory {
    /**
     * Creates a new {@code DatagramSocketImpl} instance.
     *
     * @return a new instance of {@code DatagramSocketImpl}.
     */
    DatagramSocketImpl createDatagramSocketImpl();
}

package java.net;

/**
 * This interface defines a factory for socket implementations. It
 * is used by the classes {@code Socket} and
 * {@code ServerSocket} to create actual socket
 * implementations.
 */
public interface SocketImplFactory {
    /**
     * Creates a new {@code SocketImpl} instance.
     *
     * @return a new instance of {@code SocketImpl}.
     */
    SocketImpl createSocketImpl();
}

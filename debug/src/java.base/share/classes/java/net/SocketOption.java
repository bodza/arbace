package java.net;

/**
 * A socket option associated with a socket.
 *
 * @param <T>     The type of the socket option value.
 */
public interface SocketOption<T> {
    /**
     * Returns the name of the socket option.
     *
     * @return the name of the socket option
     */
    String name();

    /**
     * Returns the type of the socket option value.
     *
     * @return the type of the socket option value
     */
    Class<T> type();
}

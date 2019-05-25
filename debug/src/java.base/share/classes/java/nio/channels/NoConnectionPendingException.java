package java.nio.channels;

/**
 * Unchecked exception thrown when the {@link SocketChannel#finishConnect
 * finishConnect} method of a {@link SocketChannel} is invoked without first
 * successfully invoking its {@link SocketChannel#connect connect} method.
 */
public class NoConnectionPendingException extends IllegalStateException {
    private static final long serialVersionUID = -8296561183633134743L;

    /**
     * Constructs an instance of this class.
     */
    public NoConnectionPendingException() { }
}

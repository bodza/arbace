package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to connect a {@link
 * SocketChannel} that is already connected.
 */
public class AlreadyConnectedException extends IllegalStateException {
    private static final long serialVersionUID = -7331895245053773357L;

    /**
     * Constructs an instance of this class.
     */
    public AlreadyConnectedException() { }
}

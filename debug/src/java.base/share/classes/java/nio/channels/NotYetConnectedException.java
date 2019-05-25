package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to invoke an I/O
 * operation upon a socket channel that is not yet connected.
 */
public class NotYetConnectedException extends IllegalStateException {
    private static final long serialVersionUID = 4697316551909513464L;

    /**
     * Constructs an instance of this class.
     */
    public NotYetConnectedException() { }
}

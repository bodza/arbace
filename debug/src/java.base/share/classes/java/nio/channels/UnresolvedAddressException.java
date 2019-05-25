package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to invoke a network
 * operation upon an unresolved socket address.
 */
public class UnresolvedAddressException extends IllegalArgumentException {
    private static final long serialVersionUID = 6136959093620794148L;

    /**
     * Constructs an instance of this class.
     */
    public UnresolvedAddressException() { }
}

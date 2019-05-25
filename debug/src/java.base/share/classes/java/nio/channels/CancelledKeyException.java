package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to use
 * a selection key that is no longer valid.
 */
public class CancelledKeyException extends IllegalStateException {
    private static final long serialVersionUID = -8438032138028814268L;

    /**
     * Constructs an instance of this class.
     */
    public CancelledKeyException() { }
}

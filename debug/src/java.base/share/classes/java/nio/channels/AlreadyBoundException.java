package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to bind the socket a
 * network oriented channel that is already bound.
 */
public class AlreadyBoundException extends IllegalStateException {
    private static final long serialVersionUID = 6796072983322737592L;

    /**
     * Constructs an instance of this class.
     */
    public AlreadyBoundException() { }
}

package java.nio;

/**
 * Unchecked exception thrown when an attempt is made to reset a buffer
 * when its mark is not defined.
 */
public class InvalidMarkException extends IllegalStateException {
    private static final long serialVersionUID = 1698329710438510774L;

    /**
     * Constructs an instance of this class.
     */
    public InvalidMarkException() { }
}

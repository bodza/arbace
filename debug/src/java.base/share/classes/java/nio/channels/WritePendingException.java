package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to write to an
 * asynchronous socket channel and a previous write has not completed.
 */
public class WritePendingException extends IllegalStateException {
    private static final long serialVersionUID = 7031871839266032276L;

    /**
     * Constructs an instance of this class.
     */
    public WritePendingException() { }
}

package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to write
 * to a channel that was not originally opened for writing.
 */
public class NonWritableChannelException extends IllegalStateException {
    private static final long serialVersionUID = -7071230488279011621L;

    /**
     * Constructs an instance of this class.
     */
    public NonWritableChannelException() { }
}

package java.nio;

/**
 * Unchecked exception thrown when a relative <i>put</i> operation reaches
 * the target buffer's limit.
 */
public class BufferOverflowException extends RuntimeException {
    private static final long serialVersionUID = -5484897634319144535L;

    /**
     * Constructs an instance of this class.
     */
    public BufferOverflowException() { }
}

package java.nio;

/**
 * Unchecked exception thrown when a relative <i>put</i> operation reaches
 * the target buffer's limit.
 */
public class BufferOverflowException extends RuntimeException {
    /**
     * Constructs an instance of this class.
     */
    public BufferOverflowException() { }
}

package java.nio;

/**
 * Unchecked exception thrown when a relative <i>get</i> operation reaches
 * the source buffer's limit.
 */
public class BufferUnderflowException extends RuntimeException {
    /**
     * Constructs an instance of this class.
     */
    public BufferUnderflowException() { }
}

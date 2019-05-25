package java.nio;

/**
 * Unchecked exception thrown when a relative <i>get</i> operation reaches
 * the source buffer's limit.
 */
public class BufferUnderflowException extends RuntimeException {
    private static final long serialVersionUID = -1713313658691622206L;

    /**
     * Constructs an instance of this class.
     */
    public BufferUnderflowException() { }
}

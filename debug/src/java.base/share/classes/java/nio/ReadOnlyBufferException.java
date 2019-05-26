package java.nio;

/**
 * Unchecked exception thrown when a content-mutation method such as
 * <code>put</code> or <code>compact</code> is invoked upon a read-only buffer.
 */
public class ReadOnlyBufferException extends UnsupportedOperationException {
    /**
     * Constructs an instance of this class.
     */
    public ReadOnlyBufferException() { }
}

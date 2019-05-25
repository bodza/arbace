package java.nio;

/**
 * Unchecked exception thrown when a content-mutation method such as
 * <code>put</code> or <code>compact</code> is invoked upon a read-only buffer.
 */
public class ReadOnlyBufferException extends UnsupportedOperationException {
    private static final long serialVersionUID = -1210063976496234090L;

    /**
     * Constructs an instance of this class.
     */
    public ReadOnlyBufferException() { }
}

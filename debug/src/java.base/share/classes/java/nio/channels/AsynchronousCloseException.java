package java.nio.channels;

/**
 * Checked exception received by a thread when another thread closes the
 * channel or the part of the channel upon which it is blocked in an I/O
 * operation.
 */
public class AsynchronousCloseException extends ClosedChannelException {
    private static final long serialVersionUID = 6891178312432313966L;

    /**
     * Constructs an instance of this class.
     */
    public AsynchronousCloseException() { }
}

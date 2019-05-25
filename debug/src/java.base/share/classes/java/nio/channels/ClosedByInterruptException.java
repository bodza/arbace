package java.nio.channels;

/**
 * Checked exception received by a thread when another thread interrupts it
 * while it is blocked in an I/O operation upon a channel.  Before this
 * exception is thrown the channel will have been closed and the interrupt
 * status of the previously-blocked thread will have been set.
 */
public class ClosedByInterruptException extends AsynchronousCloseException {
    private static final long serialVersionUID = -4488191543534286750L;

    /**
     * Constructs an instance of this class.
     */
    public ClosedByInterruptException() { }
}

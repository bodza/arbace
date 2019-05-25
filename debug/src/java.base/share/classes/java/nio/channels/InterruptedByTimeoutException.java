package java.nio.channels;

/**
 * Checked exception received by a thread when a timeout elapses before an
 * asynchronous operation completes.
 */
public class InterruptedByTimeoutException extends java.io.IOException {
    private static final long serialVersionUID = -4268008601014042947L;

    /**
     * Constructs an instance of this class.
     */
    public InterruptedByTimeoutException() { }
}

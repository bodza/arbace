package java.util.concurrent;

/**
 * Exception thrown when a thread tries to wait upon a barrier that is
 * in a broken state, or which enters the broken state while the thread
 * is waiting.
 */
public class BrokenBarrierException extends Exception {
    /**
     * Constructs a {@code BrokenBarrierException} with no specified detail
     * message.
     */
    public BrokenBarrierException() {}

    /**
     * Constructs a {@code BrokenBarrierException} with the specified
     * detail message.
     *
     * @param message the detail message
     */
    public BrokenBarrierException(String message) {
        super(message);
    }
}

package java.nio.channels;

/**
 * Checked exception received by a thread when another thread interrupts it
 * while it is waiting to acquire a file lock.  Before this exception is thrown
 * the interrupt status of the previously-blocked thread will have been set.
 */
public class FileLockInterruptionException extends java.io.IOException {
    private static final long serialVersionUID = 7104080643653532383L;

    /**
     * Constructs an instance of this class.
     */
    public FileLockInterruptionException() { }
}

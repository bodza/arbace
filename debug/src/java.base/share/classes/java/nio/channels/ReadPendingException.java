package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to read from an
 * asynchronous socket channel and a previous read has not completed.
 */
public class ReadPendingException extends IllegalStateException {
    private static final long serialVersionUID = 1986315242191227217L;

    /**
     * Constructs an instance of this class.
     */
    public ReadPendingException() { }
}

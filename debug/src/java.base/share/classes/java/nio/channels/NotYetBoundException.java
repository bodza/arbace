package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to invoke an I/O
 * operation upon a server socket channel that is not yet bound.
 */
public class NotYetBoundException extends IllegalStateException {
    private static final long serialVersionUID = 4640999303950202242L;

    /**
     * Constructs an instance of this class.
     */
    public NotYetBoundException() { }
}

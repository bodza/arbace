package java.net;

/**
 * Signals that a timeout has occurred on a socket read or accept.
 */
public class SocketTimeoutException extends java.io.InterruptedIOException {
    /**
     * Constructs a new SocketTimeoutException with a detail
     * message.
     * @param msg the detail message
     */
    public SocketTimeoutException(String msg) {
        super(msg);
    }

    /**
     * Construct a new SocketTimeoutException with no detailed message.
     */
    public SocketTimeoutException() {}
}

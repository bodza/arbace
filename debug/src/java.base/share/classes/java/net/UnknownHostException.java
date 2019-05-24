package java.net;

import java.io.IOException;

/**
 * Thrown to indicate that the IP address of a host could not be determined.
 */
public class UnknownHostException extends IOException {
    /**
     * Constructs a new {@code UnknownHostException} with the
     * specified detail message.
     *
     * @param message   the detail message.
     */
    public UnknownHostException(String message) {
        super(message);
    }

    /**
     * Constructs a new {@code UnknownHostException} with no detail
     * message.
     */
    public UnknownHostException() {
    }
}

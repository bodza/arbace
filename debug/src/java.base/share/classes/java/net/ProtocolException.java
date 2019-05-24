package java.net;

import java.io.IOException;

/**
 * Thrown to indicate that there is an error in the underlying
 * protocol, such as a TCP error.
 */
public class ProtocolException extends IOException {
    /**
     * Constructs a new {@code ProtocolException} with the
     * specified detail message.
     *
     * @param message   the detail message.
     */
    public ProtocolException(String message) {
        super(message);
    }

    /**
     * Constructs a new {@code ProtocolException} with no detail message.
     */
    public ProtocolException() {
    }
}

package java.net;

import java.io.IOException;

/**
 * Thrown to indicate that an unknown service exception has
 * occurred. Either the MIME type returned by a URL connection does
 * not make sense, or the application is attempting to write to a
 * read-only URL connection.
 */
public class UnknownServiceException extends IOException {
    /**
     * Constructs a new {@code UnknownServiceException} with no
     * detail message.
     */
    public UnknownServiceException() {
    }

    /**
     * Constructs a new {@code UnknownServiceException} with the
     * specified detail message.
     *
     * @param msg   the detail message.
     */
    public UnknownServiceException(String msg) {
        super(msg);
    }
}

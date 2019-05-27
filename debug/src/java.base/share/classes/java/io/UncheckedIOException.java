package java.io;

import java.util.Objects;

/**
 * Wraps an {@link IOException} with an unchecked exception.
 */
public class UncheckedIOException extends RuntimeException {
    /**
     * Constructs an instance of this class.
     *
     * @param message
     *          the detail message, can be null
     * @param cause
     *          the {@code IOException}
     *
     * @throws NullPointerException
     *          if the cause is {@code null}
     */
    public UncheckedIOException(String message, IOException cause) {
        super(message, Objects.requireNonNull(cause));
    }

    /**
     * Constructs an instance of this class.
     *
     * @param cause
     *          the {@code IOException}
     *
     * @throws NullPointerException
     *          if the cause is {@code null}
     */
    public UncheckedIOException(IOException cause) {
        super(Objects.requireNonNull(cause));
    }

    /**
     * Returns the cause of this exception.
     *
     * @return the {@code IOException} which is the cause of this exception.
     */
    // @Override
    public IOException getCause() {
        return (IOException) super.getCause();
    }
}

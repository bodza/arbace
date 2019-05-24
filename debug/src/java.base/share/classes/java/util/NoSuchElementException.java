package java.util;

/**
 * Thrown by various accessor methods to indicate that the element being requested
 * does not exist.
 */
public class NoSuchElementException extends RuntimeException {
    /**
     * Constructs a {@code NoSuchElementException} with {@code null}
     * as its error message string.
     */
    public NoSuchElementException() {
        super();
    }

    /**
     * Constructs a {@code NoSuchElementException}, saving a reference
     * to the error message string {@code s} for later retrieval by the
     * {@code getMessage} method.
     *
     * @param s   the detail message.
     */
    public NoSuchElementException(String s) {
        super(s);
    }
}

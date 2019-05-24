package java.util;

/**
 * Thrown by a {@code Scanner} to indicate that the token
 * retrieved does not match the pattern for the expected type, or
 * that the token is out of range for the expected type.
 */
public class InputMismatchException extends NoSuchElementException {
    /**
     * Constructs an {@code InputMismatchException} with {@code null}
     * as its error message string.
     */
    public InputMismatchException() {
        super();
    }

    /**
     * Constructs an {@code InputMismatchException}, saving a reference
     * to the error message string {@code s} for later retrieval by the
     * {@code getMessage} method.
     *
     * @param s   the detail message.
     */
    public InputMismatchException(String s) {
        super(s);
    }
}

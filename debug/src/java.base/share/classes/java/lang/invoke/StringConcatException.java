package java.lang.invoke;

/**
 * StringConcatException is thrown by {@link StringConcatFactory} when linkage
 * invariants are violated.
 */
public class StringConcatException extends Exception {
    /**
     * Constructs an exception with a message
     * @param msg exception message
     */
    public StringConcatException(String msg) {
        super(msg);
    }

    /**
     * Constructs an exception with a message and a linked throwable
     * @param msg   exception message
     * @param cause throwable cause
     */
    public StringConcatException(String msg, Throwable cause) {
        super(msg, cause);
    }
}

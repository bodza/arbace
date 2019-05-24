package java.lang.reflect;

/**
 * Thrown when Java language access checks cannot be suppressed.
 */
public class InaccessibleObjectException extends RuntimeException {
    /**
     * Constructs an {@code InaccessibleObjectException} with no detail message.
     */
    public InaccessibleObjectException() {
    }

    /**
     * Constructs an {@code InaccessibleObjectException} with the given detail
     * message.
     *
     * @param msg
     *        The detail message
     */
    public InaccessibleObjectException(String msg) {
        super(msg);
    }
}

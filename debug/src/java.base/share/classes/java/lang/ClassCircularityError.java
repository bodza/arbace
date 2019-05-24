package java.lang;

/**
 * Thrown when the Java Virtual Machine detects a circularity in the
 * superclass hierarchy of a class being loaded.
 */
public class ClassCircularityError extends LinkageError {
    /**
     * Constructs a {@code ClassCircularityError} with no detail message.
     */
    public ClassCircularityError() {
        super();
    }

    /**
     * Constructs a {@code ClassCircularityError} with the specified detail
     * message.
     *
     * @param s
     *         The detail message
     */
    public ClassCircularityError(String s) {
        super(s);
    }
}

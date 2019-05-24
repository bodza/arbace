package java.lang;

/**
 * Thrown when a particular method cannot be found.
 */
public class NoSuchMethodException extends ReflectiveOperationException {
    /**
     * Constructs a <code>NoSuchMethodException</code> without a detail message.
     */
    public NoSuchMethodException() {
        super();
    }

    /**
     * Constructs a <code>NoSuchMethodException</code> with a detail message.
     *
     * @param s   the detail message.
     */
    public NoSuchMethodException(String s) {
        super(s);
    }
}

package java.lang;

/**
 * Signals that the class doesn't have a field of a specified name.
 */
public class NoSuchFieldException extends ReflectiveOperationException {
    /**
     * Constructor.
     */
    public NoSuchFieldException() {
        super();
    }

    /**
     * Constructor with a detail message.
     *
     * @param s the detail message
     */
    public NoSuchFieldException(String s) {
        super(s);
    }
}

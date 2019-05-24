package java.lang;

/**
 * Thrown if an application tries to access or modify a specified
 * field of an object, and that object no longer has that field.
 *
 * Normally, this error is caught by the compiler; this error can
 * only occur at run time if the definition of a class has
 * incompatibly changed.
 */
public class NoSuchFieldError extends IncompatibleClassChangeError {
    /**
     * Constructs a <code>NoSuchFieldError</code> with no detail message.
     */
    public NoSuchFieldError() {
        super();
    }

    /**
     * Constructs a <code>NoSuchFieldError</code> with the specified
     * detail message.
     *
     * @param s   the detail message.
     */
    public NoSuchFieldError(String s) {
        super(s);
    }
}

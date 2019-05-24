package java.lang;

/**
 * Thrown if an application tries to call a specified method of a
 * class (either static or instance), and that class no longer has a
 * definition of that method.
 *
 * Normally, this error is caught by the compiler; this error can
 * only occur at run time if the definition of a class has
 * incompatibly changed.
 */
public class NoSuchMethodError extends IncompatibleClassChangeError {
    /**
     * Constructs a <code>NoSuchMethodError</code> with no detail message.
     */
    public NoSuchMethodError() {
        super();
    }

    /**
     * Constructs a <code>NoSuchMethodError</code> with the
     * specified detail message.
     *
     * @param s   the detail message.
     */
    public NoSuchMethodError(String s) {
        super(s);
    }
}

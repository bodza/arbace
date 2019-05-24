package java.lang;

/**
 * Thrown if an application attempts to access or modify a field, or
 * to call a method that it does not have access to.
 *
 * Normally, this error is caught by the compiler; this error can
 * only occur at run time if the definition of a class has
 * incompatibly changed.
 */
public class IllegalAccessError extends IncompatibleClassChangeError {
    /**
     * Constructs an <code>IllegalAccessError</code> with no detail message.
     */
    public IllegalAccessError() {
        super();
    }

    /**
     * Constructs an <code>IllegalAccessError</code> with the specified
     * detail message.
     *
     * @param s   the detail message.
     */
    public IllegalAccessError(String s) {
        super(s);
    }
}

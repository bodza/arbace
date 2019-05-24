package java.lang;

/**
 * Thrown when an application tries to use the Java <code>new</code>
 * construct to instantiate an abstract class or an interface.
 *
 * Normally, this error is caught by the compiler; this error can
 * only occur at run time if the definition of a class has
 * incompatibly changed.
 */
public class InstantiationError extends IncompatibleClassChangeError {
    /**
     * Constructs an <code>InstantiationError</code> with no detail  message.
     */
    public InstantiationError() {
        super();
    }

    /**
     * Constructs an <code>InstantiationError</code> with the specified
     * detail message.
     *
     * @param s   the detail message.
     */
    public InstantiationError(String s) {
        super(s);
    }
}

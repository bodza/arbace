package java.lang;

/**
 * Thrown if the Java Virtual Machine or a <code>ClassLoader</code> instance
 * tries to load in the definition of a class (as part of a normal method call
 * or as part of creating a new instance using the <code>new</code> expression)
 * and no definition of the class could be found.
 *
 * The searched-for class definition existed when the currently
 * executing class was compiled, but the definition can no longer be
 * found.
 */
public class NoClassDefFoundError extends LinkageError {
    /**
     * Constructs a <code>NoClassDefFoundError</code> with no detail message.
     */
    public NoClassDefFoundError() {
        super();
    }

    /**
     * Constructs a <code>NoClassDefFoundError</code> with the specified
     * detail message.
     *
     * @param s   the detail message.
     */
    public NoClassDefFoundError(String s) {
        super(s);
    }
}

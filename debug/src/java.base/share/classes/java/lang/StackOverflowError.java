package java.lang;

/**
 * Thrown when a stack overflow occurs because an application
 * recurses too deeply.
 */
public class StackOverflowError extends VirtualMachineError {
    /**
     * Constructs a <code>StackOverflowError</code> with no detail message.
     */
    public StackOverflowError() {
        super();
    }

    /**
     * Constructs a <code>StackOverflowError</code> with the specified
     * detail message.
     *
     * @param s   the detail message.
     */
    public StackOverflowError(String s) {
        super(s);
    }
}

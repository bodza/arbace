package java.lang;

/**
 * Thrown when an unknown but serious exception has occurred in the
 * Java Virtual Machine.
 */
public class UnknownError extends VirtualMachineError {
    /**
     * Constructs an <code>UnknownError</code> with no detail message.
     */
    public UnknownError() {
        super();
    }

    /**
     * Constructs an <code>UnknownError</code> with the specified detail
     * message.
     *
     * @param s   the detail message.
     */
    public UnknownError(String s) {
        super(s);
    }
}

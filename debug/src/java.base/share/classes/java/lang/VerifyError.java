package java.lang;

/**
 * Thrown when the "verifier" detects that a class file,
 * though well formed, contains some sort of internal inconsistency
 * or security problem.
 */
public class VerifyError extends LinkageError {
    /**
     * Constructs an <code>VerifyError</code> with no detail message.
     */
    public VerifyError() {
        super();
    }

    /**
     * Constructs an <code>VerifyError</code> with the specified detail message.
     *
     * @param s   the detail message.
     */
    public VerifyError(String s) {
        super(s);
    }
}

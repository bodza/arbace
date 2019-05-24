package java.lang;

/**
 * Thrown when the Java Virtual Machine attempts to read a class
 * file and determines that the file is malformed or otherwise cannot
 * be interpreted as a class file.
 */
public class ClassFormatError extends LinkageError {
    /**
     * Constructs a <code>ClassFormatError</code> with no detail message.
     */
    public ClassFormatError() {
        super();
    }

    /**
     * Constructs a <code>ClassFormatError</code> with the specified
     * detail message.
     *
     * @param s   the detail message.
     */
    public ClassFormatError(String s) {
        super(s);
    }
}

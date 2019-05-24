package java.lang;

/**
 * Thrown if an application tries to create an array with negative size.
 */
public class NegativeArraySizeException extends RuntimeException {
    /**
     * Constructs a <code>NegativeArraySizeException</code> with no
     * detail message.
     */
    public NegativeArraySizeException() {
        super();
    }

    /**
     * Constructs a <code>NegativeArraySizeException</code> with the
     * specified detail message.
     *
     * @param s   the detail message.
     */
    public NegativeArraySizeException(String s) {
        super(s);
    }
}

package java.lang;

/**
 * Thrown to indicate that an index of some sort (such as to an array, to a
 * string, or to a vector) is out of range.
 *
 * Applications can subclass this class to indicate similar exceptions.
 */
public class IndexOutOfBoundsException extends RuntimeException {
    /**
     * Constructs an {@code IndexOutOfBoundsException} with no detail message.
     */
    public IndexOutOfBoundsException() {
        super();
    }

    /**
     * Constructs an {@code IndexOutOfBoundsException} with the specified detail
     * message.
     *
     * @param s the detail message
     */
    public IndexOutOfBoundsException(String s) {
        super(s);
    }

    /**
     * Constructs a new {@code IndexOutOfBoundsException} class with an
     * argument indicating the illegal index.
     *
     * The index is included in this exception's detail message.  The
     * exact presentation format of the detail message is unspecified.
     *
     * @param index the illegal index.
     */
    public IndexOutOfBoundsException(int index) {
        super(String.str("Index out of range: ", index));
    }
}

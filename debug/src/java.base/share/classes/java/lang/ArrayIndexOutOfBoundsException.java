package java.lang;

/**
 * Thrown to indicate that an array has been accessed with an illegal index. The
 * index is either negative or greater than or equal to the size of the array.
 */
public class ArrayIndexOutOfBoundsException extends IndexOutOfBoundsException {
    /**
     * Constructs an {@code ArrayIndexOutOfBoundsException} with no detail
     * message.
     */
    public ArrayIndexOutOfBoundsException() {
        super();
    }

    /**
     * Constructs an {@code ArrayIndexOutOfBoundsException} class with the
     * specified detail message.
     *
     * @param s the detail message.
     */
    public ArrayIndexOutOfBoundsException(String s) {
        super(s);
    }

    /**
     * Constructs a new {@code ArrayIndexOutOfBoundsException} class with an
     * argument indicating the illegal index.
     *
     * The index is included in this exception's detail message.  The
     * exact presentation format of the detail message is unspecified.
     *
     * @param index the illegal index.
     */
    public ArrayIndexOutOfBoundsException(int index) {
        super("Array index out of range: " + index);
    }
}

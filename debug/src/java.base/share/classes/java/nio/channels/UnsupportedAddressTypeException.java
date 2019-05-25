package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to bind or connect
 * to a socket address of a type that is not supported.
 */
public class UnsupportedAddressTypeException extends IllegalArgumentException {
    private static final long serialVersionUID = -2964323842829700493L;

    /**
     * Constructs an instance of this class.
     */
    public UnsupportedAddressTypeException() { }
}

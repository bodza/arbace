package java.lang.reflect;

/**
 * Thrown when a semantically malformed parameterized type is
 * encountered by a reflective method that needs to instantiate it.
 * For example, if the number of type arguments to a parameterized type
 * is wrong.
 */
public class MalformedParameterizedTypeException extends RuntimeException {
    /**
     * Constructs a {@code MalformedParameterizedTypeException} with
     * no detail message.
     */
    public MalformedParameterizedTypeException() {
        super();
    }

    /**
     * Constructs a {@code MalformedParameterizedTypeException} with
     * the given detail message.
     * @param message the detail message; may be {@code null}
     */
    public MalformedParameterizedTypeException(String message) {
        super(message);
    }
}

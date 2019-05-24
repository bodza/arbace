package java.io;

/**
 * Base class for character conversion exceptions.
 */
public class CharConversionException extends java.io.IOException {
    /**
     * This provides no detailed message.
     */
    public CharConversionException() {
    }

    /**
     * This provides a detailed message.
     *
     * @param s the detailed message associated with the exception.
     */
    public CharConversionException(String s) {
        super(s);
    }
}

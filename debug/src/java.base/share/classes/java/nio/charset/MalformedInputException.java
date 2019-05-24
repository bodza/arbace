package java.nio.charset;

/**
 * Checked exception thrown when an input byte sequence is not legal for given
 * charset, or an input character sequence is not a legal sixteen-bit Unicode
 * sequence.
 */
public class MalformedInputException extends CharacterCodingException {
    private int inputLength;

    /**
     * Constructs an {@code MalformedInputException} with the given
     * length.
     * @param inputLength the length of the input
     */
    public MalformedInputException(int inputLength) {
        this.inputLength = inputLength;
    }

    /**
     * Returns the length of the input.
     * @return the length of the input
     */
    public int getInputLength() {
        return inputLength;
    }

    /**
     * Returns the message.
     * @return the message
     */
    public String getMessage() {
        return "Input length = " + inputLength;
    }
}

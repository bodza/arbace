package java.nio.charset;

/**
 * A typesafe enumeration for coding-error actions.
 *
 * Instances of this class are used to specify how malformed-input and
 * unmappable-character errors are to be handled by charset <a
 * href="CharsetDecoder.html#cae">decoders</a> and <a
 * href="CharsetEncoder.html#cae">encoders</a>.
 */
public class CodingErrorAction {
    private String name;

    private CodingErrorAction(String name) {
        this.name = name;
    }

    /**
     * Action indicating that a coding error is to be handled by dropping the
     * erroneous input and resuming the coding operation.
     */
    public static final CodingErrorAction IGNORE = new CodingErrorAction("IGNORE");

    /**
     * Action indicating that a coding error is to be handled by dropping the
     * erroneous input, appending the coder's replacement value to the output
     * buffer, and resuming the coding operation.
     */
    public static final CodingErrorAction REPLACE = new CodingErrorAction("REPLACE");

    /**
     * Action indicating that a coding error is to be reported, either by
     * returning a {@link CoderResult} object or by throwing a {@link
     * CharacterCodingException}, whichever is appropriate for the method
     * implementing the coding process.
     */
    public static final CodingErrorAction REPORT = new CodingErrorAction("REPORT");

    /**
     * Returns a string describing this action.
     *
     * @return A descriptive string
     */
    public String toString() {
        return name;
    }
}

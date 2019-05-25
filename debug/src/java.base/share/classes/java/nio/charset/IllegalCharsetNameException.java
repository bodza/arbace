package java.nio.charset;

/**
 * Unchecked exception thrown when a string that is not a
 * <a href=Charset.html#names>legal charset name</a> is used as such.
 */
public class IllegalCharsetNameException extends IllegalArgumentException {
    private static final long serialVersionUID = 1457525358470002989L;

    private String charsetName;

    /**
     * Constructs an instance of this class.
     *
     * @param  charsetName
     *         The illegal charset name
     */
    public IllegalCharsetNameException(String charsetName) {
        super(String.valueOf(charsetName));
        this.charsetName = charsetName;
    }

    /**
     * Retrieves the illegal charset name.
     *
     * @return  The illegal charset name
     */
    public String getCharsetName() {
        return charsetName;
    }
}

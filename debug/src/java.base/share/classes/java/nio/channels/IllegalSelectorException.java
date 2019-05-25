package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to register a channel
 * with a selector that was not created by the provider that created the
 * channel.
 */
public class IllegalSelectorException extends IllegalArgumentException {
    private static final long serialVersionUID = -8406323347253320987L;

    /**
     * Constructs an instance of this class.
     */
    public IllegalSelectorException() { }
}

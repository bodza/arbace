package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to open a channel
 * in a group that was not created by the same provider.
 */
public class IllegalChannelGroupException extends IllegalArgumentException {
    private static final long serialVersionUID = -2495041211157744253L;

    /**
     * Constructs an instance of this class.
     */
    public IllegalChannelGroupException() { }
}

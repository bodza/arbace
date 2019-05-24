package sun.nio.ch;

import java.nio.channels.spi.SelectorProvider;

/**
 * Creates this platform's default SelectorProvider
 */
public class DefaultSelectorProvider {
    /**
     * Prevent instantiation.
     */
    private DefaultSelectorProvider() { }

    /**
     * Returns the default SelectorProvider.
     */
    public static SelectorProvider create() {
        return new EPollSelectorProvider();
    }
}

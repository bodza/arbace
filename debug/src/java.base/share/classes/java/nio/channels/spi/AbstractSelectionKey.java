package java.nio.channels.spi;

import java.nio.channels.*;

/**
 * Base implementation class for selection keys.
 *
 * This class tracks the validity of the key and implements cancellation.
 */
public abstract class AbstractSelectionKey extends SelectionKey {
    /**
     * Initializes a new instance of this class.
     */
    protected AbstractSelectionKey() { }

    private volatile boolean valid = true;

    public final boolean isValid() {
        return valid;
    }

    void invalidate() {
        valid = false;
    }

    /**
     * Cancels this key.
     *
     * If this key has not yet been cancelled then it is added to its
     * selector's cancelled-key set while synchronized on that set.
     */
    public final void cancel() {
        // Synchronizing "this" to prevent this key from getting canceled
        // multiple times by different threads, which might cause race
        // condition between selector's select() and channel's close().
        synchronized (this) {
            if (valid) {
                valid = false;
                ((AbstractSelector)selector()).cancel(this);
            }
        }
    }
}

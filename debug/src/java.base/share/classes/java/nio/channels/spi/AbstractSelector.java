package java.nio.channels.spi;

import java.io.IOException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.HashSet;
import java.util.Set;
import sun.nio.ch.Interruptible;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Base implementation class for selectors.
 *
 * This class encapsulates the low-level machinery required to implement
 * the interruption of selection operations.  A concrete selector class must
 * invoke the {@link #begin begin} and {@link #end end} methods before and
 * after, respectively, invoking an I/O operation that might block
 * indefinitely.  In order to ensure that the {@link #end end} method is always
 * invoked, these methods should be used within a
 * {@code try}&nbsp;...&nbsp;{@code finally} block:
 *
 * <blockquote><pre id="be">
 * try {
 *     begin();
 *     // Perform blocking I/O operation here
 *     ...
 * } finally {
 *     end();
 * }</pre></blockquote>
 *
 * This class also defines methods for maintaining a selector's
 * cancelled-key set and for removing a key from its channel's key set, and
 * declares the abstract {@link #register register} method that is invoked by a
 * selectable channel's {@link AbstractSelectableChannel#register register}
 * method in order to perform the actual work of registering a channel.
 */
public abstract class AbstractSelector extends Selector {
    private final AtomicBoolean selectorOpen = new AtomicBoolean(true);

    // The provider that created this selector
    private final SelectorProvider provider;

    /**
     * Initializes a new instance of this class.
     *
     * @param provider
     *         The provider that created this selector
     */
    protected AbstractSelector(SelectorProvider provider) {
        this.provider = provider;
    }

    private final Set<SelectionKey> cancelledKeys = new HashSet<SelectionKey>();

    void cancel(SelectionKey k) {
        synchronized (cancelledKeys) {
            cancelledKeys.add(k);
        }
    }

    /**
     * Closes this selector.
     *
     * If the selector has already been closed then this method returns
     * immediately.  Otherwise it marks the selector as closed and then invokes
     * the {@link #implCloseSelector implCloseSelector} method in order to
     * complete the close operation.
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public final void close() throws IOException {
        boolean open = selectorOpen.getAndSet(false);
        if (!open)
            return;
        implCloseSelector();
    }

    /**
     * Closes this selector.
     *
     * This method is invoked by the {@link #close close} method in order
     * to perform the actual work of closing the selector.  This method is only
     * invoked if the selector has not yet been closed, and it is never invoked
     * more than once.
     *
     * An implementation of this method must arrange for any other thread
     * that is blocked in a selection operation upon this selector to return
     * immediately as if by invoking the {@link
     * java.nio.channels.Selector#wakeup wakeup} method.
     *
     * @throws IOException
     *          If an I/O error occurs while closing the selector
     */
    protected abstract void implCloseSelector() throws IOException;

    public final boolean isOpen() {
        return selectorOpen.get();
    }

    /**
     * Returns the provider that created this channel.
     *
     * @return The provider that created this channel
     */
    public final SelectorProvider provider() {
        return provider;
    }

    /**
     * Retrieves this selector's cancelled-key set.
     *
     * This set should only be used while synchronized upon it.
     *
     * @return The cancelled-key set
     */
    protected final Set<SelectionKey> cancelledKeys() {
        return cancelledKeys;
    }

    /**
     * Registers the given channel with this selector.
     *
     * This method is invoked by a channel's {@link
     * AbstractSelectableChannel#register register} method in order to perform
     * the actual work of registering the channel with this selector.
     *
     * @param ch
     *         The channel to be registered
     *
     * @param ops
     *         The initial interest set, which must be valid
     *
     * @param att
     *         The initial attachment for the resulting key
     *
     * @return A new key representing the registration of the given channel
     *          with this selector
     */
    protected abstract SelectionKey register(AbstractSelectableChannel ch, int ops, Object att);

    /**
     * Removes the given key from its channel's key set.
     *
     * This method must be invoked by the selector for each channel that it
     * deregisters.
     *
     * @param key
     *         The selection key to be removed
     */
    protected final void deregister(AbstractSelectionKey key) {
        ((AbstractSelectableChannel)key.channel()).removeKey(key);
    }

    // -- Interruption machinery --

    private Interruptible interruptor = null;

    /**
     * Marks the beginning of an I/O operation that might block indefinitely.
     *
     * This method should be invoked in tandem with the {@link #end end}
     * method, using a {@code try}&nbsp;...&nbsp;{@code finally} block as
     * shown <a href="#be">above</a>, in order to implement interruption for
     * this selector.
     *
     * Invoking this method arranges for the selector's {@link
     * Selector#wakeup wakeup} method to be invoked if a thread's {@link
     * Thread#interrupt interrupt} method is invoked while the thread is
     * blocked in an I/O operation upon the selector.
     */
    protected final void begin() {
        if (interruptor == null) {
            interruptor = new Interruptible() {
                    public void interrupt(Thread ignore) {
                        AbstractSelector.this.wakeup();
                    }};
        }
        Thread.blockedOn(interruptor);
        Thread me = Thread.currentThread();
        if (me.isInterrupted())
            interruptor.interrupt(me);
    }

    /**
     * Marks the end of an I/O operation that might block indefinitely.
     *
     * This method should be invoked in tandem with the {@link #begin begin}
     * method, using a {@code try}&nbsp;...&nbsp;{@code finally} block as
     * shown <a href="#be">above</a>, in order to implement interruption for
     * this selector.
     */
    protected final void end() {
        Thread.blockedOn(null);
    }
}

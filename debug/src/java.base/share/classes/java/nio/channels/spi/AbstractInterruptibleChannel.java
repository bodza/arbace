package java.nio.channels.spi;

import java.io.IOException;
import java.nio.channels.*;
import sun.nio.ch.Interruptible;

/**
 * Base implementation class for interruptible channels.
 *
 * This class encapsulates the low-level machinery required to implement
 * the asynchronous closing and interruption of channels.  A concrete channel
 * class must invoke the {@link #begin begin} and {@link #end end} methods
 * before and after, respectively, invoking an I/O operation that might block
 * indefinitely.  In order to ensure that the {@link #end end} method is always
 * invoked, these methods should be used within a
 * {@code try}&nbsp;...&nbsp;{@code finally} block:
 *
 * <blockquote><pre id="be">
 * boolean completed = false;
 * try {
 *     begin();
 *     completed = ...; // Perform blocking I/O operation
 *     return ...; // Return result
 * } finally {
 *     end(completed);
 * }</pre></blockquote>
 *
 * The {@code completed} argument to the {@link #end end} method tells
 * whether or not the I/O operation actually completed, that is, whether it had
 * any effect that would be visible to the invoker.  In the case of an
 * operation that reads bytes, for example, this argument should be
 * {@code true} if, and only if, some bytes were actually transferred into the
 * invoker's target buffer.
 *
 * A concrete channel class must also implement the {@link
 * #implCloseChannel implCloseChannel} method in such a way that if it is
 * invoked while another thread is blocked in a native I/O operation upon the
 * channel then that operation will immediately return, either by throwing an
 * exception or by returning normally.  If a thread is interrupted or the
 * channel upon which it is blocked is asynchronously closed then the channel's
 * {@link #end end} method will throw the appropriate exception.
 *
 * This class performs the synchronization required to implement the {@link
 * java.nio.channels.Channel} specification.  Implementations of the {@link
 * #implCloseChannel implCloseChannel} method need not synchronize against
 * other threads that might be attempting to close the channel.
 */
public abstract class AbstractInterruptibleChannel implements Channel, InterruptibleChannel {
    private final Object closeLock = new Object();
    private volatile boolean closed;

    /**
     * Initializes a new instance of this class.
     */
    protected AbstractInterruptibleChannel() { }

    /**
     * Closes this channel.
     *
     * If the channel has already been closed then this method returns
     * immediately.  Otherwise it marks the channel as closed and then invokes
     * the {@link #implCloseChannel implCloseChannel} method in order to
     * complete the close operation.
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public final void close() throws IOException {
        synchronized (closeLock) {
            if (closed)
                return;
            closed = true;
            implCloseChannel();
        }
    }

    /**
     * Closes this channel.
     *
     * This method is invoked by the {@link #close close} method in order
     * to perform the actual work of closing the channel.  This method is only
     * invoked if the channel has not yet been closed, and it is never invoked
     * more than once.
     *
     * An implementation of this method must arrange for any other thread
     * that is blocked in an I/O operation upon this channel to return
     * immediately, either by throwing an exception or by returning normally.
     *
     * @throws IOException
     *          If an I/O error occurs while closing the channel
     */
    protected abstract void implCloseChannel() throws IOException;

    public final boolean isOpen() {
        return !closed;
    }

    // -- Interruption machinery --

    private Interruptible interruptor;
    private volatile Thread interrupted;

    /**
     * Marks the beginning of an I/O operation that might block indefinitely.
     *
     * This method should be invoked in tandem with the {@link #end end}
     * method, using a {@code try}&nbsp;...&nbsp;{@code finally} block as
     * shown <a href="#be">above</a>, in order to implement asynchronous
     * closing and interruption for this channel.
     */
    protected final void begin() {
        if (interruptor == null) {
            interruptor = new Interruptible() {
                    public void interrupt(Thread target) {
                        synchronized (closeLock) {
                            if (closed)
                                return;
                            closed = true;
                            interrupted = target;
                            try {
                                AbstractInterruptibleChannel.this.implCloseChannel();
                            } catch (IOException x) { }
                        }
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
     * This method should be invoked in tandem with the {@link #begin
     * begin} method, using a {@code try}&nbsp;...&nbsp;{@code finally} block
     * as shown <a href="#be">above</a>, in order to implement asynchronous
     * closing and interruption for this channel.
     *
     * @param completed
     *         {@code true} if, and only if, the I/O operation completed
     *         successfully, that is, had some effect that would be visible to
     *         the operation's invoker
     *
     * @throws AsynchronousCloseException
     *          If the channel was asynchronously closed
     *
     * @throws ClosedByInterruptException
     *          If the thread blocked in the I/O operation was interrupted
     */
    protected final void end(boolean completed) throws AsynchronousCloseException
    {
        Thread.blockedOn(null);
        Thread interrupted = this.interrupted;
        if (interrupted != null && interrupted == Thread.currentThread()) {
            this.interrupted = null;
            throw new ClosedByInterruptException();
        }
        if (!completed && closed)
            throw new AsynchronousCloseException();
    }
}

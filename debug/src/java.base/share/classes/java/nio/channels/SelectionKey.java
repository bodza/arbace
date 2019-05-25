package java.nio.channels;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

/**
 * A token representing the registration of a {@link SelectableChannel} with a
 * {@link Selector}.
 *
 * A selection key is created each time a channel is registered with a
 * selector.  A key remains valid until it is <i>cancelled</i> by invoking its
 * {@link #cancel cancel} method, by closing its channel, or by closing its
 * selector.  Cancelling a key does not immediately remove it from its
 * selector; it is instead added to the selector's <a
 * href="Selector.html#ks"><i>cancelled-key set</i></a> for removal during the
 * next selection operation.  The validity of a key may be tested by invoking
 * its {@link #isValid isValid} method.
 *
 * <a id="opsets"></a>
 *
 * A selection key contains two <i>operation sets</i> represented as
 * integer values.  Each bit of an operation set denotes a category of
 * selectable operations that are supported by the key's channel.
 *
 * <ul>
 *   <li>The <i>interest set</i> determines which operation categories will
 *   be tested for readiness the next time one of the selector's selection
 *   methods is invoked.  The interest set is initialized with the value given
 *   when the key is created; it may later be changed via the {@link
 *   #interestOps(int)} method.</li>
 *
 *   <li>The <i>ready set</i> identifies the operation categories for which
 *   the key's channel has been detected to be ready by the key's selector.
 *   The ready set is initialized to zero when the key is created; it may later
 *   be updated by the selector during a selection operation, but it cannot be
 *   updated directly.</li>
 * </ul>
 *
 * That a selection key's ready set indicates that its channel is ready for
 * some operation category is a hint, but not a guarantee, that an operation in
 * such a category may be performed by a thread without causing the thread to
 * block.  A ready set is most likely to be accurate immediately after the
 * completion of a selection operation.  It is likely to be made inaccurate by
 * external events and by I/O operations that are invoked upon the
 * corresponding channel.
 *
 * This class defines all known operation-set bits, but precisely which
 * bits are supported by a given channel depends upon the type of the channel.
 * Each subclass of {@link SelectableChannel} defines an {@link
 * SelectableChannel#validOps() validOps()} method which returns a set
 * identifying just those operations that are supported by the channel.  An
 * attempt to set or test an operation-set bit that is not supported by a key's
 * channel will result in an appropriate run-time exception.
 *
 * It is often necessary to associate some application-specific data with a
 * selection key, for example an object that represents the state of a
 * higher-level protocol and handles readiness notifications in order to
 * implement that protocol.  Selection keys therefore support the
 * <i>attachment</i> of a single arbitrary object to a key.  An object can be
 * attached via the {@link #attach attach} method and then later retrieved via
 * the {@link #attachment() attachment} method.
 *
 * Selection keys are safe for use by multiple concurrent threads.  A
 * selection operation will always use the interest-set value that was current
 * at the moment that the operation began.
 */
public abstract class SelectionKey {
    /**
     * Constructs an instance of this class.
     */
    protected SelectionKey() { }

    // -- Channel and selector operations --

    /**
     * Returns the channel for which this key was created.  This method will
     * continue to return the channel even after the key is cancelled.
     *
     * @return This key's channel
     */
    public abstract SelectableChannel channel();

    /**
     * Returns the selector for which this key was created.  This method will
     * continue to return the selector even after the key is cancelled.
     *
     * @return This key's selector
     */
    public abstract Selector selector();

    /**
     * Tells whether or not this key is valid.
     *
     * A key is valid upon creation and remains so until it is cancelled,
     * its channel is closed, or its selector is closed.
     *
     * @return {@code true} if, and only if, this key is valid
     */
    public abstract boolean isValid();

    /**
     * Requests that the registration of this key's channel with its selector
     * be cancelled.  Upon return the key will be invalid and will have been
     * added to its selector's cancelled-key set.  The key will be removed from
     * all of the selector's key sets during the next selection operation.
     *
     * If this key has already been cancelled then invoking this method has
     * no effect.  Once cancelled, a key remains forever invalid.
     *
     * This method may be invoked at any time.  It synchronizes on the
     * selector's cancelled-key set, and therefore may block briefly if invoked
     * concurrently with a cancellation or selection operation involving the
     * same selector.
     */
    public abstract void cancel();

    // -- Operation-set accessors --

    /**
     * Retrieves this key's interest set.
     *
     * It is guaranteed that the returned set will only contain operation
     * bits that are valid for this key's channel.
     *
     * @return This key's interest set
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public abstract int interestOps();

    /**
     * Sets this key's interest set to the given value.
     *
     * This method may be invoked at any time.  If this method is invoked
     * while a selection operation is in progress then it has no effect upon
     * that operation; the change to the key's interest set will be seen by the
     * next selection operation.
     *
     * @param ops  The new interest set
     *
     * @return This selection key
     *
     * @throws IllegalArgumentException
     *          If a bit in the set does not correspond to an operation that
     *          is supported by this key's channel, that is, if
     *          {@code (ops & ~channel().validOps()) != 0}
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public abstract SelectionKey interestOps(int ops);

    /**
     * Atomically sets this key's interest set to the bitwise union ("or") of
     * the existing interest set and the given value. This method is guaranteed
     * to be atomic with respect to other concurrent calls to this method or to
     * {@link #interestOpsAnd(int)}.
     *
     * This method may be invoked at any time.  If this method is invoked
     * while a selection operation is in progress then it has no effect upon
     * that operation; the change to the key's interest set will be seen by the
     * next selection operation.
     *
     * @implSpec The default implementation synchronizes on this key and invokes
     * {@code interestOps()} and {@code interestOps(int)} to retrieve and set
     * this key's interest set.
     *
     * @param ops  The interest set to apply
     *
     * @return The previous interest set
     *
     * @throws IllegalArgumentException
     *          If a bit in the set does not correspond to an operation that
     *          is supported by this key's channel, that is, if
     *          {@code (ops & ~channel().validOps()) != 0}
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public int interestOpsOr(int ops) {
        synchronized (this) {
            int oldVal = interestOps();
            interestOps(oldVal | ops);
            return oldVal;
        }
    }

    /**
     * Atomically sets this key's interest set to the bitwise intersection ("and")
     * of the existing interest set and the given value. This method is guaranteed
     * to be atomic with respect to other concurrent calls to this method or to
     * {@link #interestOpsOr(int)}.
     *
     * This method may be invoked at any time.  If this method is invoked
     * while a selection operation is in progress then it has no effect upon
     * that operation; the change to the key's interest set will be seen by the
     * next selection operation.
     *
     * @apiNote Unlike the {@code interestOps(int)} and {@code interestOpsOr(int)}
     * methods, this method does not throw {@code IllegalArgumentException} when
     * invoked with bits in the interest set that do not correspond to an
     * operation that is supported by this key's channel. This is to allow
     * operation bits in the interest set to be cleared using bitwise complement
     * values, e.g., {@code interestOpsAnd(~SelectionKey.OP_READ)} will remove
     * the {@code OP_READ} from the interest set without affecting other bits.
     *
     * @implSpec The default implementation synchronizes on this key and invokes
     * {@code interestOps()} and {@code interestOps(int)} to retrieve and set
     * this key's interest set.
     *
     * @param ops  The interest set to apply
     *
     * @return The previous interest set
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public int interestOpsAnd(int ops) {
        synchronized (this) {
            int oldVal = interestOps();
            interestOps(oldVal & ops);
            return oldVal;
        }
    }

    /**
     * Retrieves this key's ready-operation set.
     *
     * It is guaranteed that the returned set will only contain operation
     * bits that are valid for this key's channel.
     *
     * @return This key's ready-operation set
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public abstract int readyOps();

    // -- Operation bits and bit-testing convenience methods --

    /**
     * Operation-set bit for read operations.
     *
     * Suppose that a selection key's interest set contains
     * {@code OP_READ} at the start of a <a
     * href="Selector.html#selop">selection operation</a>.  If the selector
     * detects that the corresponding channel is ready for reading, has reached
     * end-of-stream, has been remotely shut down for further reading, or has
     * an error pending, then it will add {@code OP_READ} to the key's
     * ready-operation set.
     */
    public static final int OP_READ = 1 << 0;

    /**
     * Operation-set bit for write operations.
     *
     * Suppose that a selection key's interest set contains
     * {@code OP_WRITE} at the start of a <a
     * href="Selector.html#selop">selection operation</a>.  If the selector
     * detects that the corresponding channel is ready for writing, has been
     * remotely shut down for further writing, or has an error pending, then it
     * will add {@code OP_WRITE} to the key's ready set.
     */
    public static final int OP_WRITE = 1 << 2;

    /**
     * Operation-set bit for socket-connect operations.
     *
     * Suppose that a selection key's interest set contains
     * {@code OP_CONNECT} at the start of a <a
     * href="Selector.html#selop">selection operation</a>.  If the selector
     * detects that the corresponding socket channel is ready to complete its
     * connection sequence, or has an error pending, then it will add
     * {@code OP_CONNECT} to the key's ready set.
     */
    public static final int OP_CONNECT = 1 << 3;

    /**
     * Operation-set bit for socket-accept operations.
     *
     * Suppose that a selection key's interest set contains
     * {@code OP_ACCEPT} at the start of a <a
     * href="Selector.html#selop">selection operation</a>.  If the selector
     * detects that the corresponding server-socket channel is ready to accept
     * another connection, or has an error pending, then it will add
     * {@code OP_ACCEPT} to the key's ready set.
     */
    public static final int OP_ACCEPT = 1 << 4;

    /**
     * Tests whether this key's channel is ready for reading.
     *
     * An invocation of this method of the form {@code k.isReadable()}
     * behaves in exactly the same way as the expression
     *
     * <blockquote><pre>{@code
     * k.readyOps() & OP_READ != 0
     * }</pre></blockquote>
     *
     * If this key's channel does not support read operations then this
     * method always returns {@code false}.
     *
     * @return {@code true} if, and only if,
     *          {@code readyOps() & OP_READ} is nonzero
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public final boolean isReadable() {
        return (readyOps() & OP_READ) != 0;
    }

    /**
     * Tests whether this key's channel is ready for writing.
     *
     * An invocation of this method of the form {@code k.isWritable()}
     * behaves in exactly the same way as the expression
     *
     * <blockquote><pre>{@code
     * k.readyOps() & OP_WRITE != 0
     * }</pre></blockquote>
     *
     * If this key's channel does not support write operations then this
     * method always returns {@code false}.
     *
     * @return {@code true} if, and only if,
     *          {@code readyOps() & OP_WRITE} is nonzero
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public final boolean isWritable() {
        return (readyOps() & OP_WRITE) != 0;
    }

    /**
     * Tests whether this key's channel has either finished, or failed to
     * finish, its socket-connection operation.
     *
     * An invocation of this method of the form {@code k.isConnectable()}
     * behaves in exactly the same way as the expression
     *
     * <blockquote><pre>{@code
     * k.readyOps() & OP_CONNECT != 0
     * }</pre></blockquote>
     *
     * If this key's channel does not support socket-connect operations
     * then this method always returns {@code false}.
     *
     * @return {@code true} if, and only if,
     *          {@code readyOps() & OP_CONNECT} is nonzero
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public final boolean isConnectable() {
        return (readyOps() & OP_CONNECT) != 0;
    }

    /**
     * Tests whether this key's channel is ready to accept a new socket
     * connection.
     *
     * An invocation of this method of the form {@code k.isAcceptable()}
     * behaves in exactly the same way as the expression
     *
     * <blockquote><pre>{@code
     * k.readyOps() & OP_ACCEPT != 0
     * }</pre></blockquote>
     *
     * If this key's channel does not support socket-accept operations then
     * this method always returns {@code false}.
     *
     * @return {@code true} if, and only if,
     *          {@code readyOps() & OP_ACCEPT} is nonzero
     *
     * @throws CancelledKeyException
     *          If this key has been cancelled
     */
    public final boolean isAcceptable() {
        return (readyOps() & OP_ACCEPT) != 0;
    }

    // -- Attachments --

    private volatile Object attachment;

    private static final AtomicReferenceFieldUpdater<SelectionKey,Object> attachmentUpdater = AtomicReferenceFieldUpdater.newUpdater(SelectionKey.class, Object.class, "attachment");

    /**
     * Attaches the given object to this key.
     *
     * An attached object may later be retrieved via the {@link #attachment()
     * attachment} method.  Only one object may be attached at a time; invoking
     * this method causes any previous attachment to be discarded.  The current
     * attachment may be discarded by attaching {@code null}.
     *
     * @param ob
     *         The object to be attached; may be {@code null}
     *
     * @return The previously-attached object, if any,
     *          otherwise {@code null}
     */
    public final Object attach(Object ob) {
        return attachmentUpdater.getAndSet(this, ob);
    }

    /**
     * Retrieves the current attachment.
     *
     * @return The object currently attached to this key,
     *          or {@code null} if there is no attachment
     */
    public final Object attachment() {
        return attachment;
    }
}

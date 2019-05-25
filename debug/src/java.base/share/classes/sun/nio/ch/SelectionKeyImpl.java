package sun.nio.ch;

import java.nio.channels.CancelledKeyException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.spi.AbstractSelectionKey;

/**
 * An implementation of SelectionKey.
 */
public final class SelectionKeyImpl extends AbstractSelectionKey {
    private static final VarHandle INTERESTOPS = ConstantBootstraps.fieldVarHandle(MethodHandles.lookup(), "interestOps", VarHandle.class, SelectionKeyImpl.class, int.class);

    private final SelChImpl channel;
    private final SelectorImpl selector;

    private volatile int interestOps;
    private volatile int readyOps;

    // registered events in kernel, used by some Selector implementations
    private int registeredEvents;

    // index of key in pollfd array, used by some Selector implementations
    private int index;

    SelectionKeyImpl(SelChImpl ch, SelectorImpl sel) {
        channel = ch;
        selector = sel;
    }

    private void ensureValid() {
        if (!isValid())
            throw new CancelledKeyException();
    }

    int getFDVal() {
        return channel.getFDVal();
    }

    @Override
    public SelectableChannel channel() {
        return (SelectableChannel)channel;
    }

    @Override
    public Selector selector() {
        return selector;
    }

    @Override
    public int interestOps() {
        ensureValid();
        return interestOps;
    }

    @Override
    public SelectionKey interestOps(int ops) {
        ensureValid();
        if ((ops & ~channel().validOps()) != 0)
            throw new IllegalArgumentException();
        int oldOps = (int) INTERESTOPS.getAndSet(this, ops);
        if (ops != oldOps) {
            selector.setEventOps(this);
        }
        return this;
    }

    @Override
    public int interestOpsOr(int ops) {
        ensureValid();
        if ((ops & ~channel().validOps()) != 0)
            throw new IllegalArgumentException();
        int oldVal = (int) INTERESTOPS.getAndBitwiseOr(this, ops);
        if (oldVal != (oldVal | ops)) {
            selector.setEventOps(this);
        }
        return oldVal;
    }

    @Override
    public int interestOpsAnd(int ops) {
        ensureValid();
        int oldVal = (int) INTERESTOPS.getAndBitwiseAnd(this, ops);
        if (oldVal != (oldVal & ops)) {
            selector.setEventOps(this);
        }
        return oldVal;
    }

    @Override
    public int readyOps() {
        ensureValid();
        return readyOps;
    }

    // The nio versions of these operations do not care if a key
    // has been invalidated. They are for internal use by nio code.

    public void nioReadyOps(int ops) {
        readyOps = ops;
    }

    public int nioReadyOps() {
        return readyOps;
    }

    public SelectionKey nioInterestOps(int ops) {
        if ((ops & ~channel().validOps()) != 0)
            throw new IllegalArgumentException();
        interestOps = ops;
        selector.setEventOps(this);
        return this;
    }

    public int nioInterestOps() {
        return interestOps;
    }

    int translateInterestOps() {
        return channel.translateInterestOps(interestOps);
    }

    boolean translateAndSetReadyOps(int ops) {
        return channel.translateAndSetReadyOps(ops, this);
    }

    boolean translateAndUpdateReadyOps(int ops) {
        return channel.translateAndUpdateReadyOps(ops, this);
    }

    void registeredEvents(int events) {
        // assert Thread.holdsLock(selector);
        this.registeredEvents = events;
    }

    int registeredEvents() {
        // assert Thread.holdsLock(selector);
        return registeredEvents;
    }

    int getIndex() {
        return index;
    }

    void setIndex(int i) {
        index = i;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("channel=")
          .append(channel)
          .append(", selector=")
          .append(selector);
        if (isValid()) {
            sb.append(", interestOps=")
              .append(interestOps)
              .append(", readyOps=")
              .append(readyOps);
        } else {
            sb.append(", invalid");
        }
        return sb.toString();
    }

    // used by Selector implementations to record when the key was selected
    int lastPolled;
}

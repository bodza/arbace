package sun.nio.ch;

import java.nio.channels.*;
import java.net.SocketAddress;
import java.net.SocketOption;
import java.net.StandardSocketOptions;
import java.net.InetSocketAddress;
import java.io.FileDescriptor;
import java.io.IOException;
import java.util.Set;
import java.util.HashSet;
import java.util.Collections;
import java.util.concurrent.Future;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Base implementation of AsynchronousServerSocketChannel.
 */
abstract class AsynchronousServerSocketChannelImpl extends AsynchronousServerSocketChannel implements Cancellable, Groupable {
    protected final FileDescriptor fd;

    // the local address to which the channel's socket is bound
    protected volatile InetSocketAddress localAddress;

    // need this lock to set local address
    private final Object stateLock = new Object();

    // close support
    private ReadWriteLock closeLock = new ReentrantReadWriteLock();
    private volatile boolean closed;

    // set true when accept operation is cancelled
    private volatile boolean acceptKilled;

    // set true when exclusive binding is on and SO_REUSEADDR is emulated
    private boolean isReuseAddress;

    AsynchronousServerSocketChannelImpl(AsynchronousChannelGroupImpl group) {
        super(group.provider());
        this.fd = Net.serverSocket(true);
    }

    @Override
    public final boolean isOpen() {
        return !closed;
    }

    /**
     * Marks beginning of access to file descriptor/handle
     */
    final void begin() throws IOException {
        closeLock.readLock().lock();
        if (!isOpen())
            throw new ClosedChannelException();
    }

    /**
     * Marks end of access to file descriptor/handle
     */
    final void end() {
        closeLock.readLock().unlock();
    }

    /**
     * Invoked to close file descriptor/handle.
     */
    abstract void implClose() throws IOException;

    @Override
    public final void close() throws IOException {
        // synchronize with any threads using file descriptor/handle
        closeLock.writeLock().lock();
        try {
            if (closed)
                return; // already closed
            closed = true;
        } finally {
            closeLock.writeLock().unlock();
        }
        implClose();
    }

    /**
     * Invoked by accept to accept connection
     */
    abstract Future<AsynchronousSocketChannel>
        implAccept(Object attachment,
                   CompletionHandler<AsynchronousSocketChannel,Object> handler);

    @Override
    public final Future<AsynchronousSocketChannel> accept() {
        return implAccept(null, null);
    }

    @Override
    @SuppressWarnings("unchecked")
    public final <A> void accept(A attachment, CompletionHandler<AsynchronousSocketChannel,? super A> handler)
    {
        if (handler == null)
            throw new NullPointerException("'handler' is null");
        implAccept(attachment, (CompletionHandler<AsynchronousSocketChannel,Object>)handler);
    }

    final boolean isAcceptKilled() {
        return acceptKilled;
    }

    @Override
    public final void onCancel(PendingFuture<?,?> task) {
        acceptKilled = true;
    }

    @Override
    public final AsynchronousServerSocketChannel bind(SocketAddress local, int backlog) throws IOException
    {
        InetSocketAddress isa = (local == null) ? new InetSocketAddress(0) : Net.checkAddress(local);

        try {
            begin();
            synchronized (stateLock) {
                if (localAddress != null)
                    throw new AlreadyBoundException();
                Net.bind(fd, isa.getAddress(), isa.getPort());
                Net.listen(fd, backlog < 1 ? 50 : backlog);
                localAddress = Net.localAddress(fd);
            }
        } finally {
            end();
        }
        return this;
    }

    @Override
    public final SocketAddress getLocalAddress() throws IOException {
        if (!isOpen())
            throw new ClosedChannelException();
        return Net.getRevealedLocalAddress(localAddress);
    }

    @Override
    public final <T> AsynchronousServerSocketChannel setOption(SocketOption<T> name, T value) throws IOException
    {
        if (name == null)
            throw new NullPointerException();
        if (!supportedOptions().contains(name))
            throw new UnsupportedOperationException("'" + name + "' not supported");

        try {
            begin();
            if (name == StandardSocketOptions.SO_REUSEADDR && Net.useExclusiveBind())
            {
                // SO_REUSEADDR emulated when using exclusive bind
                isReuseAddress = (Boolean)value;
            } else {
                Net.setSocketOption(fd, Net.UNSPEC, name, value);
            }
            return this;
        } finally {
            end();
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public final <T> T getOption(SocketOption<T> name) throws IOException {
        if (name == null)
            throw new NullPointerException();
        if (!supportedOptions().contains(name))
            throw new UnsupportedOperationException("'" + name + "' not supported");

        try {
            begin();
            if (name == StandardSocketOptions.SO_REUSEADDR && Net.useExclusiveBind())
            {
                // SO_REUSEADDR emulated when using exclusive bind
                return (T)Boolean.valueOf(isReuseAddress);
            }
            return (T) Net.getSocketOption(fd, Net.UNSPEC, name);
        } finally {
            end();
        }
    }

    private static class DefaultOptionsHolder {
        static final Set<SocketOption<?>> defaultOptions = defaultOptions();

        private static Set<SocketOption<?>> defaultOptions() {
            HashSet<SocketOption<?>> set = new HashSet<>(2);
            set.add(StandardSocketOptions.SO_RCVBUF);
            set.add(StandardSocketOptions.SO_REUSEADDR);
            if (Net.isReusePortAvailable()) {
                set.add(StandardSocketOptions.SO_REUSEPORT);
            }
            return Collections.unmodifiableSet(set);
        }
    }

    @Override
    public final Set<SocketOption<?>> supportedOptions() {
        return DefaultOptionsHolder.defaultOptions;
    }

    @Override
    public final String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getClass().getName());
        sb.append('[');
        if (!isOpen())
            sb.append("closed");
        else {
            if (localAddress == null) {
                sb.append("unbound");
            } else {
                sb.append(Net.getRevealedLocalAddressAsString(localAddress));
            }
        }
        sb.append(']');
        return sb.toString();
    }
}

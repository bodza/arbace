package sun.nio.ch;

import java.nio.channels.*;
import java.nio.channels.spi.AsynchronousChannelProvider;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadFactory;
import java.io.IOException;

public class BsdAsynchronousChannelProvider extends AsynchronousChannelProvider {
    private static volatile KQueuePort defaultPort;

    private KQueuePort defaultEventPort() throws IOException {
        if (defaultPort == null) {
            synchronized (BsdAsynchronousChannelProvider.class) {
                if (defaultPort == null) {
                    defaultPort = new KQueuePort(this, ThreadPool.getDefault()).start();
                }
            }
        }
        return defaultPort;
    }

    public BsdAsynchronousChannelProvider() {
    }

    @Override
    public AsynchronousChannelGroup openAsynchronousChannelGroup(int nThreads, ThreadFactory factory) throws IOException
    {
        return new KQueuePort(this, ThreadPool.create(nThreads, factory)).start();
    }

    @Override
    public AsynchronousChannelGroup openAsynchronousChannelGroup(ExecutorService executor, int initialSize) throws IOException
    {
        return new KQueuePort(this, ThreadPool.wrap(executor, initialSize)).start();
    }

    private Port toPort(AsynchronousChannelGroup group) throws IOException {
        if (group == null) {
            return defaultEventPort();
        } else {
            if (!(group instanceof KQueuePort))
                throw new IllegalChannelGroupException();
            return (Port)group;
        }
    }

    @Override
    public AsynchronousServerSocketChannel openAsynchronousServerSocketChannel(AsynchronousChannelGroup group) throws IOException
    {
        return new UnixAsynchronousServerSocketChannelImpl(toPort(group));
    }

    @Override
    public AsynchronousSocketChannel openAsynchronousSocketChannel(AsynchronousChannelGroup group) throws IOException
    {
        return new UnixAsynchronousSocketChannelImpl(toPort(group));
    }
}

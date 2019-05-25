package java.nio.channels;

import java.io.IOException;
import java.nio.channels.spi.*;

/**
 * A pair of channels that implements a unidirectional pipe.
 *
 * A pipe consists of a pair of channels: A writable {@link
 * Pipe.SinkChannel sink} channel and a readable {@link Pipe.SourceChannel source}
 * channel.  Once some bytes are written to the sink channel they can be read
 * from the source channel in exactly the order in which they were written.
 *
 * Whether or not a thread writing bytes to a pipe will block until another
 * thread reads those bytes, or some previously-written bytes, from the pipe is
 * system-dependent and therefore unspecified.  Many pipe implementations will
 * buffer up to a certain number of bytes between the sink and source channels,
 * but such buffering should not be assumed.
 */
public abstract class Pipe {
    /**
     * A channel representing the readable end of a {@link Pipe}.
     */
    public abstract static class SourceChannel extends AbstractSelectableChannel implements ReadableByteChannel, ScatteringByteChannel {
        /**
         * Constructs a new instance of this class.
         *
         * @param provider
         *         The selector provider
         */
        protected SourceChannel(SelectorProvider provider) {
            super(provider);
        }

        /**
         * Returns an operation set identifying this channel's supported
         * operations.
         *
         * Pipe-source channels only support reading, so this method
         * returns {@link SelectionKey#OP_READ}.
         *
         * @return The valid-operation set
         */
        public final int validOps() {
            return SelectionKey.OP_READ;
        }
    }

    /**
     * A channel representing the writable end of a {@link Pipe}.
     */
    public abstract static class SinkChannel extends AbstractSelectableChannel implements WritableByteChannel, GatheringByteChannel {
        /**
         * Initializes a new instance of this class.
         *
         * @param provider
         *         The selector provider
         */
        protected SinkChannel(SelectorProvider provider) {
            super(provider);
        }

        /**
         * Returns an operation set identifying this channel's supported
         * operations.
         *
         * Pipe-sink channels only support writing, so this method returns
         * {@link SelectionKey#OP_WRITE}.
         *
         * @return The valid-operation set
         */
        public final int validOps() {
            return SelectionKey.OP_WRITE;
        }
    }

    /**
     * Initializes a new instance of this class.
     */
    protected Pipe() { }

    /**
     * Returns this pipe's source channel.
     *
     * @return This pipe's source channel
     */
    public abstract SourceChannel source();

    /**
     * Returns this pipe's sink channel.
     *
     * @return This pipe's sink channel
     */
    public abstract SinkChannel sink();

    /**
     * Opens a pipe.
     *
     * The new pipe is created by invoking the {@link
     * java.nio.channels.spi.SelectorProvider#openPipe openPipe} method of the
     * system-wide default {@link java.nio.channels.spi.SelectorProvider}
     * object.
     *
     * @return A new pipe
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    public static Pipe open() throws IOException {
        return SelectorProvider.provider().openPipe();
    }
}

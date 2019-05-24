package java.nio.channels;

import java.io.IOException;

/**
 * A channel that can read and write bytes.  This interface simply unifies
 * {@link ReadableByteChannel} and {@link WritableByteChannel}; it does not
 * specify any new operations.
 */
public interface ByteChannel extends ReadableByteChannel, WritableByteChannel {
}

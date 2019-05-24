package sun.nio.cs;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CoderResult;

/**
 * A decoder that can be delegated to by another decoder
 * when normal inheritance cannot be used.
 * Used by autodecting decoders.
 */
public interface DelegatableDecoder {
    CoderResult decodeLoop(ByteBuffer src, CharBuffer dst);
    void implReset();
    CoderResult implFlush(CharBuffer out);
}

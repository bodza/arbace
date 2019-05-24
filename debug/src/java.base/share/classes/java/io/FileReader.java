package java.io;

import java.nio.charset.Charset;

/**
 * Reads text from character files using a default buffer size. Decoding from bytes
 * to characters uses either a specified {@linkplain java.nio.charset.Charset charset}
 * or the platform's
 * {@linkplain java.nio.charset.Charset#defaultCharset() default charset}.
 *
 * The {@code FileReader} is meant for reading streams of characters. For reading
 * streams of raw bytes, consider using a {@code FileInputStream}.
 */
public class FileReader extends InputStreamReader {
    /**
     * Creates a new {@code FileReader}, given the {@code FileDescriptor} to read,
     * using the platform's
     * {@linkplain java.nio.charset.Charset#defaultCharset() default charset}.
     *
     * @param fd the {@code FileDescriptor} to read
     */
    public FileReader(FileDescriptor fd) {
        super(new FileInputStream(fd));
    }
}

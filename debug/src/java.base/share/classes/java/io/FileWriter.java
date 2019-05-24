package java.io;

import java.nio.charset.Charset;

/**
 * Writes text to character files using a default buffer size. Encoding from characters
 * to bytes uses either a specified {@linkplain java.nio.charset.Charset charset}
 * or the platform's
 * {@linkplain java.nio.charset.Charset#defaultCharset() default charset}.
 *
 * Whether or not a file is available or may be created depends upon the
 * underlying platform.  Some platforms, in particular, allow a file to be
 * opened for writing by only one {@code FileWriter} (or other file-writing
 * object) at a time.  In such situations the constructors in this class
 * will fail if the file involved is already open.
 *
 * The {@code FileWriter} is meant for writing streams of characters. For writing
 * streams of raw bytes, consider using a {@code FileOutputStream}.
 */
public class FileWriter extends OutputStreamWriter {
    /**
     * Constructs a {@code FileWriter} given a file descriptor,
     * using the platform's
     * {@linkplain java.nio.charset.Charset#defaultCharset() default charset}.
     *
     * @param fd  the {@code FileDescriptor} to write.
     */
    public FileWriter(FileDescriptor fd) {
        super(new FileOutputStream(fd));
    }
}

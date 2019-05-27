package java.io;

/**
 * A <code>FileInputStream</code> obtains input bytes
 * from a file in a file system. What files
 * are  available depends on the host environment.
 *
 * <code>FileInputStream</code> is meant for reading streams of raw bytes
 * such as image data. For reading streams of characters, consider using
 * <code>FileReader</code>.
 *
 * @apiNote
 * To release resources used by this stream {@link #close} should be called
 * directly or by try-with-resources. Subclasses are responsible for the cleanup
 * of resources acquired by the subclass.
 * Subclasses that override {@link #finalize} in order to perform cleanup
 * should be modified to use alternative cleanup mechanisms such as
 * {@link java.lang.ref.Cleaner} and remove the overriding {@code finalize} method.
 *
 * @implSpec
 * If this FileInputStream has been subclassed and the {@link #close}
 * method has been overridden, the {@link #close} method will be
 * called when the FileInputStream is unreachable.
 * Otherwise, it is implementation specific how the resource cleanup described in
 * {@link #close} is performed.
 */
public class FileInputStream extends InputStream {
    /* File Descriptor - handle to the open file */
    private final FileDescriptor fd;

    /**
     * The path of the referenced file
     * (null if the stream is created with a file descriptor)
     */
    private final String path;

    private final Object closeLock = new Object();

    private volatile boolean closed;

    private final Object altFinalizer;

    /**
     * Creates a <code>FileInputStream</code> by using the file descriptor
     * <code>fdo</code>, which represents an existing connection to an
     * actual file in the file system.
     *
     * If <code>fdo</code> is null then a <code>NullPointerException</code>
     * is thrown.
     *
     * This constructor does not throw an exception if <code>fdo</code>
     * is {@link java.io.FileDescriptor#valid() invalid}.
     * However, if the methods are invoked on the resulting stream to attempt
     * I/O on the stream, an <code>IOException</code> is thrown.
     *
     * @param fdo   the file descriptor to be opened for reading.
     */
    public FileInputStream(FileDescriptor fdo) {
        if (fdo == null) {
            throw new NullPointerException();
        }
        fd = fdo;
        path = null;
        altFinalizer = null;

        /*
         * FileDescriptor is being shared by streams.
         * Register this stream with FileDescriptor tracker.
         */
        fd.attach(this);
    }

    /**
     * Reads a byte of data from this input stream. This method blocks
     * if no input is yet available.
     *
     * @return the next byte of data, or <code>-1</code> if the end of the
     *             file is reached.
     * @throws IOException  if an I/O error occurs.
     */
    public int read() throws IOException {
        return read0();
    }

    private native int read0() throws IOException;

    /**
     * Reads a subarray as a sequence of bytes.
     * @param b the data to be written
     * @param off the start offset in the data
     * @param len the number of bytes that are written
     * @throws IOException If an I/O error has occurred.
     */
    private native int readBytes(byte b[], int off, int len) throws IOException;

    /**
     * Reads up to <code>b.length</code> bytes of data from this input
     * stream into an array of bytes. This method blocks until some input
     * is available.
     *
     * @param b   the buffer into which the data is read.
     * @return the total number of bytes read into the buffer, or
     *             <code>-1</code> if there is no more data because the end of
     *             the file has been reached.
     * @throws IOException  if an I/O error occurs.
     */
    public int read(byte b[]) throws IOException {
        return readBytes(b, 0, b.length);
    }

    /**
     * Reads up to <code>len</code> bytes of data from this input stream
     * into an array of bytes. If <code>len</code> is not zero, the method
     * blocks until some input is available; otherwise, no
     * bytes are read and <code>0</code> is returned.
     *
     * @param b     the buffer into which the data is read.
     * @param off   the start offset in the destination array <code>b</code>
     * @param len   the maximum number of bytes read.
     * @return the total number of bytes read into the buffer, or
     *             <code>-1</code> if there is no more data because the end of
     *             the file has been reached.
     * @throws NullPointerException If <code>b</code> is <code>null</code>.
     * @throws IndexOutOfBoundsException If <code>off</code> is negative,
     * <code>len</code> is negative, or <code>len</code> is greater than
     * <code>b.length - off</code>
     * @throws IOException  if an I/O error occurs.
     */
    public int read(byte b[], int off, int len) throws IOException {
        return readBytes(b, off, len);
    }

    /**
     * Skips over and discards <code>n</code> bytes of data from the
     * input stream.
     *
     * The <code>skip</code> method may, for a variety of
     * reasons, end up skipping over some smaller number of bytes,
     * possibly <code>0</code>. If <code>n</code> is negative, the method
     * will try to skip backwards. In case the backing file does not support
     * backward skip at its current position, an <code>IOException</code> is
     * thrown. The actual number of bytes skipped is returned. If it skips
     * forwards, it returns a positive value. If it skips backwards, it
     * returns a negative value.
     *
     * This method may skip more bytes than what are remaining in the
     * backing file. This produces no exception and the number of bytes skipped
     * may include some number of bytes that were beyond the EOF of the
     * backing file. Attempting to read from the stream after skipping past
     * the end will result in -1 indicating the end of the file.
     *
     * @param n   the number of bytes to be skipped.
     * @return the actual number of bytes skipped.
     * @throws IOException  if n is negative, if the stream does not
     *             support seek, or if an I/O error occurs.
     */
    public long skip(long n) throws IOException {
        return skip0(n);
    }

    private native long skip0(long n) throws IOException;

    /**
     * Returns an estimate of the number of remaining bytes that can be read (or
     * skipped over) from this input stream without blocking by the next
     * invocation of a method for this input stream. Returns 0 when the file
     * position is beyond EOF. The next invocation might be the same thread
     * or another thread. A single read or skip of this many bytes will not
     * block, but may read or skip fewer bytes.
     *
     * In some cases, a non-blocking read (or skip) may appear to be
     * blocked when it is merely slow, for example when reading large
     * files over slow networks.
     *
     * @return an estimate of the number of remaining bytes that can be read
     *             (or skipped over) from this input stream without blocking.
     * @throws IOException  if this file input stream has been closed by calling
     *             {@code close} or an I/O error occurs.
     */
    public int available() throws IOException {
        return available0();
    }

    private native int available0() throws IOException;

    /**
     * Closes this file input stream and releases any system resources
     * associated with the stream.
     *
     * @apiNote
     * Overriding {@link #close} to perform cleanup actions is reliable
     * only when called directly or when called by try-with-resources.
     * Do not depend on finalization to invoke {@code close};
     * finalization is not reliable and is deprecated.
     * If cleanup of native resources is needed, other mechanisms such as
     * {@linkplain java.lang.ref.Cleaner} should be used.
     *
     * @throws IOException  if an I/O error occurs.
     */
    public void close() throws IOException {
        if (closed) {
            return;
        }
        synchronized (closeLock) {
            if (closed) {
                return;
            }
            closed = true;
        }

        fd.closeAll(new Closeable() {
            public void close() throws IOException {
               fd.close();
           }
        });
    }

    /**
     * Returns the <code>FileDescriptor</code>
     * object that represents the connection to
     * the actual file in the file system being
     * used by this <code>FileInputStream</code>.
     *
     * @return the file descriptor object associated with this stream.
     * @throws IOException  if an I/O error occurs.
     */
    public final FileDescriptor getFD() throws IOException {
        if (fd != null) {
            return fd;
        }
        throw new IOException();
    }

    private static native void initIDs();

    static {
        initIDs();
    }

    /**
     * Ensures that the {@link #close} method of this file input stream is
     * called when there are no more references to it.
     * The {@link #finalize} method does not call {@link #close} directly.
     *
     * @apiNote
     * To release resources used by this stream {@link #close} should be called
     * directly or by try-with-resources.
     *
     * @implSpec
     * If this FileInputStream has been subclassed and the {@link #close}
     * method has been overridden, the {@link #close} method will be
     * called when the FileInputStream is unreachable.
     * Otherwise, it is implementation specific how the resource cleanup described in
     * {@link #close} is performed.
     *
     * @deprecated The {@code finalize} method has been deprecated and will be removed.
     *     Subclasses that override {@code finalize} in order to perform cleanup
     *     should be modified to use alternative cleanup mechanisms and
     *     to remove the overriding {@code finalize} method.
     *     When overriding the {@code finalize} method, its implementation must explicitly
     *     ensure that {@code super.finalize()} is invoked as described in {@link Object#finalize}.
     *     See the specification for {@link Object#finalize()} for further
     *     information about migration options.
     *
     * @throws IOException  if an I/O error occurs.
     */
    // @Deprecated(since="9", forRemoval = true)
    /* oops! protected */public void finalize() throws IOException {
    }

    /*
     * Returns a finalizer object if the FIS needs a finalizer; otherwise null.
     * If the FIS has a close method; it needs an AltFinalizer.
     */
    private static Object getFinalizer(FileInputStream fis) {
        Class<?> clazz = fis.getClass();
        while (clazz != FileInputStream.class) {
            try {
                clazz.getDeclaredMethod("close");
                return new AltFinalizer(fis);
            } catch (NoSuchMethodException nsme) {
                // ignore
            }
            clazz = clazz.getSuperclass();
        }
        return null;
    }

    /**
     * Class to call {@code FileInputStream.close} when finalized.
     * If finalization of the stream is needed, an instance is created
     * in its constructor(s).  When the set of instances
     * related to the stream is unreachable, the AltFinalizer performs
     * the needed call to the stream's {@code close} method.
     */
    static class AltFinalizer {
        private final FileInputStream fis;

        AltFinalizer(FileInputStream fis) {
            this.fis = fis;
        }

        // @Override
        // @SuppressWarnings("deprecation")
        /* oops! protected */public final void finalize() {
            try {
                if ((fis.fd != null) && (fis.fd != FileDescriptor.in)) {
                    /* if fd is shared, the references in FileDescriptor
                     * will ensure that finalizer is only called when
                     * safe to do so. All references using the fd have
                     * become unreachable. We can call close()
                     */
                    fis.close();
                }
            } catch (IOException ioe) {
                // ignore
            }
        }
    }
}
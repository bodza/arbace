package java.io;

/**
 * A file output stream is an output stream for writing data to a
 * <code>File</code> or to a <code>FileDescriptor</code>. Whether or not
 * a file is available or may be created depends upon the underlying
 * platform.  Some platforms, in particular, allow a file to be opened
 * for writing by only one {@code FileOutputStream} (or other
 * file-writing object) at a time.  In such situations the constructors in
 * this class will fail if the file involved is already open.
 *
 * <code>FileOutputStream</code> is meant for writing streams of raw bytes
 * such as image data. For writing streams of characters, consider using
 * <code>FileWriter</code>.
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
 * If this FileOutputStream has been subclassed and the {@link #close}
 * method has been overridden, the {@link #close} method will be
 * called when the FileInputStream is unreachable.
 * Otherwise, it is implementation specific how the resource cleanup described in
 * {@link #close} is performed.
 */
public class FileOutputStream extends OutputStream {
    /**
     * The system dependent file descriptor.
     */
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
     * Creates a file output stream to write to the specified file
     * descriptor, which represents an existing connection to an actual
     * file in the file system.
     *
     * If <code>fdObj</code> is null then a <code>NullPointerException</code>
     * is thrown.
     *
     * This constructor does not throw an exception if <code>fdObj</code>
     * is {@link java.io.FileDescriptor#valid() invalid}.
     * However, if the methods are invoked on the resulting stream to attempt
     * I/O on the stream, an <code>IOException</code> is thrown.
     *
     * @param fdo   the file descriptor to be opened for writing
     */
    public FileOutputStream(FileDescriptor fdo) {
        if (fdo == null) {
            throw new NullPointerException();
        }
        this.fd = fdo;
        this.path = null;
        this.altFinalizer = null;

        fd.attach(this);
    }

    /**
     * Writes the specified byte to this file output stream.
     *
     * @param b   the byte to be written.
     * @param append   {@code true} if the write operation first
     *     advances the position to the end of file
     */
    private native void write(int b, boolean append) throws IOException;

    /**
     * Writes the specified byte to this file output stream. Implements
     * the <code>write</code> method of <code>OutputStream</code>.
     *
     * @param b   the byte to be written.
     * @throws IOException  if an I/O error occurs.
     */
    public void write(int b) throws IOException {
        write(b, fd.append);
    }

    /**
     * Writes a sub array as a sequence of bytes.
     * @param b the data to be written
     * @param off the start offset in the data
     * @param len the number of bytes that are written
     * @param append {@code true} to first advance the position to the
     *     end of file
     * @throws IOException If an I/O error has occurred.
     */
    private native void writeBytes(byte b[], int off, int len, boolean append) throws IOException;

    /**
     * Writes <code>b.length</code> bytes from the specified byte array
     * to this file output stream.
     *
     * @param b   the data.
     * @throws IOException  if an I/O error occurs.
     */
    public void write(byte b[]) throws IOException {
        writeBytes(b, 0, b.length, fd.append);
    }

    /**
     * Writes <code>len</code> bytes from the specified byte array
     * starting at offset <code>off</code> to this file output stream.
     *
     * @param b     the data.
     * @param off   the start offset in the data.
     * @param len   the number of bytes to write.
     * @throws IOException  if an I/O error occurs.
     */
    public void write(byte b[], int off, int len) throws IOException {
        writeBytes(b, off, len, fd.append);
    }

    /**
     * Closes this file output stream and releases any system resources
     * associated with this stream. This file output stream may no longer
     * be used for writing bytes.
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
     * Returns the file descriptor associated with this stream.
     *
     * @return the <code>FileDescriptor</code> object that represents
     *          the connection to the file in the file system being used
     *          by this <code>FileOutputStream</code> object.
     *
     * @throws IOException  if an I/O error occurs.
     */
     public final FileDescriptor getFD()  throws IOException {
        if (fd != null) {
            return fd;
        }
        throw new IOException();
     }

    /**
     * Cleans up the connection to the file, and ensures that the
     * {@link #close} method of this file output stream is
     * called when there are no more references to this stream.
     * The {@link #finalize} method does not call {@link #close} directly.
     *
     * @apiNote
     * To release resources used by this stream {@link #close} should be called
     * directly or by try-with-resources.
     *
     * @implSpec
     * If this FileOutputStream has been subclassed and the {@link #close}
     * method has been overridden, the {@link #close} method will be
     * called when the FileOutputStream is unreachable.
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

    private static native void initIDs();

    static {
        initIDs();
    }

    /*
     * Returns a finalizer object if the FOS needs a finalizer; otherwise null.
     * If the FOS has a close method; it needs an AltFinalizer.
     */
    private static Object getFinalizer(FileOutputStream fos) {
        Class<?> clazz = fos.getClass();
        while (clazz != FileOutputStream.class) {
            try {
                clazz.getDeclaredMethod("close");
                return new AltFinalizer(fos);
            } catch (NoSuchMethodException nsme) {
                // ignore
            }
            clazz = clazz.getSuperclass();
        }
        return null;
    }

    /**
     * Class to call {@code FileOutputStream.close} when finalized.
     * If finalization of the stream is needed, an instance is created
     * in its constructor(s).  When the set of instances
     * related to the stream is unreachable, the AltFinalizer performs
     * the needed call to the stream's {@code close} method.
     */
    static class AltFinalizer {
        private final FileOutputStream fos;

        AltFinalizer(FileOutputStream fos) {
            this.fos = fos;
        }

        // @Override
        // @SuppressWarnings("deprecation")
        /* oops! protected */public final void finalize() {
            try {
                if (fos.fd != null) {
                    if (fos.fd == FileDescriptor.out || fos.fd == FileDescriptor.err) {
                        // Subclass may override flush; otherwise it is no-op
                        fos.flush();
                    } else {
                        /* if fd is shared, the references in FileDescriptor
                         * will ensure that finalizer is only called when
                         * safe to do so. All references using the fd have
                         * become unreachable. We can call close()
                         */
                        fos.close();
                    }
                }
            } catch (IOException ioe) {
                // ignore
            }
        }
    }
}

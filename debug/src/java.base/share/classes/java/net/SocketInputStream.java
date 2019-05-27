package java.net;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.IOException;

import sun.net.ConnectionResetException;

/**
 * This stream extends FileInputStream to implement a
 * SocketInputStream. Note that this class should <b>NOT</b> be
 * public.
 */
class SocketInputStream extends FileInputStream {
    static {
        init();
    }

    private boolean eof;
    private AbstractPlainSocketImpl impl = null;
    private byte temp[];
    private Socket socket = null;

    /**
     * Creates a new SocketInputStream. Can only be called
     * by a Socket. This method needs to hang on to the owner Socket so
     * that the fd will not be closed.
     * @param impl the implemented socket input stream
     */
    SocketInputStream(AbstractPlainSocketImpl impl) throws IOException {
        super(impl.getFileDescriptor());
        this.impl = impl;
        socket = impl.getSocket();
    }

    /**
     * Reads into an array of bytes at the specified offset using
     * the received socket primitive.
     * @param fd the FileDescriptor
     * @param b the buffer into which the data is read
     * @param off the start offset of the data
     * @param len the maximum number of bytes read
     * @param timeout the read timeout in ms
     * @return the actual number of bytes read, -1 is
     *          returned when the end of the stream is reached.
     * @throws IOException If an I/O error has occurred.
     */
    private native int socketRead0(FileDescriptor fd, byte b[], int off, int len, int timeout) throws IOException;

    // wrap native call to allow instrumentation
    /**
     * Reads into an array of bytes at the specified offset using
     * the received socket primitive.
     * @param fd the FileDescriptor
     * @param b the buffer into which the data is read
     * @param off the start offset of the data
     * @param len the maximum number of bytes read
     * @param timeout the read timeout in ms
     * @return the actual number of bytes read, -1 is
     *          returned when the end of the stream is reached.
     * @throws IOException If an I/O error has occurred.
     */
    private int socketRead(FileDescriptor fd, byte b[], int off, int len, int timeout) throws IOException {
        return socketRead0(fd, b, off, len, timeout);
    }

    /**
     * Reads into a byte array data from the socket.
     * @param b the buffer into which the data is read
     * @return the actual number of bytes read, -1 is
     *          returned when the end of the stream is reached.
     * @throws IOException If an I/O error has occurred.
     */
    public int read(byte b[]) throws IOException {
        return read(b, 0, b.length);
    }

    /**
     * Reads into a byte array <i>b</i> at offset <i>off</i>,
     * <i>length</i> bytes of data.
     * @param b the buffer into which the data is read
     * @param off the start offset of the data
     * @param length the maximum number of bytes read
     * @return the actual number of bytes read, -1 is
     *          returned when the end of the stream is reached.
     * @throws IOException If an I/O error has occurred.
     */
    public int read(byte b[], int off, int length) throws IOException {
        return read(b, off, length, impl.getTimeout());
    }

    int read(byte b[], int off, int length, int timeout) throws IOException {
        int n;

        // EOF already encountered
        if (eof) {
            return -1;
        }

        // connection reset
        if (impl.isConnectionReset()) {
            throw new SocketException("Connection reset");
        }

        // bounds check
        if (length <= 0 || off < 0 || length > b.length - off) {
            if (length == 0) {
                return 0;
            }
            throw new ArrayIndexOutOfBoundsException(String.str("length == ", length, " off == ", off, " buffer length == ", b.length));
        }

        // acquire file descriptor and do the read
        FileDescriptor fd = impl.acquireFD();
        try {
            n = socketRead(fd, b, off, length, timeout);
            if (n > 0) {
                return n;
            }
        } catch (ConnectionResetException rstExc) {
            impl.setConnectionReset();
        } finally {
            impl.releaseFD();
        }

        /*
         * If we get here we are at EOF, the socket has been closed,
         * or the connection has been reset.
         */
        if (impl.isClosedOrPending()) {
            throw new SocketException("Socket closed");
        }
        if (impl.isConnectionReset()) {
            throw new SocketException("Connection reset");
        }
        eof = true;
        return -1;
    }

    /**
     * Reads a single byte from the socket.
     */
    public int read() throws IOException {
        if (eof) {
            return -1;
        }
        temp = new byte[1];
        int n = read(temp, 0, 1);
        if (n <= 0) {
            return -1;
        }
        return temp[0] & 0xff;
    }

    /**
     * Skips n bytes of input.
     * @param numbytes the number of bytes to skip
     * @return the actual number of bytes skipped.
     * @throws IOException If an I/O error has occurred.
     */
    public long skip(long numbytes) throws IOException {
        if (numbytes <= 0) {
            return 0;
        }
        long n = numbytes;
        int buflen = (int) Math.min(1024, n);
        byte data[] = new byte[buflen];
        while (n > 0) {
            int r = read(data, 0, (int) Math.min((long) buflen, n));
            if (r < 0) {
                break;
            }
            n -= r;
        }
        return numbytes - n;
    }

    /**
     * Returns the number of bytes that can be read without blocking.
     * @return the number of immediately available bytes
     */
    public int available() throws IOException {
        return impl.available();
    }

    /**
     * Closes the stream.
     */
    private boolean closing = false;
    public void close() throws IOException {
        // Prevent recursion. See BugId 4484411
        if (closing)
            return;
        closing = true;
        if (socket != null) {
            if (!socket.isClosed())
                socket.close();
        } else
            impl.close();
        closing = false;
    }

    void setEOF(boolean eof) {
        this.eof = eof;
    }

    /**
     * Overrides finalize, the fd is closed by the Socket.
     */
    // @SuppressWarnings({"deprecation", "removal"})
    /* oops! protected */public void finalize() {}

    /**
     * Perform class load-time initializations.
     */
    private static native void init();
}

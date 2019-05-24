package java.io;

/**
 * Abstract class for reading filtered character streams.
 * The abstract class <code>FilterReader</code> itself
 * provides default methods that pass all requests to
 * the contained stream. Subclasses of <code>FilterReader</code>
 * should override some of these methods and may also provide
 * additional methods and fields.
 */
public abstract class FilterReader extends Reader {
    /**
     * The underlying character-input stream.
     */
    protected Reader in;

    /**
     * Creates a new filtered reader.
     *
     * @param in  a Reader object providing the underlying stream.
     * @throws NullPointerException if <code>in</code> is <code>null</code>
     */
    protected FilterReader(Reader in) {
        super(in);
        this.in = in;
    }

    /**
     * Reads a single character.
     *
     * @throws IOException  If an I/O error occurs
     */
    public int read() throws IOException {
        return in.read();
    }

    /**
     * Reads characters into a portion of an array.
     *
     * @throws IOException  If an I/O error occurs
     * @throws IndexOutOfBoundsException {@inheritDoc}
     */
    public int read(char cbuf[], int off, int len) throws IOException {
        return in.read(cbuf, off, len);
    }

    /**
     * Skips characters.
     *
     * @throws IOException  If an I/O error occurs
     */
    public long skip(long n) throws IOException {
        return in.skip(n);
    }

    /**
     * Tells whether this stream is ready to be read.
     *
     * @throws IOException  If an I/O error occurs
     */
    public boolean ready() throws IOException {
        return in.ready();
    }

    /**
     * Tells whether this stream supports the mark() operation.
     */
    public boolean markSupported() {
        return in.markSupported();
    }

    /**
     * Marks the present position in the stream.
     *
     * @throws IOException  If an I/O error occurs
     */
    public void mark(int readAheadLimit) throws IOException {
        in.mark(readAheadLimit);
    }

    /**
     * Resets the stream.
     *
     * @throws IOException  If an I/O error occurs
     */
    public void reset() throws IOException {
        in.reset();
    }

    public void close() throws IOException {
        in.close();
    }
}

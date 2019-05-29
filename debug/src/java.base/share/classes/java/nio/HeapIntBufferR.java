package java.nio;

/**
 * A read-only HeapIntBuffer.  This class extends the corresponding
 * read/write class, overriding the mutation methods to throw a {@link
 * ReadOnlyBufferException} and overriding the view-buffer methods to return an
 * instance of this class rather than of the superclass.
 */
class HeapIntBufferR extends HeapIntBuffer {
    // Cached array base offset
    private static final long ARRAY_BASE_OFFSET = UNSAFE.arrayBaseOffset(int[].class);

    // Cached array base offset
    private static final long ARRAY_INDEX_SCALE = UNSAFE.arrayIndexScale(int[].class);

    HeapIntBufferR(int cap, int lim) {
        super(cap, lim);
        this.isReadOnly = true;
    }

    HeapIntBufferR(int[] buf, int off, int len) {
        super(buf, off, len);
        this.isReadOnly = true;
    }

    protected HeapIntBufferR(int[] buf, int mark, int pos, int lim, int cap, int off) {
        super(buf, mark, pos, lim, cap, off);
        this.isReadOnly = true;
    }

    public IntBuffer slice() {
        return new HeapIntBufferR(hb, -1, 0, this.remaining(), this.remaining(), this.position() + offset);
    }

    public IntBuffer duplicate() {
        return new HeapIntBufferR(hb, this.markValue(), this.position(), this.limit(), this.capacity(), offset);
    }

    public IntBuffer asReadOnlyBuffer() {
        return duplicate();
    }

    public boolean isReadOnly() {
        return true;
    }

    public IntBuffer put(int x) {
        throw new ReadOnlyBufferException();
    }

    public IntBuffer put(int i, int x) {
        throw new ReadOnlyBufferException();
    }

    public IntBuffer put(int[] src, int offset, int length) {
        throw new ReadOnlyBufferException();
    }

    public IntBuffer put(IntBuffer src) {
        throw new ReadOnlyBufferException();
    }

    public IntBuffer compact() {
        throw new ReadOnlyBufferException();
    }

    public ByteOrder order() {
        return ByteOrder.nativeOrder();
    }
}

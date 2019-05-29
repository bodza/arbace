package java.nio;

/**
 * A read-only HeapByteBuffer.  This class extends the corresponding
 * read/write class, overriding the mutation methods to throw a {@link
 * ReadOnlyBufferException} and overriding the view-buffer methods to return an
 * instance of this class rather than of the superclass.
 */
class HeapByteBufferR extends HeapByteBuffer {
    // Cached array base offset
    private static final long ARRAY_BASE_OFFSET = UNSAFE.arrayBaseOffset(byte[].class);

    // Cached array base offset
    private static final long ARRAY_INDEX_SCALE = UNSAFE.arrayIndexScale(byte[].class);

    HeapByteBufferR(int cap, int lim) {
        super(cap, lim);
        this.isReadOnly = true;
    }

    HeapByteBufferR(byte[] buf, int off, int len) {
        super(buf, off, len);
        this.isReadOnly = true;
    }

    protected HeapByteBufferR(byte[] buf, int mark, int pos, int lim, int cap, int off) {
        super(buf, mark, pos, lim, cap, off);
        this.isReadOnly = true;
    }

    public ByteBuffer slice() {
        return new HeapByteBufferR(hb, -1, 0, this.remaining(), this.remaining(), this.position() + offset);
    }

    ByteBuffer slice(int pos, int lim) {
        // assert (pos >= 0);
        // assert (pos <= lim);
        int rem = lim - pos;
        return new HeapByteBufferR(hb, -1, 0, rem, rem, pos + offset);
    }

    public ByteBuffer duplicate() {
        return new HeapByteBufferR(hb, this.markValue(), this.position(), this.limit(), this.capacity(), offset);
    }

    public ByteBuffer asReadOnlyBuffer() {
        return duplicate();
    }

    public boolean isReadOnly() {
        return true;
    }

    public ByteBuffer put(byte x) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer put(int i, byte x) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer put(byte[] src, int offset, int length) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer put(ByteBuffer src) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer compact() {
        throw new ReadOnlyBufferException();
    }

    byte _get(int i) {
        return hb[i];
    }

    void _put(int i, byte b) {
        throw new ReadOnlyBufferException();
    }

    // char

    public ByteBuffer putChar(char x) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer putChar(int i, char x) {
        throw new ReadOnlyBufferException();
    }

    public CharBuffer asCharBuffer() {
        int size = this.remaining() >> 1;
        long addr = address + position();
        return (bigEndian
                ? (CharBuffer)(new ByteBufferAsCharBufferRB(this, -1, 0, size, size, addr))
                : (CharBuffer)(new ByteBufferAsCharBufferRL(this, -1, 0, size, size, addr)));
    }

    // short

    public ByteBuffer putShort(short x) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer putShort(int i, short x) {
        throw new ReadOnlyBufferException();
    }

    public ShortBuffer asShortBuffer() {
        int size = this.remaining() >> 1;
        long addr = address + position();
        return (bigEndian
                ? (ShortBuffer)(new ByteBufferAsShortBufferRB(this, -1, 0, size, size, addr))
                : (ShortBuffer)(new ByteBufferAsShortBufferRL(this, -1, 0, size, size, addr)));
    }

    // int

    public ByteBuffer putInt(int x) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer putInt(int i, int x) {
        throw new ReadOnlyBufferException();
    }

    public IntBuffer asIntBuffer() {
        int size = this.remaining() >> 2;
        long addr = address + position();
        return (bigEndian
                ? (IntBuffer)(new ByteBufferAsIntBufferRB(this, -1, 0, size, size, addr))
                : (IntBuffer)(new ByteBufferAsIntBufferRL(this, -1, 0, size, size, addr)));
    }

    // long

    public ByteBuffer putLong(long x) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer putLong(int i, long x) {
        throw new ReadOnlyBufferException();
    }

    public LongBuffer asLongBuffer() {
        int size = this.remaining() >> 3;
        long addr = address + position();
        return (bigEndian
                ? (LongBuffer)(new ByteBufferAsLongBufferRB(this, -1, 0, size, size, addr))
                : (LongBuffer)(new ByteBufferAsLongBufferRL(this, -1, 0, size, size, addr)));
    }

    // float

    public ByteBuffer putFloat(float x) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer putFloat(int i, float x) {
        throw new ReadOnlyBufferException();
    }

    public FloatBuffer asFloatBuffer() {
        int size = this.remaining() >> 2;
        long addr = address + position();
        return (bigEndian
                ? (FloatBuffer)(new ByteBufferAsFloatBufferRB(this, -1, 0, size, size, addr))
                : (FloatBuffer)(new ByteBufferAsFloatBufferRL(this, -1, 0, size, size, addr)));
    }

    // double

    public ByteBuffer putDouble(double x) {
        throw new ReadOnlyBufferException();
    }

    public ByteBuffer putDouble(int i, double x) {
        throw new ReadOnlyBufferException();
    }

    public DoubleBuffer asDoubleBuffer() {
        int size = this.remaining() >> 3;
        long addr = address + position();
        return (bigEndian
                ? (DoubleBuffer)(new ByteBufferAsDoubleBufferRB(this, -1, 0, size, size, addr))
                : (DoubleBuffer)(new ByteBufferAsDoubleBufferRL(this, -1, 0, size, size, addr)));
    }
}

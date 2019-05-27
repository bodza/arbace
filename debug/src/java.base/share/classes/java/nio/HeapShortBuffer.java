package java.nio;

/**
 * A read/write HeapShortBuffer.
 */
class HeapShortBuffer extends ShortBuffer {
    // Cached array base offset
    private static final long ARRAY_BASE_OFFSET = UNSAFE.arrayBaseOffset(short[].class);

    // Cached array base offset
    private static final long ARRAY_INDEX_SCALE = UNSAFE.arrayIndexScale(short[].class);

    HeapShortBuffer(int cap, int lim) {
        super(-1, 0, lim, cap, new short[cap], 0);
        this.address = ARRAY_BASE_OFFSET;
    }

    HeapShortBuffer(short[] buf, int off, int len) {
        super(-1, off, off + len, buf.length, buf, 0);
        this.address = ARRAY_BASE_OFFSET;
    }

    protected HeapShortBuffer(short[] buf, int mark, int pos, int lim, int cap, int off) {
        super(mark, pos, lim, cap, buf, off);
        this.address = ARRAY_BASE_OFFSET + off * ARRAY_INDEX_SCALE;
    }

    public ShortBuffer slice() {
        return new HeapShortBuffer(hb,
                                        -1,
                                        0,
                                        this.remaining(),
                                        this.remaining(),
                                        this.position() + offset);
    }

    public ShortBuffer duplicate() {
        return new HeapShortBuffer(hb,
                                        this.markValue(),
                                        this.position(),
                                        this.limit(),
                                        this.capacity(),
                                        offset);
    }

    public ShortBuffer asReadOnlyBuffer() {
        return new HeapShortBufferR(hb,
                                     this.markValue(),
                                     this.position(),
                                     this.limit(),
                                     this.capacity(),
                                     offset);
    }

    protected int ix(int i) {
        return i + offset;
    }

    public short get() {
        return hb[ix(nextGetIndex())];
    }

    public short get(int i) {
        return hb[ix(checkIndex(i))];
    }

    public ShortBuffer get(short[] dst, int offset, int length) {
        // oops! checkBounds(offset, length, dst.length);
        if (length > remaining())
            throw new BufferUnderflowException();
        System.arraycopy(hb, ix(position()), dst, offset, length);
        position(position() + length);
        return this;
    }

    public boolean isDirect() {
        return false;
    }

    public boolean isReadOnly() {
        return false;
    }

    public ShortBuffer put(short x) {
        hb[ix(nextPutIndex())] = x;
        return this;
    }

    public ShortBuffer put(int i, short x) {
        hb[ix(checkIndex(i))] = x;
        return this;
    }

    public ShortBuffer put(short[] src, int offset, int length) {
        // oops! checkBounds(offset, length, src.length);
        if (length > remaining())
            throw new BufferOverflowException();
        System.arraycopy(src, offset, hb, ix(position()), length);
        position(position() + length);
        return this;
    }

    public ShortBuffer put(ShortBuffer src) {
        if (src instanceof HeapShortBuffer) {
            if (src == this)
                throw createSameBufferException();
            HeapShortBuffer sb = (HeapShortBuffer)src;
            int n = sb.remaining();
            if (n > remaining())
                throw new BufferOverflowException();
            System.arraycopy(sb.hb, sb.ix(sb.position()),
                             hb, ix(position()), n);
            sb.position(sb.position() + n);
            position(position() + n);
        } else if (src.isDirect()) {
            int n = src.remaining();
            if (n > remaining())
                throw new BufferOverflowException();
            src.get(hb, ix(position()), n);
            position(position() + n);
        } else {
            super.put(src);
        }
        return this;
    }

    public ShortBuffer compact() {
        System.arraycopy(hb, ix(position()), hb, ix(0), remaining());
        position(remaining());
        limit(capacity());
        discardMark();
        return this;
    }

    public ByteOrder order() {
        return ByteOrder.nativeOrder();
    }
}

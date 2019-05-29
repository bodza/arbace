package java.nio;

/**
 * A read/write HeapLongBuffer.
 */
class HeapLongBuffer extends LongBuffer {
    // Cached array base offset
    private static final long ARRAY_BASE_OFFSET = UNSAFE.arrayBaseOffset(long[].class);

    // Cached array base offset
    private static final long ARRAY_INDEX_SCALE = UNSAFE.arrayIndexScale(long[].class);

    HeapLongBuffer(int cap, int lim) {
        super(-1, 0, lim, cap, new long[cap], 0);
        this.address = ARRAY_BASE_OFFSET;
    }

    HeapLongBuffer(long[] buf, int off, int len) {
        super(-1, off, off + len, buf.length, buf, 0);
        this.address = ARRAY_BASE_OFFSET;
    }

    protected HeapLongBuffer(long[] buf, int mark, int pos, int lim, int cap, int off) {
        super(mark, pos, lim, cap, buf, off);
        this.address = ARRAY_BASE_OFFSET + off * ARRAY_INDEX_SCALE;
    }

    public LongBuffer slice() {
        return new HeapLongBuffer(hb, -1, 0, this.remaining(), this.remaining(), this.position() + offset);
    }

    public LongBuffer duplicate() {
        return new HeapLongBuffer(hb, this.markValue(), this.position(), this.limit(), this.capacity(), offset);
    }

    public LongBuffer asReadOnlyBuffer() {
        return new HeapLongBufferR(hb, this.markValue(), this.position(), this.limit(), this.capacity(), offset);
    }

    protected int ix(int i) {
        return i + offset;
    }

    public long get() {
        return hb[ix(nextGetIndex())];
    }

    public long get(int i) {
        return hb[ix(checkIndex(i))];
    }

    public LongBuffer get(long[] dst, int offset, int length) {
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

    public LongBuffer put(long x) {
        hb[ix(nextPutIndex())] = x;
        return this;
    }

    public LongBuffer put(int i, long x) {
        hb[ix(checkIndex(i))] = x;
        return this;
    }

    public LongBuffer put(long[] src, int offset, int length) {
        // oops! checkBounds(offset, length, src.length);
        if (length > remaining())
            throw new BufferOverflowException();
        System.arraycopy(src, offset, hb, ix(position()), length);
        position(position() + length);
        return this;
    }

    public LongBuffer put(LongBuffer src) {
        if (src instanceof HeapLongBuffer) {
            if (src == this)
                throw createSameBufferException();
            HeapLongBuffer sb = (HeapLongBuffer)src;
            int n = sb.remaining();
            if (n > remaining())
                throw new BufferOverflowException();
            System.arraycopy(sb.hb, sb.ix(sb.position()), hb, ix(position()), n);
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

    public LongBuffer compact() {
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

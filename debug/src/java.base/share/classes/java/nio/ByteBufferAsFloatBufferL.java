package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsFloatBufferL extends FloatBuffer {
    protected final ByteBuffer bb;

    ByteBufferAsFloatBufferL(ByteBuffer bb) {
        super(-1, 0,
              bb.remaining() >> 2,
              bb.remaining() >> 2);
        this.bb = bb;
        // enforce limit == capacity
        int cap = this.capacity();
        this.limit(cap);
        int pos = this.position();
        assert (pos <= cap);
        address = bb.address;
    }

    ByteBufferAsFloatBufferL(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
        super(mark, pos, lim, cap);
        this.bb = bb;
        address = addr;
        assert address >= bb.address;
    }

    @Override
    Object base() {
        return bb.hb;
    }

    public FloatBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        long addr = byteOffset(pos);
        return new ByteBufferAsFloatBufferL(bb, -1, 0, rem, rem, addr);
    }

    public FloatBuffer duplicate() {
        return new ByteBufferAsFloatBufferL(bb,
                                                    this.markValue(),
                                                    this.position(),
                                                    this.limit(),
                                                    this.capacity(),
                                                    address);
    }

    public FloatBuffer asReadOnlyBuffer() {
        return new ByteBufferAsFloatBufferRL(bb,
                                                 this.markValue(),
                                                 this.position(),
                                                 this.limit(),
                                                 this.capacity(),
                                                 address);
    }

    private int ix(int i) {
        int off = (int) (address - bb.address);
        return (i << 2) + off;
    }

    protected long byteOffset(long i) {
        return (i << 2) + address;
    }

    public float get() {
        int x = UNSAFE.getIntUnaligned(bb.hb, byteOffset(nextGetIndex()),
            false);
        return Float.intBitsToFloat(x);
    }

    public float get(int i) {
        int x = UNSAFE.getIntUnaligned(bb.hb, byteOffset(checkIndex(i)),
            false);
        return Float.intBitsToFloat(x);
    }

    public FloatBuffer put(float x) {
        int y = Float.floatToRawIntBits(x);
        UNSAFE.putIntUnaligned(bb.hb, byteOffset(nextPutIndex()), y,
            false);
        return this;
    }

    public FloatBuffer put(int i, float x) {
        int y = Float.floatToRawIntBits(x);
        UNSAFE.putIntUnaligned(bb.hb, byteOffset(checkIndex(i)), y,
            false);
        return this;
    }

    public FloatBuffer compact() {
        int pos = position();
        int lim = limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);

        ByteBuffer db = bb.duplicate();
        db.limit(ix(lim));
        db.position(ix(0));
        ByteBuffer sb = db.slice();
        sb.position(pos << 2);
        sb.compact();
        position(rem);
        limit(capacity());
        discardMark();
        return this;
    }

    public boolean isDirect() {
        return bb.isDirect();
    }

    public boolean isReadOnly() {
        return false;
    }

    public ByteOrder order() {
        return ByteOrder.LITTLE_ENDIAN;
    }
}

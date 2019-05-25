package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsShortBufferL extends ShortBuffer {
    protected final ByteBuffer bb;

    ByteBufferAsShortBufferL(ByteBuffer bb) {
        super(-1, 0,
              bb.remaining() >> 1,
              bb.remaining() >> 1);
        this.bb = bb;
        // enforce limit == capacity
        int cap = this.capacity();
        this.limit(cap);
        int pos = this.position();
        assert (pos <= cap);
        address = bb.address;
    }

    ByteBufferAsShortBufferL(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
        super(mark, pos, lim, cap);
        this.bb = bb;
        address = addr;
        assert address >= bb.address;
    }

    @Override
    Object base() {
        return bb.hb;
    }

    public ShortBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        long addr = byteOffset(pos);
        return new ByteBufferAsShortBufferL(bb, -1, 0, rem, rem, addr);
    }

    public ShortBuffer duplicate() {
        return new ByteBufferAsShortBufferL(bb,
                                                    this.markValue(),
                                                    this.position(),
                                                    this.limit(),
                                                    this.capacity(),
                                                    address);
    }

    public ShortBuffer asReadOnlyBuffer() {
        return new ByteBufferAsShortBufferRL(bb,
                                                 this.markValue(),
                                                 this.position(),
                                                 this.limit(),
                                                 this.capacity(),
                                                 address);
    }

    private int ix(int i) {
        int off = (int) (address - bb.address);
        return (i << 1) + off;
    }

    protected long byteOffset(long i) {
        return (i << 1) + address;
    }

    public short get() {
        short x = UNSAFE.getShortUnaligned(bb.hb, byteOffset(nextGetIndex()),
            false);
        return (x);
    }

    public short get(int i) {
        short x = UNSAFE.getShortUnaligned(bb.hb, byteOffset(checkIndex(i)),
            false);
        return (x);
    }

    public ShortBuffer put(short x) {
        short y = (x);
        UNSAFE.putShortUnaligned(bb.hb, byteOffset(nextPutIndex()), y,
            false);
        return this;
    }

    public ShortBuffer put(int i, short x) {
        short y = (x);
        UNSAFE.putShortUnaligned(bb.hb, byteOffset(checkIndex(i)), y,
            false);
        return this;
    }

    public ShortBuffer compact() {
        int pos = position();
        int lim = limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);

        ByteBuffer db = bb.duplicate();
        db.limit(ix(lim));
        db.position(ix(0));
        ByteBuffer sb = db.slice();
        sb.position(pos << 1);
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

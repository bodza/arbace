package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsLongBufferRB extends ByteBufferAsLongBufferB {
    ByteBufferAsLongBufferRB(ByteBuffer bb) {
        super(bb);
    }

    ByteBufferAsLongBufferRB(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
        super(bb, mark, pos, lim, cap, addr);
    }

    // @Override
    Object base() {
        return bb.hb;
    }

    public LongBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        // assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        long addr = byteOffset(pos);
        return new ByteBufferAsLongBufferRB(bb, -1, 0, rem, rem, addr);
    }

    public LongBuffer duplicate() {
        return new ByteBufferAsLongBufferRB(bb, this.markValue(), this.position(), this.limit(), this.capacity(), address);
    }

    public LongBuffer asReadOnlyBuffer() {
        return duplicate();
    }

    public LongBuffer put(long x) {
        throw new ReadOnlyBufferException();
    }

    public LongBuffer put(int i, long x) {
        throw new ReadOnlyBufferException();
    }

    public LongBuffer compact() {
        throw new ReadOnlyBufferException();
    }

    public boolean isDirect() {
        return bb.isDirect();
    }

    public boolean isReadOnly() {
        return true;
    }

    public ByteOrder order() {
        return ByteOrder.BIG_ENDIAN;
    }
}

package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsShortBufferRL extends ByteBufferAsShortBufferL {
    ByteBufferAsShortBufferRL(ByteBuffer bb) {
        super(bb);
    }

    ByteBufferAsShortBufferRL(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
        super(bb, mark, pos, lim, cap, addr);
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
        return new ByteBufferAsShortBufferRL(bb, -1, 0, rem, rem, addr);
    }

    public ShortBuffer duplicate() {
        return new ByteBufferAsShortBufferRL(bb,
                                                    this.markValue(),
                                                    this.position(),
                                                    this.limit(),
                                                    this.capacity(),
                                                    address);
    }

    public ShortBuffer asReadOnlyBuffer() {
        return duplicate();
    }

    public ShortBuffer put(short x) {
        throw new ReadOnlyBufferException();
    }

    public ShortBuffer put(int i, short x) {
        throw new ReadOnlyBufferException();
    }

    public ShortBuffer compact() {
        throw new ReadOnlyBufferException();
    }

    public boolean isDirect() {
        return bb.isDirect();
    }

    public boolean isReadOnly() {
        return true;
    }

    public ByteOrder order() {
        return ByteOrder.LITTLE_ENDIAN;
    }
}

package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsDoubleBufferRB extends ByteBufferAsDoubleBufferB {
    ByteBufferAsDoubleBufferRB(ByteBuffer bb) {
        super(bb);
    }

    ByteBufferAsDoubleBufferRB(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
        super(bb, mark, pos, lim, cap, addr);
    }

    @Override
    Object base() {
        return bb.hb;
    }

    public DoubleBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        long addr = byteOffset(pos);
        return new ByteBufferAsDoubleBufferRB(bb, -1, 0, rem, rem, addr);
    }

    public DoubleBuffer duplicate() {
        return new ByteBufferAsDoubleBufferRB(bb,
                                                    this.markValue(),
                                                    this.position(),
                                                    this.limit(),
                                                    this.capacity(),
                                                    address);
    }

    public DoubleBuffer asReadOnlyBuffer() {
        return duplicate();
    }

    public DoubleBuffer put(double x) {
        throw new ReadOnlyBufferException();
    }

    public DoubleBuffer put(int i, double x) {
        throw new ReadOnlyBufferException();
    }

    public DoubleBuffer compact() {
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

package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsDoubleBufferRL extends ByteBufferAsDoubleBufferL {
    ByteBufferAsDoubleBufferRL(ByteBuffer bb) {
        super(bb);
    }

    ByteBufferAsDoubleBufferRL(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
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
        return new ByteBufferAsDoubleBufferRL(bb, -1, 0, rem, rem, addr);
    }

    public DoubleBuffer duplicate() {
        return new ByteBufferAsDoubleBufferRL(bb,
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
        return ByteOrder.LITTLE_ENDIAN;
    }
}

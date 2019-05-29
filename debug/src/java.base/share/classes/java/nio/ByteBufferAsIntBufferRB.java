package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsIntBufferRB extends ByteBufferAsIntBufferB {
    ByteBufferAsIntBufferRB(ByteBuffer bb) {
        super(bb);
    }

    ByteBufferAsIntBufferRB(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
        super(bb, mark, pos, lim, cap, addr);
    }

    // @Override
    Object base() {
        return bb.hb;
    }

    public IntBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        // assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        long addr = byteOffset(pos);
        return new ByteBufferAsIntBufferRB(bb, -1, 0, rem, rem, addr);
    }

    public IntBuffer duplicate() {
        return new ByteBufferAsIntBufferRB(bb, this.markValue(), this.position(), this.limit(), this.capacity(), address);
    }

    public IntBuffer asReadOnlyBuffer() {
        return duplicate();
    }

    public IntBuffer put(int x) {
        throw new ReadOnlyBufferException();
    }

    public IntBuffer put(int i, int x) {
        throw new ReadOnlyBufferException();
    }

    public IntBuffer compact() {
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

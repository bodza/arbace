package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsFloatBufferRB extends ByteBufferAsFloatBufferB {
    ByteBufferAsFloatBufferRB(ByteBuffer bb) {
        super(bb);
    }

    ByteBufferAsFloatBufferRB(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
        super(bb, mark, pos, lim, cap, addr);
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
        return new ByteBufferAsFloatBufferRB(bb, -1, 0, rem, rem, addr);
    }

    public FloatBuffer duplicate() {
        return new ByteBufferAsFloatBufferRB(bb,
                                                    this.markValue(),
                                                    this.position(),
                                                    this.limit(),
                                                    this.capacity(),
                                                    address);
    }

    public FloatBuffer asReadOnlyBuffer() {
        return duplicate();
    }

    public FloatBuffer put(float x) {
        throw new ReadOnlyBufferException();
    }

    public FloatBuffer put(int i, float x) {
        throw new ReadOnlyBufferException();
    }

    public FloatBuffer compact() {
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

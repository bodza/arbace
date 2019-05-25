package java.nio;

import jdk.internal.misc.Unsafe;

class ByteBufferAsCharBufferRB extends ByteBufferAsCharBufferB {
    ByteBufferAsCharBufferRB(ByteBuffer bb) {
        super(bb);
    }

    ByteBufferAsCharBufferRB(ByteBuffer bb, int mark, int pos, int lim, int cap, long addr) {
        super(bb, mark, pos, lim, cap, addr);
    }

    @Override
    Object base() {
        return bb.hb;
    }

    public CharBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        long addr = byteOffset(pos);
        return new ByteBufferAsCharBufferRB(bb, -1, 0, rem, rem, addr);
    }

    public CharBuffer duplicate() {
        return new ByteBufferAsCharBufferRB(bb,
                                                    this.markValue(),
                                                    this.position(),
                                                    this.limit(),
                                                    this.capacity(),
                                                    address);
    }

    public CharBuffer asReadOnlyBuffer() {
        return duplicate();
    }

    public CharBuffer put(char x) {
        throw new ReadOnlyBufferException();
    }

    public CharBuffer put(int i, char x) {
        throw new ReadOnlyBufferException();
    }

    public CharBuffer compact() {
        throw new ReadOnlyBufferException();
    }

    public boolean isDirect() {
        return bb.isDirect();
    }

    public boolean isReadOnly() {
        return true;
    }

    public String toString(int start, int end) {
        if ((end > limit()) || (start > end))
            throw new IndexOutOfBoundsException();
        try {
            int len = end - start;
            char[] ca = new char[len];
            CharBuffer cb = CharBuffer.wrap(ca);
            CharBuffer db = this.duplicate();
            db.position(start);
            db.limit(end);
            cb.put(db);
            return new String(ca);
        } catch (StringIndexOutOfBoundsException x) {
            throw new IndexOutOfBoundsException();
        }
    }

    // --- Methods to support CharSequence ---

    public CharBuffer subSequence(int start, int end) {
        int pos = position();
        int lim = limit();
        assert (pos <= lim);
        pos = (pos <= lim ? pos : lim);
        int len = lim - pos;

        if ((start < 0) || (end > len) || (start > end))
            throw new IndexOutOfBoundsException();
        return new ByteBufferAsCharBufferRB(bb,
                                                  -1,
                                                  pos + start,
                                                  pos + end,
                                                  capacity(),
                                                  address);
    }

    public ByteOrder order() {
        return ByteOrder.BIG_ENDIAN;
    }

    ByteOrder charRegionOrder() {
        return order();
    }
}

package java.nio;

import java.io.FileDescriptor;
import java.lang.ref.Reference;
import jdk.internal.misc.VM;
import jdk.internal.ref.Cleaner;
import sun.nio.ch.DirectBuffer;

class DirectCharBufferRS extends DirectCharBufferS implements DirectBuffer {
    // For duplicates and slices
    //
    DirectCharBufferRS(DirectBuffer db, int mark, int pos, int lim, int cap, int off) {
        super(db, mark, pos, lim, cap, off);
        this.isReadOnly = true;
    }

    @Override
    Object base() {
        return null;
    }

    public CharBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        int off = (pos << 1);
        assert (off >= 0);
        return new DirectCharBufferRS(this, -1, 0, rem, rem, off);
    }

    public CharBuffer duplicate() {
        return new DirectCharBufferRS(this,
                                              this.markValue(),
                                              this.position(),
                                              this.limit(),
                                              this.capacity(),
                                              0);
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

    public CharBuffer put(CharBuffer src) {
        throw new ReadOnlyBufferException();
    }

    public CharBuffer put(char[] src, int offset, int length) {
        throw new ReadOnlyBufferException();
    }

    public CharBuffer compact() {
        throw new ReadOnlyBufferException();
    }

    public boolean isDirect() {
        return true;
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
        return new DirectCharBufferRS(this,
                                            -1,
                                            pos + start,
                                            pos + end,
                                            capacity(),
                                            offset);
    }

    public ByteOrder order() {
        return ((ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN)
                ? ByteOrder.LITTLE_ENDIAN : ByteOrder.BIG_ENDIAN);
    }

    ByteOrder charRegionOrder() {
        return order();
    }
}

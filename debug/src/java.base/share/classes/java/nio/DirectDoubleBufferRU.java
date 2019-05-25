package java.nio;

import java.io.FileDescriptor;
import java.lang.ref.Reference;
import jdk.internal.misc.VM;
import jdk.internal.ref.Cleaner;
import sun.nio.ch.DirectBuffer;

class DirectDoubleBufferRU extends DirectDoubleBufferU implements DirectBuffer {
    // For duplicates and slices
    //
    DirectDoubleBufferRU(DirectBuffer db, int mark, int pos, int lim, int cap, int off) {
        super(db, mark, pos, lim, cap, off);
        this.isReadOnly = true;
    }

    @Override
    Object base() {
        return null;
    }

    public DoubleBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        int off = (pos << 3);
        assert (off >= 0);
        return new DirectDoubleBufferRU(this, -1, 0, rem, rem, off);
    }

    public DoubleBuffer duplicate() {
        return new DirectDoubleBufferRU(this,
                                              this.markValue(),
                                              this.position(),
                                              this.limit(),
                                              this.capacity(),
                                              0);
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

    public DoubleBuffer put(DoubleBuffer src) {
        throw new ReadOnlyBufferException();
    }

    public DoubleBuffer put(double[] src, int offset, int length) {
        throw new ReadOnlyBufferException();
    }

    public DoubleBuffer compact() {
        throw new ReadOnlyBufferException();
    }

    public boolean isDirect() {
        return true;
    }

    public boolean isReadOnly() {
        return true;
    }

    public ByteOrder order() {
        return ((ByteOrder.nativeOrder() != ByteOrder.BIG_ENDIAN)
                ? ByteOrder.LITTLE_ENDIAN : ByteOrder.BIG_ENDIAN);
    }
}

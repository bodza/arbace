package java.nio;

import java.io.FileDescriptor;
import java.lang.ref.Reference;
import jdk.internal.misc.VM;
import jdk.internal.ref.Cleaner;
import sun.nio.ch.DirectBuffer;

class DirectFloatBufferRS extends DirectFloatBufferS implements DirectBuffer {
    // For duplicates and slices
    //
    DirectFloatBufferRS(DirectBuffer db, int mark, int pos, int lim, int cap, int off) {
        super(db, mark, pos, lim, cap, off);
        this.isReadOnly = true;
    }

    @Override
    Object base() {
        return null;
    }

    public FloatBuffer slice() {
        int pos = this.position();
        int lim = this.limit();
        assert (pos <= lim);
        int rem = (pos <= lim ? lim - pos : 0);
        int off = (pos << 2);
        assert (off >= 0);
        return new DirectFloatBufferRS(this, -1, 0, rem, rem, off);
    }

    public FloatBuffer duplicate() {
        return new DirectFloatBufferRS(this,
                                              this.markValue(),
                                              this.position(),
                                              this.limit(),
                                              this.capacity(),
                                              0);
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

    public FloatBuffer put(FloatBuffer src) {
        throw new ReadOnlyBufferException();
    }

    public FloatBuffer put(float[] src, int offset, int length) {
        throw new ReadOnlyBufferException();
    }

    public FloatBuffer compact() {
        throw new ReadOnlyBufferException();
    }

    public boolean isDirect() {
        return true;
    }

    public boolean isReadOnly() {
        return true;
    }

    public ByteOrder order() {
        return ((ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN)
                ? ByteOrder.LITTLE_ENDIAN : ByteOrder.BIG_ENDIAN);
    }
}

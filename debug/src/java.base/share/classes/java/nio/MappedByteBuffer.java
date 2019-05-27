package java.nio;

import java.io.FileDescriptor;
import java.lang.ref.Reference;
import jdk.internal.misc.Unsafe;

/**
 * A direct byte buffer whose content is a memory-mapped region of a file.
 *
 * This class extends the {@link ByteBuffer} class with operations that are
 * specific to memory-mapped file regions.
 *
 * A mapped byte buffer and the file mapping that it represents remain
 * valid until the buffer itself is garbage-collected.
 *
 * The content of a mapped byte buffer can change at any time, for example
 * if the content of the corresponding region of the mapped file is changed by
 * this program or another.  Whether or not such changes occur, and when they
 * occur, is operating-system dependent and therefore unspecified.
 *
 * <a id="inaccess"></a>All or part of a mapped byte buffer may become
 * inaccessible at any time, for example if the mapped file is truncated.  An
 * attempt to access an inaccessible region of a mapped byte buffer will not
 * change the buffer's content and will cause an unspecified exception to be
 * thrown either at the time of the access or at some later time.  It is
 * therefore strongly recommended that appropriate precautions be taken to
 * avoid the manipulation of a mapped file by this program, or by a
 * concurrently running program, except to read or write the file's content.
 *
 * Mapped byte buffers otherwise behave no differently than ordinary direct
 * byte buffers.
 */
public abstract class MappedByteBuffer extends ByteBuffer {
    // For mapped buffers, a FileDescriptor that may be used for mapping
    // operations if valid; null if the buffer is not mapped.
    private final FileDescriptor fd;

    // This should only be invoked by the DirectByteBuffer constructors
    MappedByteBuffer(int mark, int pos, int lim, int cap, FileDescriptor fd) {
        super(mark, pos, lim, cap);
        this.fd = fd;
    }

    MappedByteBuffer(int mark, int pos, int lim, int cap) {
        super(mark, pos, lim, cap);
        this.fd = null;
    }

    // Returns the distance (in bytes) of the buffer from the page aligned address
    // of the mapping. Computed each time to avoid storing in every direct buffer.
    private long mappingOffset() {
        int ps = Bits.pageSize();
        long offset = address % ps;
        return (offset >= 0) ? offset : (ps + offset);
    }

    private long mappingAddress(long mappingOffset) {
        return address - mappingOffset;
    }

    private long mappingLength(long mappingOffset) {
        return (long)capacity() + mappingOffset;
    }

    /**
     * Tells whether or not this buffer's content is resident in physical
     * memory.
     *
     * A return value of {@code true} implies that it is highly likely
     * that all of the data in this buffer is resident in physical memory and
     * may therefore be accessed without incurring any virtual-memory page
     * faults or I/O operations.  A return value of {@code false} does not
     * necessarily imply that the buffer's content is not resident in physical
     * memory.
     *
     * The returned value is a hint, rather than a guarantee, because the
     * underlying operating system may have paged out some of the buffer's data
     * by the time that an invocation of this method returns.
     *
     * @return  {@code true} if it is likely that this buffer's content
     *          is resident in physical memory
     */
    public final boolean isLoaded() {
        if (fd == null) {
            return true;
        }
        if ((address == 0) || (capacity() == 0))
            return true;
        long offset = mappingOffset();
        long length = mappingLength(offset);
        return isLoaded0(mappingAddress(offset), length, Bits.pageCount(length));
    }

    // not used, but a potential target for a store, see load() for details.
    private static byte unused;

    /**
     * Loads this buffer's content into physical memory.
     *
     * This method makes a best effort to ensure that, when it returns,
     * this buffer's content is resident in physical memory.  Invoking this
     * method may cause some number of page faults and I/O operations to
     * occur.
     *
     * @return  This buffer
     */
    public final MappedByteBuffer load() {
        if (fd == null) {
            return this;
        }
        if ((address == 0) || (capacity() == 0))
            return this;
        long offset = mappingOffset();
        long length = mappingLength(offset);
        load0(mappingAddress(offset), length);

        // Read a byte from each page to bring it into memory. A checksum
        // is computed as we go along to prevent the compiler from otherwise
        // considering the loop as dead code.
        Unsafe unsafe = Unsafe.getUnsafe();
        int ps = Bits.pageSize();
        int count = Bits.pageCount(length);
        long a = mappingAddress(offset);
        byte x = 0;
        try {
            for (int i=0; i<count; i++) {
                // TODO consider changing to getByteOpaque thus avoiding
                // dead code elimination and the need to calculate a checksum
                x ^= unsafe.getByte(a);
                a += ps;
            }
        } finally {
            Reference.reachabilityFence(this);
        }
        if (unused != 0)
            unused = x;

        return this;
    }

    /**
     * Forces any changes made to this buffer's content to be written to the
     * storage device containing the mapped file.
     *
     * If the file mapped into this buffer resides on a local storage
     * device then when this method returns it is guaranteed that all changes
     * made to the buffer since it was created, or since this method was last
     * invoked, will have been written to that device.
     *
     * If the file does not reside on a local device then no such guarantee
     * is made.
     *
     * @return  This buffer
     */
    public final MappedByteBuffer force() {
        if (fd == null) {
            return this;
        }
        if ((address != 0) && (capacity() != 0)) {
            long offset = mappingOffset();
            force0(fd, mappingAddress(offset), mappingLength(offset));
        }
        return this;
    }

    private native boolean isLoaded0(long address, long length, int pageCount);
    private native void load0(long address, long length);
    private native void force0(FileDescriptor fd, long address, long length);

    // -- Covariant return type overrides

    /**
     * {@inheritDoc}
     */
    // @Override
    public final MappedByteBuffer position(int newPosition) {
        super.position(newPosition);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public final MappedByteBuffer limit(int newLimit) {
        super.limit(newLimit);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public final MappedByteBuffer mark() {
        super.mark();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public final MappedByteBuffer reset() {
        super.reset();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public final MappedByteBuffer clear() {
        super.clear();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public final MappedByteBuffer flip() {
        super.flip();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public final MappedByteBuffer rewind() {
        super.rewind();
        return this;
    }
}
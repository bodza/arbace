package java.nio;

import jdk.internal.util.ArraysSupport;

/**
 * A byte buffer.
 *
 * This class defines six categories of operations upon
 * byte buffers:
 *
 * <ul>
 *
 *   <li>Absolute and relative {@link #get() <i>get</i>} and
 *   {@link #put(byte) <i>put</i>} methods that read and write
 *   single bytes;</li>
 *
 *   <li>Relative {@link #get(byte[]) <i>bulk get</i>}
 *   methods that transfer contiguous sequences of bytes from this buffer
 *   into an array;</li>
 *
 *   <li>Relative {@link #put(byte[]) <i>bulk put</i>}
 *   methods that transfer contiguous sequences of bytes from a
 *   byte array or some other byte
 *   buffer into this buffer;</li>
 *
 *   <li>Absolute and relative {@link #getChar() <i>get</i>}
 *   and {@link #putChar(char) <i>put</i>} methods that read and
 *   write values of other primitive types, translating them to and from
 *   sequences of bytes in a particular byte order;</li>
 *
 *   <li>Methods for creating <i><a href="#views">view buffers</a></i>,
 *   which allow a byte buffer to be viewed as a buffer containing values of
 *   some other primitive type; and</li>
 *
 *   <li>A method for {@link #compact compacting}
 *   a byte buffer.</li>
 *
 * </ul>
 *
 * Byte buffers can be created either by {@link #allocate
 * <i>allocation</i>}, which allocates space for the buffer's
 *
 * content, or by {@link #wrap(byte[]) <i>wrapping</i>} an
 * existing byte array  into a buffer.
 *
 * <a id="direct"></a>
 * <h2> Direct <i>vs.</i> non-direct buffers </h2>
 *
 * A byte buffer is either <i>direct</i> or <i>non-direct</i>.  Given a
 * direct byte buffer, the Java virtual machine will make a best effort to
 * perform native I/O operations directly upon it.  That is, it will attempt to
 * avoid copying the buffer's content to (or from) an intermediate buffer
 * before (or after) each invocation of one of the underlying operating
 * system's native I/O operations.
 *
 * <a id="bin"></a>
 * <h2> Access to binary data </h2>
 *
 * This class defines methods for reading and writing values of all other
 * primitive types, except {@code boolean}.  Primitive values are translated
 * to (or from) sequences of bytes according to the buffer's current byte
 * order, which may be retrieved and modified via the {@link #order order}
 * methods.  Specific byte orders are represented by instances of the {@link
 * ByteOrder} class.  The initial order of a byte buffer is always {@link
 * ByteOrder#BIG_ENDIAN BIG_ENDIAN}.
 *
 * For access to heterogeneous binary data, that is, sequences of values of
 * different types, this class defines a family of absolute and relative
 * <i>get</i> and <i>put</i> methods for each type.  For 32-bit floating-point
 * values, for example, this class defines:
 *
 * <blockquote><pre>
 * float  {@link #getFloat()}
 * float  {@link #getFloat(int) getFloat(int index)}
 *  void  {@link #putFloat(float) putFloat(float f)}
 *  void  {@link #putFloat(int,float) putFloat(int index, float f)}</pre></blockquote>
 *
 * Corresponding methods are defined for the types {@code char,
 * short, int, long}, and {@code double}.  The index
 * parameters of the absolute <i>get</i> and <i>put</i> methods are in terms of
 * bytes rather than of the type being read or written.
 *
 * <a id="views"></a>
 *
 * For access to homogeneous binary data, that is, sequences of values of
 * the same type, this class defines methods that can create <i>views</i> of a
 * given byte buffer.  A <i>view buffer</i> is simply another buffer whose
 * content is backed by the byte buffer.  Changes to the byte buffer's content
 * will be visible in the view buffer, and vice versa; the two buffers'
 * position, limit, and mark values are independent.  The {@link
 * #asFloatBuffer() asFloatBuffer} method, for example, creates an instance of
 * the {@link FloatBuffer} class that is backed by the byte buffer upon which
 * the method is invoked.  Corresponding view-creation methods are defined for
 * the types {@code char, short, int, long}, and {@code double}.
 *
 * View buffers have three important advantages over the families of
 * type-specific <i>get</i> and <i>put</i> methods described above:
 *
 * <ul>
 *
 *   <li>A view buffer is indexed not in terms of bytes but rather in terms
 *   of the type-specific size of its values;</li>
 *
 *   <li>A view buffer provides relative bulk <i>get</i> and <i>put</i>
 *   methods that can transfer contiguous sequences of values between a buffer
 *   and an array or some other buffer of the same type; and</li>
 *
 *   <li>A view buffer is potentially much more efficient because it will
 *   be direct if, and only if, its backing byte buffer is direct.</li>
 *
 * </ul>
 *
 * The byte order of a view buffer is fixed to be that of its byte buffer
 * at the time that the view is created.
 *
 * <h2> Invocation chaining </h2>
 *
 * Methods in this class that do not otherwise have a value to return are
 * specified to return the buffer upon which they are invoked.  This allows
 * method invocations to be chained.
 *
 * The sequence of statements
 *
 * <blockquote><pre>
 * bb.putInt(0xCAFEBABE);
 * bb.putShort(3);
 * bb.putShort(45);</pre></blockquote>
 *
 * can, for example, be replaced by the single statement
 *
 * <blockquote><pre>
 * bb.putInt(0xCAFEBABE).putShort(3).putShort(45);
 * </pre></blockquote>
 */
public abstract class ByteBuffer extends Buffer implements Comparable<ByteBuffer> {
    // These fields are declared here rather than in Heap-X-Buffer in order to
    // reduce the number of virtual method invocations needed to access these
    // values, which is especially costly when coding small buffers.
    //
    final byte[] hb;                  // Non-null only for heap buffers
    final int offset;
    boolean isReadOnly;

    // Creates a new buffer with the given mark, position, limit, capacity,
    // backing array, and array offset
    //
    ByteBuffer(int mark, int pos, int lim, int cap, byte[] hb, int offset) {
        super(mark, pos, lim, cap);
        this.hb = hb;
        this.offset = offset;
    }

    // Creates a new buffer with the given mark, position, limit, and capacity
    //
    ByteBuffer(int mark, int pos, int lim, int cap) {
        this(mark, pos, lim, cap, null, 0);
    }

    // @Override
    Object base() {
        return hb;
    }

    /**
     * Allocates a new byte buffer.
     *
     * The new buffer's position will be zero, its limit will be its
     * capacity, its mark will be undefined, each of its elements will be
     * initialized to zero, and its byte order will be
     *
     * {@link ByteOrder#BIG_ENDIAN BIG_ENDIAN}.
     *
     * It will have a {@link #array backing array}, and its
     * {@link #arrayOffset array offset} will be zero.
     *
     * @param  capacity
     *         The new buffer's capacity, in bytes
     *
     * @return  The new byte buffer
     *
     * @throws  IllegalArgumentException
     *          If the {@code capacity} is a negative integer
     */
    public static ByteBuffer allocate(int capacity) {
        if (capacity < 0)
            throw createCapacityException(capacity);
        return new HeapByteBuffer(capacity, capacity);
    }

    /**
     * Wraps a byte array into a buffer.
     *
     * The new buffer will be backed by the given byte array;
     * that is, modifications to the buffer will cause the array to be modified
     * and vice versa.  The new buffer's capacity will be
     * {@code array.length}, its position will be {@code offset}, its limit
     * will be {@code offset + length}, its mark will be undefined, and its
     * byte order will be
     *
     * {@link ByteOrder#BIG_ENDIAN BIG_ENDIAN}.
     *
     * Its {@link #array backing array} will be the given array, and
     * its {@link #arrayOffset array offset} will be zero.
     *
     * @param  array
     *         The array that will back the new buffer
     *
     * @param  offset
     *         The offset of the subarray to be used; must be non-negative and
     *         no larger than {@code array.length}.  The new buffer's position
     *         will be set to this value.
     *
     * @param  length
     *         The length of the subarray to be used;
     *         must be non-negative and no larger than
     *         {@code array.length - offset}.
     *         The new buffer's limit will be set to {@code offset + length}.
     *
     * @return  The new byte buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If the preconditions on the {@code offset} and {@code length}
     *          parameters do not hold
     */
    public static ByteBuffer wrap(byte[] array, int offset, int length) {
        try {
            return new HeapByteBuffer(array, offset, length);
        } catch (IllegalArgumentException x) {
            throw new IndexOutOfBoundsException();
        }
    }

    /**
     * Wraps a byte array into a buffer.
     *
     * The new buffer will be backed by the given byte array;
     * that is, modifications to the buffer will cause the array to be modified
     * and vice versa.  The new buffer's capacity and limit will be
     * {@code array.length}, its position will be zero, its mark will be
     * undefined, and its byte order will be
     *
     * {@link ByteOrder#BIG_ENDIAN BIG_ENDIAN}.
     *
     * Its {@link #array backing array} will be the given array, and its
     * {@link #arrayOffset array offset} will be zero.
     *
     * @param  array
     *         The array that will back this buffer
     *
     * @return  The new byte buffer
     */
    public static ByteBuffer wrap(byte[] array) {
        return wrap(array, 0, array.length);
    }

    /**
     * Creates a new byte buffer whose content is a shared subsequence of
     * this buffer's content.
     *
     * The content of the new buffer will start at this buffer's current
     * position.  Changes to this buffer's content will be visible in the new
     * buffer, and vice versa; the two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's position will be zero, its capacity and its limit
     * will be the number of bytes remaining in this buffer, its mark will be
     * undefined, and its byte order will be
     *
     * {@link ByteOrder#BIG_ENDIAN BIG_ENDIAN}.
     *
     * The new buffer will be direct if, and only if, this buffer is direct, and
     * it will be read-only if, and only if, this buffer is read-only.
     *
     * @return  The new byte buffer
     */
    // @Override
    public abstract ByteBuffer slice();

    /**
     * Creates a new byte buffer that shares this buffer's content.
     *
     * The content of the new buffer will be that of this buffer.  Changes
     * to this buffer's content will be visible in the new buffer, and vice
     * versa; the two buffers' position, limit, and mark values will be
     * independent.
     *
     * The new buffer's capacity, limit, position,
     *
     * and mark values will be identical to those of this buffer, and its byte
     * order will be {@link ByteOrder#BIG_ENDIAN BIG_ENDIAN}.
     *
     * The new buffer will be direct if, and only if, this buffer is direct, and
     * it will be read-only if, and only if, this buffer is read-only.
     *
     * @return  The new byte buffer
     */
    // @Override
    public abstract ByteBuffer duplicate();

    /**
     * Creates a new, read-only byte buffer that shares this buffer's
     * content.
     *
     * The content of the new buffer will be that of this buffer.  Changes
     * to this buffer's content will be visible in the new buffer; the new
     * buffer itself, however, will be read-only and will not allow the shared
     * content to be modified.  The two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's capacity, limit, position,
     *
     * and mark values will be identical to those of this buffer, and its byte
     * order will be {@link ByteOrder#BIG_ENDIAN BIG_ENDIAN}.
     *
     * If this buffer is itself read-only then this method behaves in
     * exactly the same way as the {@link #duplicate duplicate} method.
     *
     * @return  The new, read-only byte buffer
     */
    public abstract ByteBuffer asReadOnlyBuffer();

    // -- Singleton get/put methods --

    /**
     * Relative <i>get</i> method.  Reads the byte at this buffer's
     * current position, and then increments the position.
     *
     * @return  The byte at the buffer's current position
     *
     * @throws  BufferUnderflowException
     *          If the buffer's current position is not smaller than its limit
     */
    public abstract byte get();

    /**
     * Relative <i>put</i> method&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes the given byte into this buffer at the current
     * position, and then increments the position.
     *
     * @param  b
     *         The byte to be written
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If this buffer's current position is not smaller than its limit
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer put(byte b);

    /**
     * Absolute <i>get</i> method.  Reads the byte at the given
     * index.
     *
     * @param  index
     *         The index from which the byte will be read
     *
     * @return  The byte at the given index
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit
     */
    public abstract byte get(int index);

    /**
     * Absolute <i>put</i> method&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes the given byte into this buffer at the given
     * index.
     *
     * @param  index
     *         The index at which the byte will be written
     *
     * @param  b
     *         The byte value to be written
     *
     * @return  This buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer put(int index, byte b);

    // -- Bulk get operations --

    /**
     * Relative bulk <i>get</i> method.
     *
     * This method transfers bytes from this buffer into the given
     * destination array.  If there are fewer bytes remaining in the
     * buffer than are required to satisfy the request, that is, if
     * {@code length}&nbsp;{@code >}&nbsp;{@code remaining()}, then no
     * bytes are transferred and a {@link BufferUnderflowException} is
     * thrown.
     *
     * Otherwise, this method copies {@code length} bytes from this
     * buffer into the given array, starting at the current position of this
     * buffer and at the given offset in the array.  The position of this
     * buffer is then incremented by {@code length}.
     *
     * In other words, an invocation of this method of the form
     * <code>src.get(dst,&nbsp;off,&nbsp;len)</code> has exactly the same effect as
     * the loop
     *
     * <pre>{@code
     *     for (int i = off; i < off + len; i++)
     *         dst[i] = src.get();
     * }</pre>
     *
     * except that it first checks that there are sufficient bytes in
     * this buffer and it is potentially much more efficient.
     *
     * @param  dst
     *         The array into which bytes are to be written
     *
     * @param  offset
     *         The offset within the array of the first byte to be
     *         written; must be non-negative and no larger than
     *         {@code dst.length}
     *
     * @param  length
     *         The maximum number of bytes to be written to the given
     *         array; must be non-negative and no larger than
     *         {@code dst.length - offset}
     *
     * @return  This buffer
     *
     * @throws  BufferUnderflowException
     *          If there are fewer than {@code length} bytes
     *          remaining in this buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If the preconditions on the {@code offset} and {@code length}
     *          parameters do not hold
     */
    public ByteBuffer get(byte[] dst, int offset, int length) {
        // oops! checkBounds(offset, length, dst.length);
        if (length > remaining())
            throw new BufferUnderflowException();
        int end = offset + length;
        for (int i = offset; i < end; i++)
            dst[i] = get();
        return this;
    }

    /**
     * Relative bulk <i>get</i> method.
     *
     * This method transfers bytes from this buffer into the given
     * destination array.  An invocation of this method of the form
     * {@code src.get(a)} behaves in exactly the same way as the invocation
     *
     * <pre>
     *     src.get(a, 0, a.length) </pre>
     *
     * @param   dst
     *          The destination array
     *
     * @return  This buffer
     *
     * @throws  BufferUnderflowException
     *          If there are fewer than {@code length} bytes
     *          remaining in this buffer
     */
    public ByteBuffer get(byte[] dst) {
        return get(dst, 0, dst.length);
    }

    // -- Bulk put operations --

    /**
     * Relative bulk <i>put</i> method&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * This method transfers the bytes remaining in the given source
     * buffer into this buffer.  If there are more bytes remaining in the
     * source buffer than in this buffer, that is, if
     * {@code src.remaining()}&nbsp;{@code >}&nbsp;{@code remaining()},
     * then no bytes are transferred and a {@link
     * BufferOverflowException} is thrown.
     *
     * Otherwise, this method copies
     * <i>n</i>&nbsp;=&nbsp;{@code src.remaining()} bytes from the given
     * buffer into this buffer, starting at each buffer's current position.
     * The positions of both buffers are then incremented by <i>n</i>.
     *
     * In other words, an invocation of this method of the form
     * {@code dst.put(src)} has exactly the same effect as the loop
     *
     * <pre>
     *     while (src.hasRemaining())
     *         dst.put(src.get()); </pre>
     *
     * except that it first checks that there is sufficient space in this
     * buffer and it is potentially much more efficient.
     *
     * @param  src
     *         The source buffer from which bytes are to be read;
     *         must not be this buffer
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there is insufficient space in this buffer
     *          for the remaining bytes in the source buffer
     *
     * @throws  IllegalArgumentException
     *          If the source buffer is this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public ByteBuffer put(ByteBuffer src) {
        if (src == this)
            throw createSameBufferException();
        if (isReadOnly())
            throw new ReadOnlyBufferException();
        int n = src.remaining();
        if (n > remaining())
            throw new BufferOverflowException();
        for (int i = 0; i < n; i++)
            put(src.get());
        return this;
    }

    /**
     * Relative bulk <i>put</i> method&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * This method transfers bytes into this buffer from the given
     * source array.  If there are more bytes to be copied from the array
     * than remain in this buffer, that is, if
     * {@code length}&nbsp;{@code >}&nbsp;{@code remaining()}, then no
     * bytes are transferred and a {@link BufferOverflowException} is
     * thrown.
     *
     * Otherwise, this method copies {@code length} bytes from the
     * given array into this buffer, starting at the given offset in the array
     * and at the current position of this buffer.  The position of this buffer
     * is then incremented by {@code length}.
     *
     * In other words, an invocation of this method of the form
     * <code>dst.put(src,&nbsp;off,&nbsp;len)</code> has exactly the same effect as
     * the loop
     *
     * <pre>{@code
     *     for (int i = off; i < off + len; i++)
     *         dst.put(a[i]);
     * }</pre>
     *
     * except that it first checks that there is sufficient space in this
     * buffer and it is potentially much more efficient.
     *
     * @param  src
     *         The array from which bytes are to be read
     *
     * @param  offset
     *         The offset within the array of the first byte to be read;
     *         must be non-negative and no larger than {@code array.length}
     *
     * @param  length
     *         The number of bytes to be read from the given array;
     *         must be non-negative and no larger than
     *         {@code array.length - offset}
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there is insufficient space in this buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If the preconditions on the {@code offset} and {@code length}
     *          parameters do not hold
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public ByteBuffer put(byte[] src, int offset, int length) {
        // oops! checkBounds(offset, length, src.length);
        if (length > remaining())
            throw new BufferOverflowException();
        int end = offset + length;
        for (int i = offset; i < end; i++)
            this.put(src[i]);
        return this;
    }

    /**
     * Relative bulk <i>put</i> method&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * This method transfers the entire content of the given source
     * byte array into this buffer.  An invocation of this method of the
     * form {@code dst.put(a)} behaves in exactly the same way as the
     * invocation
     *
     * <pre>
     *     dst.put(a, 0, a.length) </pre>
     *
     * @param   src
     *          The source array
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there is insufficient space in this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public final ByteBuffer put(byte[] src) {
        return put(src, 0, src.length);
    }

    // -- Other stuff --

    /**
     * Tells whether or not this buffer is backed by an accessible byte
     * array.
     *
     * If this method returns {@code true} then the {@link #array() array}
     * and {@link #arrayOffset() arrayOffset} methods may safely be invoked.
     *
     * @return  {@code true} if, and only if, this buffer
     *          is backed by an array and is not read-only
     */
    public final boolean hasArray() {
        return (hb != null) && !isReadOnly;
    }

    /**
     * Returns the byte array that backs this
     * buffer&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Modifications to this buffer's content will cause the returned
     * array's content to be modified, and vice versa.
     *
     * Invoke the {@link #hasArray hasArray} method before invoking this
     * method in order to ensure that this buffer has an accessible backing
     * array.
     *
     * @return  The array that backs this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is backed by an array but is read-only
     *
     * @throws  UnsupportedOperationException
     *          If this buffer is not backed by an accessible array
     */
    public final byte[] array() {
        if (hb == null)
            throw new UnsupportedOperationException();
        if (isReadOnly)
            throw new ReadOnlyBufferException();
        return hb;
    }

    /**
     * Returns the offset within this buffer's backing array of the first
     * element of the buffer&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * If this buffer is backed by an array then buffer position <i>p</i>
     * corresponds to array index <i>p</i>&nbsp;+&nbsp;{@code arrayOffset()}.
     *
     * Invoke the {@link #hasArray hasArray} method before invoking this
     * method in order to ensure that this buffer has an accessible backing
     * array.
     *
     * @return  The offset within this buffer's array
     *          of the first element of the buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is backed by an array but is read-only
     *
     * @throws  UnsupportedOperationException
     *          If this buffer is not backed by an accessible array
     */
    public final int arrayOffset() {
        if (hb == null)
            throw new UnsupportedOperationException();
        if (isReadOnly)
            throw new ReadOnlyBufferException();
        return offset;
    }

    // -- Covariant return type overrides

    /**
     * {@inheritDoc}
     */
    // @Override
    public

    ByteBuffer position(int newPosition) {
        super.position(newPosition);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public

    ByteBuffer limit(int newLimit) {
        super.limit(newLimit);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public

    ByteBuffer mark() {
        super.mark();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public

    ByteBuffer reset() {
        super.reset();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public

    ByteBuffer clear() {
        super.clear();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public

    ByteBuffer flip() {
        super.flip();
        return this;
    }

    /**
     * {@inheritDoc}
     */
    // @Override
    public

    ByteBuffer rewind() {
        super.rewind();
        return this;
    }

    /**
     * Compacts this buffer&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * The bytes between the buffer's current position and its limit,
     * if any, are copied to the beginning of the buffer.  That is, the
     * byte at index <i>p</i>&nbsp;=&nbsp;{@code position()} is copied
     * to index zero, the byte at index <i>p</i>&nbsp;+&nbsp;1 is copied
     * to index one, and so forth until the byte at index
     * {@code limit()}&nbsp;-&nbsp;1 is copied to index
     * <i>n</i>&nbsp;=&nbsp;{@code limit()}&nbsp;-&nbsp;{@code 1}&nbsp;-&nbsp;<i>p</i>.
     * The buffer's position is then set to <i>n+1</i> and its limit is set to
     * its capacity.  The mark, if defined, is discarded.
     *
     * The buffer's position is set to the number of bytes copied,
     * rather than to zero, so that an invocation of this method can be
     * followed immediately by an invocation of another relative <i>put</i>
     * method.
     *
     * Invoke this method after writing data from a buffer in case the
     * write was incomplete.  The following loop, for example, copies bytes
     * from one channel to another via the buffer {@code buf}:
     *
     * <blockquote><pre>{@code
     *   buf.clear();          // Prepare buffer for use
     *   while (in.read(buf) >= 0 || buf.position != 0) {
     *       buf.flip();
     *       out.write(buf);
     *       buf.compact();    // In case of partial write
     *   }
     * }</pre></blockquote>
     *
     * @return  This buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer compact();

    /**
     * Tells whether or not this byte buffer is direct.
     *
     * @return  {@code true} if, and only if, this buffer is direct
     */
    public abstract boolean isDirect();

    /**
     * Returns a string summarizing the state of this buffer.
     *
     * @return  A summary string
     */
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append(getClass().getName());
        sb.append("[pos=");
        sb.append(position());
        sb.append(" lim=");
        sb.append(limit());
        sb.append(" cap=");
        sb.append(capacity());
        sb.append("]");
        return sb.toString();
    }

    /**
     * Returns the current hash code of this buffer.
     *
     * The hash code of a byte buffer depends only upon its remaining
     * elements; that is, upon the elements from {@code position()} up to, and
     * including, the element at {@code limit()}&nbsp;-&nbsp;{@code 1}.
     *
     * Because buffer hash codes are content-dependent, it is inadvisable
     * to use buffers as keys in hash maps or similar data structures unless it
     * is known that their contents will not change.
     *
     * @return  The current hash code of this buffer
     */
    public int hashCode() {
        int h = 1;
        int p = position();
        for (int i = limit() - 1; i >= p; i--)

            h = 31 * h + (int)get(i);

        return h;
    }

    /**
     * Tells whether or not this buffer is equal to another object.
     *
     * Two byte buffers are equal if, and only if,
     *
     * <ol>
     *   <li>They have the same element type,</li>
     *
     *   <li>They have the same number of remaining elements, and</li>
     *
     *   <li>The two sequences of remaining elements, considered
     *   independently of their starting positions, are pointwise equal.</li>
     * </ol>
     *
     * A byte buffer is not equal to any other type of object.
     *
     * @param  ob  The object to which this buffer is to be compared
     *
     * @return  {@code true} if, and only if, this buffer is equal to the
     *           given object
     */
    public boolean equals(Object ob) {
        if (this == ob)
            return true;
        if (!(ob instanceof ByteBuffer))
            return false;
        ByteBuffer that = (ByteBuffer)ob;
        if (this.remaining() != that.remaining())
            return false;
        return BufferMismatch.mismatch(this, this.position(), that, that.position(), this.remaining()) < 0;
    }

    /**
     * Compares this buffer to another.
     *
     * Two byte buffers are compared by comparing their sequences of
     * remaining elements lexicographically, without regard to the starting
     * position of each sequence within its corresponding buffer.
     *
     * Pairs of {@code byte} elements are compared as if by invoking
     * {@link Byte#compare(byte,byte)}.
     *
     * A byte buffer is not comparable to any other type of object.
     *
     * @return  A negative integer, zero, or a positive integer as this buffer
     *          is less than, equal to, or greater than the given buffer
     */
    public int compareTo(ByteBuffer that) {
        int i = BufferMismatch.mismatch(this, this.position(), that, that.position(), Math.min(this.remaining(), that.remaining()));
        if (i >= 0) {
            return compare(this.get(this.position() + i), that.get(that.position() + i));
        }
        return this.remaining() - that.remaining();
    }

    private static int compare(byte x, byte y) {
        return Byte.compare(x, y);
    }

    /**
     * Finds and returns the relative index of the first mismatch between this
     * buffer and a given buffer.  The index is relative to the
     * {@link #position() position} of each buffer and will be in the range of
     * 0 (inclusive) up to the smaller of the {@link #remaining() remaining}
     * elements in each buffer (exclusive).
     *
     * If the two buffers share a common prefix then the returned index is
     * the length of the common prefix and it follows that there is a mismatch
     * between the two buffers at that index within the respective buffers.
     * If one buffer is a proper prefix of the other then the returned index is
     * the smaller of the remaining elements in each buffer, and it follows that
     * the index is only valid for the buffer with the larger number of
     * remaining elements.
     * Otherwise, there is no mismatch.
     *
     * @param  that
     *         The byte buffer to be tested for a mismatch with this buffer
     *
     * @return  The relative index of the first mismatch between this and the
     *          given buffer, otherwise -1 if no mismatch.
     */
    public int mismatch(ByteBuffer that) {
        int length = Math.min(this.remaining(), that.remaining());
        int r = BufferMismatch.mismatch(this, this.position(), that, that.position(), length);
        return (r == -1 && this.remaining() != that.remaining()) ? length : r;
    }

    // -- Other char stuff --

    // -- Other byte stuff: Access to binary data --

    boolean bigEndian = true;
    boolean nativeByteOrder = (ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN);

    /**
     * Retrieves this buffer's byte order.
     *
     * The byte order is used when reading or writing multibyte values, and
     * when creating buffers that are views of this byte buffer.  The order of
     * a newly-created byte buffer is always {@link ByteOrder#BIG_ENDIAN
     * BIG_ENDIAN}.
     *
     * @return  This buffer's byte order
     */
    public final ByteOrder order() {
        return bigEndian ? ByteOrder.BIG_ENDIAN : ByteOrder.LITTLE_ENDIAN;
    }

    /**
     * Modifies this buffer's byte order.
     *
     * @param  bo
     *         The new byte order,
     *         either {@link ByteOrder#BIG_ENDIAN BIG_ENDIAN}
     *         or {@link ByteOrder#LITTLE_ENDIAN LITTLE_ENDIAN}
     *
     * @return  This buffer
     */
    public final ByteBuffer order(ByteOrder bo) {
        bigEndian = (bo == ByteOrder.BIG_ENDIAN);
        nativeByteOrder =
            (bigEndian == (ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN));
        return this;
    }

    /**
     * Returns the memory address, pointing to the byte at the given index,
     * modulus the given unit size.
     *
     * A return value greater than zero indicates the address of the byte at
     * the index is misaligned for the unit size, and the value's quantity
     * indicates how much the index should be rounded up or down to locate a
     * byte at an aligned address.  Otherwise, a value of {@code 0} indicates
     * that the address of the byte at the index is aligned for the unit size.
     *
     * @apiNote
     * This method may be utilized to determine if unit size bytes from an
     * index can be accessed atomically, if supported by the native platform.
     *
     * @implNote
     * This implementation throws {@code UnsupportedOperationException} for
     * non-direct buffers when the given unit size is greater then {@code 8}.
     *
     * @param  index
     *         The index to query for alignment offset, must be non-negative, no
     *         upper bounds check is performed
     *
     * @param  unitSize
     *         The unit size in bytes, must be a power of {@code 2}
     *
     * @return  The indexed byte's memory address modulus the unit size
     *
     * @throws IllegalArgumentException
     *         If the index is negative or the unit size is not a power of
     *         {@code 2}
     *
     * @throws UnsupportedOperationException
     *         If the native platform does not guarantee stable alignment offset
     *         values for the given unit size when managing the memory regions
     *         of buffers of the same kind as this buffer (direct or
     *         non-direct).  For example, if garbage collection would result
     *         in the moving of a memory region covered by a non-direct buffer
     *         from one location to another and both locations have different
     *         alignment characteristics.
     */
    public final int alignmentOffset(int index, int unitSize) {
        if (index < 0)
            throw new IllegalArgumentException(String.str("Index less than zero: ", index));
        if (unitSize < 1 || (unitSize & (unitSize - 1)) != 0)
            throw new IllegalArgumentException(String.str("Unit size not a power of two: ", unitSize));
        if (unitSize > 8 && !isDirect())
            throw new UnsupportedOperationException(String.str("Unit size unsupported for non-direct buffers: ", unitSize));

        return (int) ((address + index) % unitSize);
    }

    /**
     * Creates a new byte buffer whose content is a shared and aligned
     * subsequence of this buffer's content.
     *
     * The content of the new buffer will start at this buffer's current
     * position rounded up to the index of the nearest aligned byte for the
     * given unit size, and end at this buffer's limit rounded down to the index
     * of the nearest aligned byte for the given unit size.
     * If rounding results in out-of-bound values then the new buffer's capacity
     * and limit will be zero.  If rounding is within bounds the following
     * expressions will be true for a new buffer {@code nb} and unit size
     * {@code unitSize}:
     * <pre>{@code
     * nb.alignmentOffset(0, unitSize) == 0
     * nb.alignmentOffset(nb.limit(), unitSize) == 0
     * }</pre>
     *
     * Changes to this buffer's content will be visible in the new
     * buffer, and vice versa; the two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's position will be zero, its capacity and its limit
     * will be the number of bytes remaining in this buffer or fewer subject to
     * alignment, its mark will be undefined, and its byte order will be
     * {@link ByteOrder#BIG_ENDIAN BIG_ENDIAN}.
     *
     * The new buffer will be direct if, and only if, this buffer is direct, and
     * it will be read-only if, and only if, this buffer is read-only.
     *
     * @apiNote
     * This method may be utilized to create a new buffer where unit size bytes
     * from index, that is a multiple of the unit size, may be accessed
     * atomically, if supported by the native platform.
     *
     * @implNote
     * This implementation throws {@code UnsupportedOperationException} for
     * non-direct buffers when the given unit size is greater then {@code 8}.
     *
     * @param  unitSize
     *         The unit size in bytes, must be a power of {@code 2}
     *
     * @return  The new byte buffer
     *
     * @throws IllegalArgumentException
     *         If the unit size not a power of {@code 2}
     *
     * @throws UnsupportedOperationException
     *         If the native platform does not guarantee stable aligned slices
     *         for the given unit size when managing the memory regions
     *         of buffers of the same kind as this buffer (direct or
     *         non-direct).  For example, if garbage collection would result
     *         in the moving of a memory region covered by a non-direct buffer
     *         from one location to another and both locations have different
     *         alignment characteristics.
     */
    public final ByteBuffer alignedSlice(int unitSize) {
        int pos = position();
        int lim = limit();

        int pos_mod = alignmentOffset(pos, unitSize);
        int lim_mod = alignmentOffset(lim, unitSize);

        // Round up the position to align with unit size
        int aligned_pos = (pos_mod > 0)
            ? pos + (unitSize - pos_mod)
            : pos;

        // Round down the limit to align with unit size
        int aligned_lim = lim - lim_mod;

        if (aligned_pos > lim || aligned_lim < pos) {
            aligned_pos = aligned_lim = pos;
        }

        return slice(aligned_pos, aligned_lim);
    }

    abstract ByteBuffer slice(int pos, int lim);

    /**
     * Relative <i>get</i> method for reading a char value.
     *
     * Reads the next two bytes at this buffer's current position,
     * composing them into a char value according to the current byte order,
     * and then increments the position by two.
     *
     * @return  The char value at the buffer's current position
     *
     * @throws  BufferUnderflowException
     *          If there are fewer than two bytes
     *          remaining in this buffer
     */
    public abstract char getChar();

    /**
     * Relative <i>put</i> method for writing a char
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes two bytes containing the given char value, in the
     * current byte order, into this buffer at the current position, and then
     * increments the position by two.
     *
     * @param  value
     *         The char value to be written
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there are fewer than two bytes
     *          remaining in this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putChar(char value);

    /**
     * Absolute <i>get</i> method for reading a char value.
     *
     * Reads two bytes at the given index, composing them into a
     * char value according to the current byte order.
     *
     * @param  index
     *         The index from which the bytes will be read
     *
     * @return  The char value at the given index
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus one
     */
    public abstract char getChar(int index);

    /**
     * Absolute <i>put</i> method for writing a char
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes two bytes containing the given char value, in the
     * current byte order, into this buffer at the given index.
     *
     * @param  index
     *         The index at which the bytes will be written
     *
     * @param  value
     *         The char value to be written
     *
     * @return  This buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus one
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putChar(int index, char value);

    /**
     * Creates a view of this byte buffer as a char buffer.
     *
     * The content of the new buffer will start at this buffer's current
     * position.  Changes to this buffer's content will be visible in the new
     * buffer, and vice versa; the two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's position will be zero, its capacity and its limit
     * will be the number of bytes remaining in this buffer divided by
     * two, its mark will be undefined, and its byte order will be that
     * of the byte buffer at the moment the view is created.  The new buffer
     * will be direct if, and only if, this buffer is direct, and it will be
     * read-only if, and only if, this buffer is read-only.
     *
     * @return  A new char buffer
     */
    public abstract CharBuffer asCharBuffer();

    /**
     * Relative <i>get</i> method for reading a short value.
     *
     * Reads the next two bytes at this buffer's current position,
     * composing them into a short value according to the current byte order,
     * and then increments the position by two.
     *
     * @return  The short value at the buffer's current position
     *
     * @throws  BufferUnderflowException
     *          If there are fewer than two bytes
     *          remaining in this buffer
     */
    public abstract short getShort();

    /**
     * Relative <i>put</i> method for writing a short
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes two bytes containing the given short value, in the
     * current byte order, into this buffer at the current position, and then
     * increments the position by two.
     *
     * @param  value
     *         The short value to be written
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there are fewer than two bytes
     *          remaining in this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putShort(short value);

    /**
     * Absolute <i>get</i> method for reading a short value.
     *
     * Reads two bytes at the given index, composing them into a
     * short value according to the current byte order.
     *
     * @param  index
     *         The index from which the bytes will be read
     *
     * @return  The short value at the given index
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus one
     */
    public abstract short getShort(int index);

    /**
     * Absolute <i>put</i> method for writing a short
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes two bytes containing the given short value, in the
     * current byte order, into this buffer at the given index.
     *
     * @param  index
     *         The index at which the bytes will be written
     *
     * @param  value
     *         The short value to be written
     *
     * @return  This buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus one
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putShort(int index, short value);

    /**
     * Creates a view of this byte buffer as a short buffer.
     *
     * The content of the new buffer will start at this buffer's current
     * position.  Changes to this buffer's content will be visible in the new
     * buffer, and vice versa; the two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's position will be zero, its capacity and its limit
     * will be the number of bytes remaining in this buffer divided by
     * two, its mark will be undefined, and its byte order will be that
     * of the byte buffer at the moment the view is created.  The new buffer
     * will be direct if, and only if, this buffer is direct, and it will be
     * read-only if, and only if, this buffer is read-only.
     *
     * @return  A new short buffer
     */
    public abstract ShortBuffer asShortBuffer();

    /**
     * Relative <i>get</i> method for reading an int value.
     *
     * Reads the next four bytes at this buffer's current position,
     * composing them into an int value according to the current byte order,
     * and then increments the position by four.
     *
     * @return  The int value at the buffer's current position
     *
     * @throws  BufferUnderflowException
     *          If there are fewer than four bytes
     *          remaining in this buffer
     */
    public abstract int getInt();

    /**
     * Relative <i>put</i> method for writing an int
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes four bytes containing the given int value, in the
     * current byte order, into this buffer at the current position, and then
     * increments the position by four.
     *
     * @param  value
     *         The int value to be written
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there are fewer than four bytes
     *          remaining in this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putInt(int value);

    /**
     * Absolute <i>get</i> method for reading an int value.
     *
     * Reads four bytes at the given index, composing them into a
     * int value according to the current byte order.
     *
     * @param  index
     *         The index from which the bytes will be read
     *
     * @return  The int value at the given index
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus three
     */
    public abstract int getInt(int index);

    /**
     * Absolute <i>put</i> method for writing an int
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes four bytes containing the given int value, in the
     * current byte order, into this buffer at the given index.
     *
     * @param  index
     *         The index at which the bytes will be written
     *
     * @param  value
     *         The int value to be written
     *
     * @return  This buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus three
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putInt(int index, int value);

    /**
     * Creates a view of this byte buffer as an int buffer.
     *
     * The content of the new buffer will start at this buffer's current
     * position.  Changes to this buffer's content will be visible in the new
     * buffer, and vice versa; the two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's position will be zero, its capacity and its limit
     * will be the number of bytes remaining in this buffer divided by
     * four, its mark will be undefined, and its byte order will be that
     * of the byte buffer at the moment the view is created.  The new buffer
     * will be direct if, and only if, this buffer is direct, and it will be
     * read-only if, and only if, this buffer is read-only.
     *
     * @return  A new int buffer
     */
    public abstract IntBuffer asIntBuffer();

    /**
     * Relative <i>get</i> method for reading a long value.
     *
     * Reads the next eight bytes at this buffer's current position,
     * composing them into a long value according to the current byte order,
     * and then increments the position by eight.
     *
     * @return  The long value at the buffer's current position
     *
     * @throws  BufferUnderflowException
     *          If there are fewer than eight bytes
     *          remaining in this buffer
     */
    public abstract long getLong();

    /**
     * Relative <i>put</i> method for writing a long
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes eight bytes containing the given long value, in the
     * current byte order, into this buffer at the current position, and then
     * increments the position by eight.
     *
     * @param  value
     *         The long value to be written
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there are fewer than eight bytes
     *          remaining in this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putLong(long value);

    /**
     * Absolute <i>get</i> method for reading a long value.
     *
     * Reads eight bytes at the given index, composing them into a
     * long value according to the current byte order.
     *
     * @param  index
     *         The index from which the bytes will be read
     *
     * @return  The long value at the given index
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus seven
     */
    public abstract long getLong(int index);

    /**
     * Absolute <i>put</i> method for writing a long
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes eight bytes containing the given long value, in the
     * current byte order, into this buffer at the given index.
     *
     * @param  index
     *         The index at which the bytes will be written
     *
     * @param  value
     *         The long value to be written
     *
     * @return  This buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus seven
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putLong(int index, long value);

    /**
     * Creates a view of this byte buffer as a long buffer.
     *
     * The content of the new buffer will start at this buffer's current
     * position.  Changes to this buffer's content will be visible in the new
     * buffer, and vice versa; the two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's position will be zero, its capacity and its limit
     * will be the number of bytes remaining in this buffer divided by
     * eight, its mark will be undefined, and its byte order will be that
     * of the byte buffer at the moment the view is created.  The new buffer
     * will be direct if, and only if, this buffer is direct, and it will be
     * read-only if, and only if, this buffer is read-only.
     *
     * @return  A new long buffer
     */
    public abstract LongBuffer asLongBuffer();

    /**
     * Relative <i>get</i> method for reading a float value.
     *
     * Reads the next four bytes at this buffer's current position,
     * composing them into a float value according to the current byte order,
     * and then increments the position by four.
     *
     * @return  The float value at the buffer's current position
     *
     * @throws  BufferUnderflowException
     *          If there are fewer than four bytes
     *          remaining in this buffer
     */
    public abstract float getFloat();

    /**
     * Relative <i>put</i> method for writing a float
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes four bytes containing the given float value, in the
     * current byte order, into this buffer at the current position, and then
     * increments the position by four.
     *
     * @param  value
     *         The float value to be written
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there are fewer than four bytes
     *          remaining in this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putFloat(float value);

    /**
     * Absolute <i>get</i> method for reading a float value.
     *
     * Reads four bytes at the given index, composing them into a
     * float value according to the current byte order.
     *
     * @param  index
     *         The index from which the bytes will be read
     *
     * @return  The float value at the given index
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus three
     */
    public abstract float getFloat(int index);

    /**
     * Absolute <i>put</i> method for writing a float
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes four bytes containing the given float value, in the
     * current byte order, into this buffer at the given index.
     *
     * @param  index
     *         The index at which the bytes will be written
     *
     * @param  value
     *         The float value to be written
     *
     * @return  This buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus three
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putFloat(int index, float value);

    /**
     * Creates a view of this byte buffer as a float buffer.
     *
     * The content of the new buffer will start at this buffer's current
     * position.  Changes to this buffer's content will be visible in the new
     * buffer, and vice versa; the two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's position will be zero, its capacity and its limit
     * will be the number of bytes remaining in this buffer divided by
     * four, its mark will be undefined, and its byte order will be that
     * of the byte buffer at the moment the view is created.  The new buffer
     * will be direct if, and only if, this buffer is direct, and it will be
     * read-only if, and only if, this buffer is read-only.
     *
     * @return  A new float buffer
     */
    public abstract FloatBuffer asFloatBuffer();

    /**
     * Relative <i>get</i> method for reading a double value.
     *
     * Reads the next eight bytes at this buffer's current position,
     * composing them into a double value according to the current byte order,
     * and then increments the position by eight.
     *
     * @return  The double value at the buffer's current position
     *
     * @throws  BufferUnderflowException
     *          If there are fewer than eight bytes
     *          remaining in this buffer
     */
    public abstract double getDouble();

    /**
     * Relative <i>put</i> method for writing a double
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes eight bytes containing the given double value, in the
     * current byte order, into this buffer at the current position, and then
     * increments the position by eight.
     *
     * @param  value
     *         The double value to be written
     *
     * @return  This buffer
     *
     * @throws  BufferOverflowException
     *          If there are fewer than eight bytes
     *          remaining in this buffer
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putDouble(double value);

    /**
     * Absolute <i>get</i> method for reading a double value.
     *
     * Reads eight bytes at the given index, composing them into a
     * double value according to the current byte order.
     *
     * @param  index
     *         The index from which the bytes will be read
     *
     * @return  The double value at the given index
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus seven
     */
    public abstract double getDouble(int index);

    /**
     * Absolute <i>put</i> method for writing a double
     * value&nbsp;&nbsp;<i>(optional operation)</i>.
     *
     * Writes eight bytes containing the given double value, in the
     * current byte order, into this buffer at the given index.
     *
     * @param  index
     *         The index at which the bytes will be written
     *
     * @param  value
     *         The double value to be written
     *
     * @return  This buffer
     *
     * @throws  IndexOutOfBoundsException
     *          If {@code index} is negative
     *          or not smaller than the buffer's limit,
     *          minus seven
     *
     * @throws  ReadOnlyBufferException
     *          If this buffer is read-only
     */
    public abstract ByteBuffer putDouble(int index, double value);

    /**
     * Creates a view of this byte buffer as a double buffer.
     *
     * The content of the new buffer will start at this buffer's current
     * position.  Changes to this buffer's content will be visible in the new
     * buffer, and vice versa; the two buffers' position, limit, and mark
     * values will be independent.
     *
     * The new buffer's position will be zero, its capacity and its limit
     * will be the number of bytes remaining in this buffer divided by
     * eight, its mark will be undefined, and its byte order will be that
     * of the byte buffer at the moment the view is created.  The new buffer
     * will be direct if, and only if, this buffer is direct, and it will be
     * read-only if, and only if, this buffer is read-only.
     *
     * @return  A new double buffer
     */
    public abstract DoubleBuffer asDoubleBuffer();
}

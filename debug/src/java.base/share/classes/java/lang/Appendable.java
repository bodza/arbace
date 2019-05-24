package java.lang;

import java.io.IOException;

/**
 * An object to which {@code char} sequences and values can be appended.
 *
 * The characters to be appended should be valid Unicode characters as
 * described in <a href="Character.html#unicode">Unicode Character
 * Representation</a>.  Note that supplementary characters may be composed of
 * multiple 16-bit {@code char} values.
 *
 * Appendables are not necessarily safe for multithreaded access.  Thread
 * safety is the responsibility of classes that extend and implement this
 * interface.
 *
 * Since this interface may be implemented by existing classes
 * with different styles of error handling there is no guarantee that
 * errors will be propagated to the invoker.
 */
public interface Appendable {
    /**
     * Appends the specified character sequence to this {@code Appendable}.
     *
     * Depending on which class implements the character sequence
     * {@code csq}, the entire sequence may not be appended.  For
     * instance, if {@code csq} is a {@link java.nio.CharBuffer} then
     * the subsequence to append is defined by the buffer's position and limit.
     *
     * @param csq
     *         The character sequence to append.  If {@code csq} is
     *         {@code null}, then the four characters {@code "null"} are
     *         appended to this Appendable.
     *
     * @return A reference to this {@code Appendable}
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    Appendable append(CharSequence csq) throws IOException;

    /**
     * Appends a subsequence of the specified character sequence to this
     * {@code Appendable}.
     *
     * An invocation of this method of the form {@code out.append(csq, start, end)}
     * when {@code csq} is not {@code null}, behaves in
     * exactly the same way as the invocation
     *
     * <pre>
     *     out.append(csq.subSequence(start, end)) </pre>
     *
     * @param csq
     *         The character sequence from which a subsequence will be
     *         appended.  If {@code csq} is {@code null}, then characters
     *         will be appended as if {@code csq} contained the four
     *         characters {@code "null"}.
     *
     * @param start
     *         The index of the first character in the subsequence
     *
     * @param end
     *         The index of the character following the last character in the
     *         subsequence
     *
     * @return A reference to this {@code Appendable}
     *
     * @throws IndexOutOfBoundsException
     *          If {@code start} or {@code end} are negative, {@code start}
     *          is greater than {@code end}, or {@code end} is greater than
     *          {@code csq.length()}
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    Appendable append(CharSequence csq, int start, int end) throws IOException;

    /**
     * Appends the specified character to this {@code Appendable}.
     *
     * @param c
     *         The character to append
     *
     * @return A reference to this {@code Appendable}
     *
     * @throws IOException
     *          If an I/O error occurs
     */
    Appendable append(char c) throws IOException;
}

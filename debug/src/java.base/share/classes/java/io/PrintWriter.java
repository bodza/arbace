package java.io;

import java.util.Objects;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;

/**
 * Prints formatted representations of objects to a text-output stream.  This
 * class implements all of the {@code print} methods found in {@link
 * PrintStream}.  It does not contain methods for writing raw bytes, for which
 * a program should use unencoded byte streams.
 *
 * Unlike the {@link PrintStream} class, if automatic flushing is enabled
 * it will be done only when one of the {@code println}, {@code printf}, or
 * {@code format} methods is invoked, rather than whenever a newline character
 * happens to be output.  These methods use the platform's own notion of line
 * separator rather than the newline character.
 *
 * Methods in this class never throw I/O exceptions, although some of its
 * constructors may.  The client may inquire as to whether any errors have
 * occurred by invoking {@link #checkError checkError()}.
 *
 * This class always replaces malformed and unmappable character sequences with
 * the charset's default replacement string.
 * The {@linkplain java.nio.charset.CharsetEncoder} class should be used when more
 * control over the encoding process is required.
 */
public class PrintWriter extends Writer {
    /**
     * The underlying character-output stream of this
     * {@code PrintWriter}.
     */
    protected Writer out;

    private final boolean autoFlush;
    private boolean trouble = false;
    private PrintStream psOut = null;

    /**
     * Returns a charset object for the given charset name.
     * @throws NullPointerException          is csn is null
     * @throws UnsupportedEncodingException  if the charset is not supported
     */
    private static Charset toCharset(String csn) throws UnsupportedEncodingException {
        Objects.requireNonNull(csn, "charsetName");
        try {
            return Charset.forName(csn);
        } catch (IllegalCharsetNameException|UnsupportedCharsetException unused) {
            // UnsupportedEncodingException should be thrown
            throw new UnsupportedEncodingException(csn);
        }
    }

    /**
     * Creates a new PrintWriter, without automatic line flushing.
     *
     * @param out        A character-output stream
     */
    public PrintWriter(Writer out) {
        this(out, false);
    }

    /**
     * Creates a new PrintWriter.
     *
     * @param out        A character-output stream
     * @param autoFlush  A boolean; if true, the {@code println},
     *                    {@code printf}, or {@code format} methods will
     *                    flush the output buffer
     */
    public PrintWriter(Writer out, boolean autoFlush) {
        super(out);
        this.out = out;
        this.autoFlush = autoFlush;
    }

    /**
     * Creates a new PrintWriter, without automatic line flushing, from an
     * existing OutputStream.  This convenience constructor creates the
     * necessary intermediate OutputStreamWriter, which will convert characters
     * into bytes using the default character encoding.
     *
     * @param out        An output stream
     */
    public PrintWriter(OutputStream out) {
        this(out, false);
    }

    /**
     * Creates a new PrintWriter from an existing OutputStream.  This
     * convenience constructor creates the necessary intermediate
     * OutputStreamWriter, which will convert characters into bytes using the
     * default character encoding.
     *
     * @param out        An output stream
     * @param autoFlush  A boolean; if true, the {@code println},
     *                    {@code printf}, or {@code format} methods will
     *                    flush the output buffer
     */
    public PrintWriter(OutputStream out, boolean autoFlush) {
        this(out, autoFlush, Charset.defaultCharset());
    }

    /**
     * Creates a new PrintWriter from an existing OutputStream.  This
     * convenience constructor creates the necessary intermediate
     * OutputStreamWriter, which will convert characters into bytes using the
     * specified charset.
     *
     * @param out        An output stream
     * @param autoFlush  A boolean; if true, the {@code println},
     *                    {@code printf}, or {@code format} methods will
     *                    flush the output buffer
     * @param charset
     *         A {@linkplain java.nio.charset.Charset charset}
     */
    public PrintWriter(OutputStream out, boolean autoFlush, Charset charset) {
        this(new BufferedWriter(new OutputStreamWriter(out, charset)), autoFlush);

        // save print stream for error propagation
        if (out instanceof java.io.PrintStream) {
            psOut = (PrintStream) out;
        }
    }

    /** Checks to make sure that the stream has not been closed */
    private void ensureOpen() throws IOException {
        if (out == null)
            throw new IOException("Stream closed");
    }

    /**
     * Flushes the stream.
     */
    public void flush() {
        try {
            synchronized (lock) {
                ensureOpen();
                out.flush();
            }
        }
        catch (IOException x) {
            trouble = true;
        }
    }

    /**
     * Closes the stream and releases any system resources associated
     * with it. Closing a previously closed stream has no effect.
     */
    public void close() {
        try {
            synchronized (lock) {
                if (out == null)
                    return;
                out.close();
                out = null;
            }
        }
        catch (IOException x) {
            trouble = true;
        }
    }

    /**
     * Flushes the stream if it's not closed and checks its error state.
     *
     * @return {@code true} if the print stream has encountered an error,
     *          either on the underlying output stream or during a format
     *          conversion.
     */
    public boolean checkError() {
        if (out != null) {
            flush();
        }
        if (out instanceof java.io.PrintWriter) {
            PrintWriter pw = (PrintWriter) out;
            return pw.checkError();
        } else if (psOut != null) {
            return psOut.checkError();
        }
        return trouble;
    }

    /**
     * Indicates that an error has occurred.
     *
     * This method will cause subsequent invocations of {@link
     * #checkError()} to return {@code true} until {@link
     * #clearError()} is invoked.
     */
    protected void setError() {
        trouble = true;
    }

    /**
     * Clears the error state of this stream.
     *
     * This method will cause subsequent invocations of {@link
     * #checkError()} to return {@code false} until another write
     * operation fails and invokes {@link #setError()}.
     */
    protected void clearError() {
        trouble = false;
    }

    /*
     * Exception-catching, synchronized output operations,
     * which also implement the write() methods of Writer
     */

    /**
     * Writes a single character.
     * @param c int specifying a character to be written.
     */
    public void write(int c) {
        try {
            synchronized (lock) {
                ensureOpen();
                out.write(c);
            }
        }
        catch (InterruptedIOException x) {
            Thread.currentThread().interrupt();
        }
        catch (IOException x) {
            trouble = true;
        }
    }

    /**
     * Writes A Portion of an array of characters.
     * @param buf Array of characters
     * @param off Offset from which to start writing characters
     * @param len Number of characters to write
     *
     * @throws IndexOutOfBoundsException
     *          If the values of the {@code off} and {@code len} parameters
     *          cause the corresponding method of the underlying {@code Writer}
     *          to throw an {@code IndexOutOfBoundsException}
     */
    public void write(char buf[], int off, int len) {
        try {
            synchronized (lock) {
                ensureOpen();
                out.write(buf, off, len);
            }
        }
        catch (InterruptedIOException x) {
            Thread.currentThread().interrupt();
        }
        catch (IOException x) {
            trouble = true;
        }
    }

    /**
     * Writes an array of characters.  This method cannot be inherited from the
     * Writer class because it must suppress I/O exceptions.
     * @param buf Array of characters to be written
     */
    public void write(char buf[]) {
        write(buf, 0, buf.length);
    }

    /**
     * Writes a portion of a string.
     * @param s A String
     * @param off Offset from which to start writing characters
     * @param len Number of characters to write
     *
     * @throws IndexOutOfBoundsException
     *          If the values of the {@code off} and {@code len} parameters
     *          cause the corresponding method of the underlying {@code Writer}
     *          to throw an {@code IndexOutOfBoundsException}
     */
    public void write(String s, int off, int len) {
        try {
            synchronized (lock) {
                ensureOpen();
                out.write(s, off, len);
            }
        }
        catch (InterruptedIOException x) {
            Thread.currentThread().interrupt();
        }
        catch (IOException x) {
            trouble = true;
        }
    }

    /**
     * Writes a string.  This method cannot be inherited from the Writer class
     * because it must suppress I/O exceptions.
     * @param s String to be written
     */
    public void write(String s) {
        write(s, 0, s.length());
    }

    private void newLine() {
        try {
            synchronized (lock) {
                ensureOpen();
                out.write(System.lineSeparator());
                if (autoFlush)
                    out.flush();
            }
        }
        catch (InterruptedIOException x) {
            Thread.currentThread().interrupt();
        }
        catch (IOException x) {
            trouble = true;
        }
    }

    /* Methods that do not terminate lines */

    /**
     * Prints a boolean value.  The string produced by {@link
     * java.lang.String#valueOf(boolean)} is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the {@link
     * #write(int)} method.
     *
     * @param b   The {@code boolean} to be printed
     */
    public void print(boolean b) {
        write(String.valueOf(b));
    }

    /**
     * Prints a character.  The character is translated into one or more bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the {@link
     * #write(int)} method.
     *
     * @param c   The {@code char} to be printed
     */
    public void print(char c) {
        write(c);
    }

    /**
     * Prints an integer.  The string produced by {@link
     * java.lang.String#valueOf(int)} is translated into bytes according
     * to the platform's default character encoding, and these bytes are
     * written in exactly the manner of the {@link #write(int)}
     * method.
     *
     * @param i   The {@code int} to be printed
     */
    public void print(int i) {
        write(String.valueOf(i));
    }

    /**
     * Prints a long integer.  The string produced by {@link
     * java.lang.String#valueOf(long)} is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the {@link #write(int)}
     * method.
     *
     * @param l   The {@code long} to be printed
     */
    public void print(long l) {
        write(String.valueOf(l));
    }

    /**
     * Prints a floating-point number.  The string produced by {@link
     * java.lang.String#valueOf(float)} is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the {@link #write(int)}
     * method.
     *
     * @param f   The {@code float} to be printed
     */
    public void print(float f) {
        write(String.valueOf(f));
    }

    /**
     * Prints a double-precision floating-point number.  The string produced by
     * {@link java.lang.String#valueOf(double)} is translated into
     * bytes according to the platform's default character encoding, and these
     * bytes are written in exactly the manner of the {@link
     * #write(int)} method.
     *
     * @param d   The {@code double} to be printed
     */
    public void print(double d) {
        write(String.valueOf(d));
    }

    /**
     * Prints an array of characters.  The characters are converted into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the {@link #write(int)}
     * method.
     *
     * @param s   The array of chars to be printed
     *
     * @throws NullPointerException  If {@code s} is {@code null}
     */
    public void print(char s[]) {
        write(s);
    }

    /**
     * Prints a string.  If the argument is {@code null} then the string
     * {@code "null"} is printed.  Otherwise, the string's characters are
     * converted into bytes according to the platform's default character
     * encoding, and these bytes are written in exactly the manner of the
     * {@link #write(int)} method.
     *
     * @param s   The {@code String} to be printed
     */
    public void print(String s) {
        write(String.valueOf(s));
    }

    /**
     * Prints an object.  The string produced by the {@link
     * java.lang.String#valueOf(Object)} method is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the {@link #write(int)}
     * method.
     *
     * @param obj   The {@code Object} to be printed
     */
    public void print(Object obj) {
        write(String.valueOf(obj));
    }

    /* Methods that do terminate lines */

    /**
     * Terminates the current line by writing the line separator string.  The
     * line separator string is defined by the system property
     * {@code line.separator}, and is not necessarily a single newline
     * character ({@code '\n'}).
     */
    public void println() {
        newLine();
    }

    /**
     * Prints a boolean value and then terminates the line.  This method behaves
     * as though it invokes {@link #print(boolean)} and then
     * {@link #println()}.
     *
     * @param x the {@code boolean} value to be printed
     */
    public void println(boolean x) {
        synchronized (lock) {
            print(x);
            println();
        }
    }

    /**
     * Prints a character and then terminates the line.  This method behaves as
     * though it invokes {@link #print(char)} and then {@link
     * #println()}.
     *
     * @param x the {@code char} value to be printed
     */
    public void println(char x) {
        synchronized (lock) {
            print(x);
            println();
        }
    }

    /**
     * Prints an integer and then terminates the line.  This method behaves as
     * though it invokes {@link #print(int)} and then {@link
     * #println()}.
     *
     * @param x the {@code int} value to be printed
     */
    public void println(int x) {
        synchronized (lock) {
            print(x);
            println();
        }
    }

    /**
     * Prints a long integer and then terminates the line.  This method behaves
     * as though it invokes {@link #print(long)} and then
     * {@link #println()}.
     *
     * @param x the {@code long} value to be printed
     */
    public void println(long x) {
        synchronized (lock) {
            print(x);
            println();
        }
    }

    /**
     * Prints a floating-point number and then terminates the line.  This method
     * behaves as though it invokes {@link #print(float)} and then
     * {@link #println()}.
     *
     * @param x the {@code float} value to be printed
     */
    public void println(float x) {
        synchronized (lock) {
            print(x);
            println();
        }
    }

    /**
     * Prints a double-precision floating-point number and then terminates the
     * line.  This method behaves as though it invokes {@link
     * #print(double)} and then {@link #println()}.
     *
     * @param x the {@code double} value to be printed
     */
    public void println(double x) {
        synchronized (lock) {
            print(x);
            println();
        }
    }

    /**
     * Prints an array of characters and then terminates the line.  This method
     * behaves as though it invokes {@link #print(char[])} and then
     * {@link #println()}.
     *
     * @param x the array of {@code char} values to be printed
     */
    public void println(char x[]) {
        synchronized (lock) {
            print(x);
            println();
        }
    }

    /**
     * Prints a String and then terminates the line.  This method behaves as
     * though it invokes {@link #print(String)} and then
     * {@link #println()}.
     *
     * @param x the {@code String} value to be printed
     */
    public void println(String x) {
        synchronized (lock) {
            print(x);
            println();
        }
    }

    /**
     * Prints an Object and then terminates the line.  This method calls
     * at first String.valueOf(x) to get the printed object's string value,
     * then behaves as
     * though it invokes {@link #print(String)} and then
     * {@link #println()}.
     *
     * @param x  The {@code Object} to be printed.
     */
    public void println(Object x) {
        String s = String.valueOf(x);
        synchronized (lock) {
            print(s);
            println();
        }
    }

    /**
     * Appends the specified character sequence to this writer.
     *
     * An invocation of this method of the form {@code out.append(csq)}
     * behaves in exactly the same way as the invocation
     *
     * <pre>{@code
     *     out.write(csq.toString())
     * }</pre>
     *
     * Depending on the specification of {@code toString} for the
     * character sequence {@code csq}, the entire sequence may not be
     * appended. For instance, invoking the {@code toString} method of a
     * character buffer will return a subsequence whose content depends upon
     * the buffer's position and limit.
     *
     * @param csq
     *         The character sequence to append.  If {@code csq} is
     *         {@code null}, then the four characters {@code "null"} are
     *         appended to this writer.
     *
     * @return This writer
     */
    public PrintWriter append(CharSequence csq) {
        write(String.valueOf(csq));
        return this;
    }

    /**
     * Appends a subsequence of the specified character sequence to this writer.
     *
     * An invocation of this method of the form
     * {@code out.append(csq, start, end)}
     * when {@code csq} is not {@code null}, behaves in
     * exactly the same way as the invocation
     *
     * <pre>{@code
     *     out.write(csq.subSequence(start, end).toString())
     * }</pre>
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
     * @return This writer
     *
     * @throws IndexOutOfBoundsException
     *          If {@code start} or {@code end} are negative, {@code start}
     *          is greater than {@code end}, or {@code end} is greater than
     *          {@code csq.length()}
     */
    public PrintWriter append(CharSequence csq, int start, int end) {
        if (csq == null)
            csq = "null";
        return append(csq.subSequence(start, end));
    }

    /**
     * Appends the specified character to this writer.
     *
     * An invocation of this method of the form {@code out.append(c)}
     * behaves in exactly the same way as the invocation
     *
     * <pre>{@code
     *     out.write(c)
     * }</pre>
     *
     * @param c
     *         The 16-bit character to append
     *
     * @return This writer
     */
    public PrintWriter append(char c) {
        write(c);
        return this;
    }
}

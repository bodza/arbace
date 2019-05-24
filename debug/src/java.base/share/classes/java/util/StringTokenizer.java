package java.util;

import java.lang.*;

/**
 * The string tokenizer class allows an application to break a
 * string into tokens. The tokenization method is much simpler than
 * the one used by the {@code StreamTokenizer} class. The
 * {@code StringTokenizer} methods do not distinguish among
 * identifiers, numbers, and quoted strings, nor do they recognize
 * and skip comments.
 *
 * The set of delimiters (the characters that separate tokens) may
 * be specified either at creation time or on a per-token basis.
 *
 * An instance of {@code StringTokenizer} behaves in one of two
 * ways, depending on whether it was created with the
 * {@code returnDelims} flag having the value {@code true}
 * or {@code false}:
 * <ul>
 * <li>If the flag is {@code false}, delimiter characters serve to
 *     separate tokens. A token is a maximal sequence of consecutive
 *     characters that are not delimiters.
 * <li>If the flag is {@code true}, delimiter characters are themselves
 *     considered to be tokens. A token is thus either one delimiter
 *     character, or a maximal sequence of consecutive characters that are
 *     not delimiters.
 * </ul>
 *
 * A {@code StringTokenizer} object internally maintains a current
 * position within the string to be tokenized. Some operations advance this
 * current position past the characters processed.
 *
 * A token is returned by taking a substring of the string that was used to
 * create the {@code StringTokenizer} object.
 *
 * The following is one example of the use of the tokenizer. The code:
 * <blockquote><pre>
 *     StringTokenizer st = new StringTokenizer("this is a test");
 *     while (st.hasMoreTokens()) {
 *         System.out.println(st.nextToken());
 *     }
 * </pre></blockquote>
 *
 * prints the following output:
 * <blockquote><pre>
 *     this
 *     is
 *     a
 *     test
 * </pre></blockquote>
 *
 * {@code StringTokenizer} is a legacy class that is retained for
 * compatibility reasons although its use is discouraged in new code. It is
 * recommended that anyone seeking this functionality use the {@code split}
 * method of {@code String} or the java.util.regex package instead.
 *
 * The following example illustrates how the {@code String.split}
 * method can be used to break up a string into its basic tokens:
 * <blockquote><pre>
 *     String[] result = "this is a test".split("\\s");
 *     for (int x = 0; x&lt;result.length; x++)
 *         System.out.println(result[x]);
 * </pre></blockquote>
 *
 * prints the following output:
 * <blockquote><pre>
 *     this
 *     is
 *     a
 *     test
 * </pre></blockquote>
 */
public class StringTokenizer implements Enumeration<Object> {
    private int currentPosition;
    private int newPosition;
    private int maxPosition;
    private String str;
    private String delimiters;
    private boolean retDelims;
    private boolean delimsChanged;

    /**
     * maxDelimCodePoint stores the value of the delimiter character with the
     * highest value. It is used to optimize the detection of delimiter
     * characters.
     *
     * It is unlikely to provide any optimization benefit in the
     * hasSurrogates case because most string characters will be
     * smaller than the limit, but we keep it so that the two code
     * paths remain similar.
     */
    private int maxDelimCodePoint;

    /**
     * If delimiters include any surrogates (including surrogate
     * pairs), hasSurrogates is true and the tokenizer uses the
     * different code path. This is because String.indexOf(int)
     * doesn't handle unpaired surrogates as a single character.
     */
    private boolean hasSurrogates = false;

    /**
     * When hasSurrogates is true, delimiters are converted to code
     * points and isDelimiter(int) is used to determine if the given
     * codepoint is a delimiter.
     */
    private int[] delimiterCodePoints;

    /**
     * Set maxDelimCodePoint to the highest char in the delimiter set.
     */
    private void setMaxDelimCodePoint() {
        if (delimiters == null) {
            maxDelimCodePoint = 0;
            return;
        }

        int m = 0;
        int c;
        int count = 0;
        for (int i = 0; i < delimiters.length(); i += Character.charCount(c)) {
            c = delimiters.charAt(i);
            if (c >= Character.MIN_HIGH_SURROGATE && c <= Character.MAX_LOW_SURROGATE) {
                c = delimiters.codePointAt(i);
                hasSurrogates = true;
            }
            if (m < c)
                m = c;
            count++;
        }
        maxDelimCodePoint = m;

        if (hasSurrogates) {
            delimiterCodePoints = new int[count];
            for (int i = 0, j = 0; i < count; i++, j += Character.charCount(c)) {
                c = delimiters.codePointAt(j);
                delimiterCodePoints[i] = c;
            }
        }
    }

    /**
     * Constructs a string tokenizer for the specified string. All
     * characters in the {@code delim} argument are the delimiters
     * for separating tokens.
     *
     * If the {@code returnDelims} flag is {@code true}, then
     * the delimiter characters are also returned as tokens. Each
     * delimiter is returned as a string of length one. If the flag is
     * {@code false}, the delimiter characters are skipped and only
     * serve as separators between tokens.
     *
     * Note that if {@code delim} is {@code null}, this constructor does
     * not throw an exception. However, trying to invoke other methods on the
     * resulting {@code StringTokenizer} may result in a
     * {@code NullPointerException}.
     *
     * @param str            a string to be parsed.
     * @param delim          the delimiters.
     * @param returnDelims   flag indicating whether to return the delimiters
     *                         as tokens.
     * @throws NullPointerException if str is {@code null}
     */
    public StringTokenizer(String str, String delim, boolean returnDelims) {
        currentPosition = 0;
        newPosition = -1;
        delimsChanged = false;
        this.str = str;
        maxPosition = str.length();
        delimiters = delim;
        retDelims = returnDelims;
        setMaxDelimCodePoint();
    }

    /**
     * Constructs a string tokenizer for the specified string. The
     * characters in the {@code delim} argument are the delimiters
     * for separating tokens. Delimiter characters themselves will not
     * be treated as tokens.
     *
     * Note that if {@code delim} is {@code null}, this constructor does
     * not throw an exception. However, trying to invoke other methods on the
     * resulting {@code StringTokenizer} may result in a
     * {@code NullPointerException}.
     *
     * @param str     a string to be parsed.
     * @param delim   the delimiters.
     * @throws NullPointerException if str is {@code null}
     */
    public StringTokenizer(String str, String delim) {
        this(str, delim, false);
    }

    /**
     * Constructs a string tokenizer for the specified string. The
     * tokenizer uses the default delimiter set, which is
     * <code>"&nbsp;&#92;t&#92;n&#92;r&#92;f"</code>: the space character,
     * the tab character, the newline character, the carriage-return character,
     * and the form-feed character. Delimiter characters themselves will
     * not be treated as tokens.
     *
     * @param str   a string to be parsed.
     * @throws NullPointerException if str is {@code null}
     */
    public StringTokenizer(String str) {
        this(str, " \t\n\r\f", false);
    }

    /**
     * Skips delimiters starting from the specified position. If retDelims
     * is false, returns the index of the first non-delimiter character at or
     * after startPos. If retDelims is true, startPos is returned.
     */
    private int skipDelimiters(int startPos) {
        if (delimiters == null)
            throw new NullPointerException();

        int position = startPos;
        while (!retDelims && position < maxPosition) {
            if (!hasSurrogates) {
                char c = str.charAt(position);
                if ((c > maxDelimCodePoint) || (delimiters.indexOf(c) < 0))
                    break;
                position++;
            } else {
                int c = str.codePointAt(position);
                if ((c > maxDelimCodePoint) || !isDelimiter(c)) {
                    break;
                }
                position += Character.charCount(c);
            }
        }
        return position;
    }

    /**
     * Skips ahead from startPos and returns the index of the next delimiter
     * character encountered, or maxPosition if no such delimiter is found.
     */
    private int scanToken(int startPos) {
        int position = startPos;
        while (position < maxPosition) {
            if (!hasSurrogates) {
                char c = str.charAt(position);
                if ((c <= maxDelimCodePoint) && (delimiters.indexOf(c) >= 0))
                    break;
                position++;
            } else {
                int c = str.codePointAt(position);
                if ((c <= maxDelimCodePoint) && isDelimiter(c))
                    break;
                position += Character.charCount(c);
            }
        }
        if (retDelims && (startPos == position)) {
            if (!hasSurrogates) {
                char c = str.charAt(position);
                if ((c <= maxDelimCodePoint) && (delimiters.indexOf(c) >= 0))
                    position++;
            } else {
                int c = str.codePointAt(position);
                if ((c <= maxDelimCodePoint) && isDelimiter(c))
                    position += Character.charCount(c);
            }
        }
        return position;
    }

    private boolean isDelimiter(int codePoint) {
        for (int delimiterCodePoint : delimiterCodePoints) {
            if (delimiterCodePoint == codePoint) {
                return true;
            }
        }
        return false;
    }

    /**
     * Tests if there are more tokens available from this tokenizer's string.
     * If this method returns {@code true}, then a subsequent call to
     * {@code nextToken} with no argument will successfully return a token.
     *
     * @return {@code true} if and only if there is at least one token
     *          in the string after the current position; {@code false}
     *          otherwise.
     */
    public boolean hasMoreTokens() {
        /*
         * Temporarily store this position and use it in the following
         * nextToken() method only if the delimiters haven't been changed in
         * that nextToken() invocation.
         */
        newPosition = skipDelimiters(currentPosition);
        return (newPosition < maxPosition);
    }

    /**
     * Returns the next token from this string tokenizer.
     *
     * @return the next token from this string tokenizer.
     * @throws NoSuchElementException  if there are no more tokens in this
     *               tokenizer's string.
     */
    public String nextToken() {
        /*
         * If next position already computed in hasMoreElements() and
         * delimiters have changed between the computation and this invocation,
         * then use the computed value.
         */

        currentPosition = (newPosition >= 0 && !delimsChanged) ? newPosition : skipDelimiters(currentPosition);

        /* Reset these anyway */
        delimsChanged = false;
        newPosition = -1;

        if (currentPosition >= maxPosition)
            throw new NoSuchElementException();
        int start = currentPosition;
        currentPosition = scanToken(currentPosition);
        return str.substring(start, currentPosition);
    }

    /**
     * Returns the next token in this string tokenizer's string. First,
     * the set of characters considered to be delimiters by this
     * {@code StringTokenizer} object is changed to be the characters in
     * the string {@code delim}. Then the next token in the string
     * after the current position is returned. The current position is
     * advanced beyond the recognized token.  The new delimiter set
     * remains the default after this call.
     *
     * @param delim   the new delimiters.
     * @return the next token, after switching to the new delimiter set.
     * @throws NoSuchElementException  if there are no more tokens in this
     *               tokenizer's string.
     * @throws NullPointerException if delim is {@code null}
     */
    public String nextToken(String delim) {
        delimiters = delim;

        /* delimiter string specified, so set the appropriate flag. */
        delimsChanged = true;

        setMaxDelimCodePoint();
        return nextToken();
    }

    /**
     * Returns the same value as the {@code hasMoreTokens}
     * method. It exists so that this class can implement the
     * {@code Enumeration} interface.
     *
     * @return {@code true} if there are more tokens;
     *          {@code false} otherwise.
     */
    public boolean hasMoreElements() {
        return hasMoreTokens();
    }

    /**
     * Returns the same value as the {@code nextToken} method,
     * except that its declared return value is {@code Object} rather than
     * {@code String}. It exists so that this class can implement the
     * {@code Enumeration} interface.
     *
     * @return the next token in the string.
     * @throws NoSuchElementException  if there are no more tokens in this
     *               tokenizer's string.
     */
    public Object nextElement() {
        return nextToken();
    }

    /**
     * Calculates the number of times that this tokenizer's
     * {@code nextToken} method can be called before it generates an
     * exception. The current position is not advanced.
     *
     * @return the number of tokens remaining in the string using the current
     *          delimiter set.
     */
    public int countTokens() {
        int count = 0;
        int currpos = currentPosition;
        while (currpos < maxPosition) {
            currpos = skipDelimiters(currpos);
            if (currpos >= maxPosition)
                break;
            currpos = scanToken(currpos);
            count++;
        }
        return count;
    }
}

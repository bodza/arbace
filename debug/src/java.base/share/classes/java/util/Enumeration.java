package java.util;

/**
 * An object that implements the Enumeration interface generates a
 * series of elements, one at a time. Successive calls to the
 * {@code nextElement} method return successive elements of the
 * series.
 *
 * For example, to print all elements of a {@code Vector<E>} <i>v</i>:
 * <pre>
 *   for (Enumeration&lt;E&gt; e = v.elements(); e.hasMoreElements(); )
 *       System.out.println(e.nextElement());</pre>
 *
 * Methods are provided to enumerate through the elements of a
 * vector, the keys of a hashtable, and the values in a hashtable.
 * Enumerations are also used to specify the input streams to a
 * {@code SequenceInputStream}.
 *
 * @apiNote
 * The functionality of this interface is duplicated by the {@link Iterator}
 * interface.  In addition, {@code Iterator} adds an optional remove operation,
 * and has shorter method names.  New implementations should consider using
 * {@code Iterator} in preference to {@code Enumeration}. It is possible to
 * adapt an {@code Enumeration} to an {@code Iterator} by using the
 * {@link #asIterator} method.
 */
public interface Enumeration<E> {
    /**
     * Tests if this enumeration contains more elements.
     *
     * @return {@code true} if and only if this enumeration object
     *           contains at least one more element to provide;
     *          {@code false} otherwise.
     */
    boolean hasMoreElements();

    /**
     * Returns the next element of this enumeration if this enumeration
     * object has at least one more element to provide.
     *
     * @return the next element of this enumeration.
     * @throws NoSuchElementException  if no more elements exist.
     */
    E nextElement();

    /**
     * Returns an {@link Iterator} that traverses the remaining elements
     * covered by this enumeration. Traversal is undefined if any methods
     * are called on this enumeration after the call to {@code asIterator}.
     *
     * @apiNote
     * This method is intended to help adapt code that produces
     * {@code Enumeration} instances to code that consumes {@code Iterator}
     * instances.
     *
     * @implSpec
     * The default implementation returns an {@code Iterator} whose
     * {@link Iterator#hasNext hasNext} method calls this Enumeration's
     * {@code hasMoreElements} method, whose {@link Iterator#next next}
     * method calls this Enumeration's {@code nextElement} method, and
     * whose {@link Iterator#remove remove} method throws
     * {@code UnsupportedOperationException}.
     *
     * @return an Iterator representing the remaining elements of this Enumeration
     */
    default Iterator<E> asIterator() {
        return new Iterator<>() {
            @Override public boolean hasNext() {
                return hasMoreElements();
            }

            @Override public E next() {
                return nextElement();
            }
        };
    }
}

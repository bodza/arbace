package java.util;

/**
 * This class provides a skeletal implementation of the {@code Set}
 * interface to minimize the effort required to implement this
 * interface.
 *
 * The process of implementing a set by extending this class is identical
 * to that of implementing a Collection by extending AbstractCollection,
 * except that all of the methods and constructors in subclasses of this
 * class must obey the additional constraints imposed by the {@code Set}
 * interface (for instance, the add method must not permit addition of
 * multiple instances of an object to a set).
 *
 * Note that this class does not override any of the implementations from
 * the {@code AbstractCollection} class.  It merely adds implementations
 * for {@code equals} and {@code hashCode}.
 *
 * This class is a member of the
 * <a href="{@docRoot}/java.base/java/util/package-summary.html#CollectionsFramework">
 * Java Collections Framework</a>.
 *
 * @param <E> the type of elements maintained by this set
 */
public abstract class AbstractSet<E> extends AbstractCollection<E> implements Set<E> {
    /**
     * Sole constructor.  (For invocation by subclass constructors, typically
     * implicit.)
     */
    protected AbstractSet() {
    }

    // Comparison and hashing

    /**
     * Compares the specified object with this set for equality.  Returns
     * {@code true} if the given object is also a set, the two sets have
     * the same size, and every member of the given set is contained in
     * this set.  This ensures that the {@code equals} method works
     * properly across different implementations of the {@code Set}
     * interface.
     *
     * This implementation first checks if the specified object is this
     * set; if so it returns {@code true}.  Then, it checks if the
     * specified object is a set whose size is identical to the size of
     * this set; if not, it returns false.  If so, it returns
     * {@code containsAll((Collection) o)}.
     *
     * @param o object to be compared for equality with this set
     * @return {@code true} if the specified object is equal to this set
     */
    public boolean equals(Object o) {
        if (o == this)
            return true;

        if (!(o instanceof Set))
            return false;
        Collection<?> c = (Collection<?>) o;
        if (c.size() != size())
            return false;
        try {
            return containsAll(c);
        } catch (ClassCastException | NullPointerException unused) {
            return false;
        }
    }

    /**
     * Returns the hash code value for this set.  The hash code of a set is
     * defined to be the sum of the hash codes of the elements in the set,
     * where the hash code of a {@code null} element is defined to be zero.
     * This ensures that {@code s1.equals(s2)} implies that
     * {@code s1.hashCode()==s2.hashCode()} for any two sets {@code s1}
     * and {@code s2}, as required by the general contract of
     * {@link Object#hashCode}.
     *
     * This implementation iterates over the set, calling the
     * {@code hashCode} method on each element in the set, and adding up
     * the results.
     *
     * @return the hash code value for this set
     */
    public int hashCode() {
        int h = 0;
        Iterator<E> i = iterator();
        while (i.hasNext()) {
            E obj = i.next();
            if (obj != null)
                h += obj.hashCode();
        }
        return h;
    }
}

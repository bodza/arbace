package jdk.internal.org.objectweb.asm.tree.analysis;

import java.util.AbstractSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * A set of at most two elements.
 */
class SmallSet<E> extends AbstractSet<E> implements Iterator<E> {
    // if e1 is null, e2 must be null; otherwise e2 must be different from e1

    E e1, e2;

    static final <T> Set<T> emptySet() {
        return new SmallSet<T>(null, null);
    }

    SmallSet(final E e1, final E e2) {
        this.e1 = e1;
        this.e2 = e2;
    }

    // Implementation of inherited abstract methods

    @Override
    public Iterator<E> iterator() {
        return new SmallSet<E>(e1, e2);
    }

    @Override
    public int size() {
        return e1 == null ? 0 : (e2 == null ? 1 : 2);
    }

    // Implementation of the Iterator interface

    public boolean hasNext() {
        return e1 != null;
    }

    public E next() {
        if (e1 == null) {
            throw new NoSuchElementException();
        }
        E e = e1;
        e1 = e2;
        e2 = null;
        return e;
    }

    public void remove() {
    }

    // Utility methods

    Set<E> union(final SmallSet<E> s) {
        if ((s.e1 == e1 && s.e2 == e2) || (s.e1 == e2 && s.e2 == e1)) {
            return this; // if the two sets are equal, return this
        }
        if (s.e1 == null) {
            return this; // if s is empty, return this
        }
        if (e1 == null) {
            return s; // if this is empty, return s
        }
        if (s.e2 == null) { // s contains exactly one element
            if (e2 == null) {
                return new SmallSet<E>(e1, s.e1); // necessarily e1 != s.e1
            } else if (s.e1 == e1 || s.e1 == e2) { // s is included in this
                return this;
            }
        }
        if (e2 == null) { // this contains exactly one element
            // if (s.e2 == null) { // cannot happen
            // return new SmallSet(e1, s.e1); // necessarily e1 != s.e1
            // } else
            if (e1 == s.e1 || e1 == s.e2) { // this in included in s
                return s;
            }
        }
        // here we know that there are at least 3 distinct elements
        HashSet<E> r = new HashSet<E>(4);
        r.add(e1);
        if (e2 != null) {
            r.add(e2);
        }
        r.add(s.e1);
        if (s.e2 != null) {
            r.add(s.e2);
        }
        return r;
    }
}

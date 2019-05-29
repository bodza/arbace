package sun.util;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.NoSuchElementException;

/**
 * A precomputed hash map.
 *
 * Subclasses of this class are of the following form:
 *
 * <blockquote><pre>
 * class FooMap extends sun.util.PreHashedMap&lt;String&gt;
 * {
 *     private FooMap() {
 *         super(ROWS, SIZE, SHIFT, MASK);
 *     }
 *
 *     protected void init(Object[] ht) {
 *         ht[0] = new Object[] { "key-1", value_1 };
 *         ht[1] = new Object[] { "key-2", value_2, new Object { "key-3", value_3 } };
 *         ...
 *     }
 * }
 * </pre></blockquote>
 *
 * The {@code init} method is invoked by the {@code PreHashedMap}
 * constructor with an object array long enough for the map's rows.  The method
 * must construct the hash chain for each row and store it in the appropriate
 * element of the array.
 *
 * Each entry in the map is represented by a unique hash-chain node.  The
 * final node of a hash chain is a two-element object array whose first element
 * is the entry's key and whose second element is the entry's value.  A
 * non-final node of a hash chain is a three-element object array whose first
 * two elements are the entry's key and value and whose third element is the
 * next node in the chain.
 *
 * Instances of this class are mutable and are not safe for concurrent
 * access.  They may be made immutable and thread-safe via the appropriate
 * methods in the {@link java.util.Collections} utility class.
 *
 * In the JDK build, subclasses of this class are typically created via the
 * {@code Hasher} program in the {@code make/tools/Hasher} directory.
 */
public abstract class PreHashedMap<V> extends AbstractMap<String, V> {
    private final int rows;
    private final int size;
    private final int shift;
    private final int mask;
    private final Object[] ht;

    /**
     * Creates a new map.
     *
     * This constructor invokes the {@link #init init} method, passing it a
     * newly-constructed row array that is {@code rows} elements long.
     *
     * @param rows
     *        The number of rows in the map
     * @param size
     *        The number of entries in the map
     * @param shift
     *        The value by which hash codes are right-shifted
     * @param mask
     *        The value with which hash codes are masked after being shifted
     */
    protected PreHashedMap(int rows, int size, int shift, int mask) {
        this.rows = rows;
        this.size = size;
        this.shift = shift;
        this.mask = mask;
        this.ht = new Object[rows];
        init(ht);
    }

    /**
     * Initializes this map.
     *
     * This method must construct the map's hash chains and store them into
     * the appropriate elements of the given hash-table row array.
     *
     * @param ht The row array to be initialized
     */
    protected abstract void init(Object[] ht);

    // @SuppressWarnings("unchecked")
    private V toV(Object x) {
        return (V)x;
    }

    public V get(Object k) {
        int h = (k.hashCode() >> shift) & mask;
        Object[] a = (Object[])ht[h];
        if (a == null)
            return null;
        for (;;) {
            if (a[0].equals(k))
                return toV(a[1]);
            if (a.length < 3)
                return null;
            a = (Object[])a[2];
        }
    }

    /**
     * @throws UnsupportedOperationException
     *         If the given key is not part of this map's initial key set
     */
    public V put(String k, V v) {
        int h = (k.hashCode() >> shift) & mask;
        Object[] a = (Object[])ht[h];
        if (a == null)
            throw new UnsupportedOperationException(k);
        for (;;) {
            if (a[0].equals(k)) {
                V ov = toV(a[1]);
                a[1] = v;
                return ov;
            }
            if (a.length < 3)
                throw new UnsupportedOperationException(k);
            a = (Object[])a[2];
        }
    }

    public Set<String> keySet() {
        return new AbstractSet<>() {
            public int size() {
                return size;
            }

            public Iterator<String> iterator() {
                return new Iterator<>() {
                    private int i = -1;
                    Object[] a = null;
                    String cur = null;

                    private boolean findNext() {
                        if (a != null) {
                            if (a.length == 3) {
                                a = (Object[])a[2];
                                cur = (String)a[0];
                                return true;
                            }
                            i++;
                            a = null;
                        }
                        cur = null;
                        if (i >= rows)
                            return false;
                        if (i < 0 || ht[i] == null) {
                            do {
                                if (++i >= rows)
                                    return false;
                            } while (ht[i] == null);
                        }
                        a = (Object[])ht[i];
                        cur = (String)a[0];
                        return true;
                    }

                    public boolean hasNext() {
                        if (cur != null)
                            return true;
                        return findNext();
                    }

                    public String next() {
                        if (cur == null) {
                            if (!findNext())
                                throw new NoSuchElementException();
                        }
                        String s = cur;
                        cur = null;
                        return s;
                    }

                    public void remove() {
                        throw new UnsupportedOperationException();
                    }
                };
            }
        };
    }

    public Set<Map.Entry<String, V>> entrySet() {
        return new AbstractSet<Map.Entry<String, V>>() {
            public int size() {
                return size;
            }

            public Iterator<Map.Entry<String, V>> iterator() {
                return new Iterator<Map.Entry<String, V>>() {
                    final Iterator<String> i = keySet().iterator();

                    public boolean hasNext() {
                        return i.hasNext();
                    }

                    public Map.Entry<String, V> next() {
                        return new Map.Entry<String, V>() {
                            String k = i.next();
                            public String getKey() { return k; }
                            public V getValue() { return get(k); }
                            public int hashCode() {
                                V v = get(k);
                                return (k.hashCode() + (v == null ? 0 : v.hashCode()));
                            }
                            public boolean equals(Object ob) {
                                if (ob == this)
                                    return true;
                                if (!(ob instanceof Map.Entry))
                                    return false;
                                Map.Entry<?,?> that = (Map.Entry<?,?>)ob;
                                return ((this.getKey() == null ? that.getKey() == null : this.getKey().equals(that.getKey()))
                                     && (this.getValue() == null ? that.getValue() == null : this.getValue().equals(that.getValue())));
                            }
                            public V setValue(V v) {
                                throw new UnsupportedOperationException();
                            }
                        };
                    }

                    public void remove() {
                        throw new UnsupportedOperationException();
                    }
                };
            }
        };
    }
}

package java.util;

import java.util.function.Consumer;
import java.util.function.DoubleConsumer;
import java.util.function.IntConsumer;
import java.util.function.LongConsumer;

/**
 * A base type for primitive specializations of {@code Iterator}.  Specialized
 * subtypes are provided for {@link OfInt int}, {@link OfLong long}, and
 * {@link OfDouble double} values.
 *
 * The specialized subtype default implementations of {@link Iterator#next} box
 * primitive values to instances of their corresponding wrapper class.  Such
 * boxing may offset any advantages gained when using the primitive
 * specializations.  To avoid boxing, the corresponding primitive-based methods
 * should be used.
 *
 * Iteration of primitive values using boxing-based method {@link Iterator#next next()}
 * does not affect the order in which the values, transformed to boxed values,
 * are encountered.
 *
 * @param <T> the type of elements returned by this PrimitiveIterator.  The
 *        type must be a wrapper type for a primitive type, such as
 *        {@code Integer} for the primitive {@code int} type.
 * @param <T_CONS> the type of primitive consumer.  The type must be a
 *        primitive specialization of {@link java.util.function.Consumer} for
 *        {@code T}, such as {@link java.util.function.IntConsumer} for
 *        {@code Integer}.
 */
public interface PrimitiveIterator<T, T_CONS> extends Iterator<T> {
    /**
     * An Iterator specialized for {@code int} values.
     */
    public static interface OfInt extends PrimitiveIterator<Integer, IntConsumer> {
        /**
         * Returns the next {@code int} element in the iteration.
         *
         * @return the next {@code int} element in the iteration
         * @throws NoSuchElementException if the iteration has no more elements
         */
        int nextInt();

        /**
         * {@inheritDoc}
         * @implSpec
         * The default implementation boxes the result of calling
         * {@link #nextInt()}, and returns that boxed result.
         */
        // @Override
        default Integer next() {
            return nextInt();
        }
    }

    /**
     * An Iterator specialized for {@code long} values.
     */
    public static interface OfLong extends PrimitiveIterator<Long, LongConsumer> {
        /**
         * Returns the next {@code long} element in the iteration.
         *
         * @return the next {@code long} element in the iteration
         * @throws NoSuchElementException if the iteration has no more elements
         */
        long nextLong();

        /**
         * {@inheritDoc}
         * @implSpec
         * The default implementation boxes the result of calling
         * {@link #nextLong()}, and returns that boxed result.
         */
        // @Override
        default Long next() {
            return nextLong();
        }
    }

    /**
     * An Iterator specialized for {@code double} values.
     */
    public static interface OfDouble extends PrimitiveIterator<Double, DoubleConsumer> {
        /**
         * Returns the next {@code double} element in the iteration.
         *
         * @return the next {@code double} element in the iteration
         * @throws NoSuchElementException if the iteration has no more elements
         */
        double nextDouble();

        /**
         * {@inheritDoc}
         * @implSpec
         * The default implementation boxes the result of calling
         * {@link #nextDouble()}, and returns that boxed result.
         */
        // @Override
        default Double next() {
            return nextDouble();
        }
    }
}

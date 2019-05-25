package java.util;

import java.util.function.IntConsumer;

/**
 * A state object for collecting statistics such as count, min, max, sum, and
 * average.
 *
 * This class is designed to work with (though does not require)
 * {@linkplain java.util.stream streams}. For example, you can compute
 * summary statistics on a stream of ints with:
 * <pre> {@code
 * IntSummaryStatistics stats = intStream.collect(IntSummaryStatistics::new, IntSummaryStatistics::accept, IntSummaryStatistics::combine);
 * }</pre>
 *
 * {@code IntSummaryStatistics} can be used as a
 * {@linkplain java.util.stream.Stream#collect(Collector) reduction}
 * target for a {@linkplain java.util.stream.Stream stream}. For example:
 *
 * <pre> {@code
 * IntSummaryStatistics stats = people.stream().collect(Collectors.summarizingInt(Person::getDependents));
 *}</pre>
 *
 * This computes, in a single pass, the count of people, as well as the minimum,
 * maximum, sum, and average of their number of dependents.
 *
 * @implNote This implementation is not thread safe. However, it is safe to use
 * {@link java.util.stream.Collectors#summarizingInt(java.util.function.ToIntFunction)
 * Collectors.summarizingInt()} on a parallel stream, because the parallel
 * implementation of {@link java.util.stream.Stream#collect Stream.collect()}
 * provides the necessary partitioning, isolation, and merging of results for
 * safe and efficient parallel execution.
 *
 * This implementation does not check for overflow of the sum.
 */
public class IntSummaryStatistics implements IntConsumer {
    private long count;
    private long sum;
    private int min = Integer.MAX_VALUE;
    private int max = Integer.MIN_VALUE;

    /**
     * Constructs an empty instance with zero count, zero sum,
     * {@code Integer.MAX_VALUE} min, {@code Integer.MIN_VALUE} max and zero
     * average.
     */
    public IntSummaryStatistics() { }

    /**
     * Constructs a non-empty instance with the specified {@code count},
     * {@code min}, {@code max}, and {@code sum}.
     *
     * If {@code count} is zero then the remaining arguments are ignored and
     * an empty instance is constructed.
     *
     * If the arguments are inconsistent then an {@code IllegalArgumentException}
     * is thrown.  The necessary consistent argument conditions are:
     * <ul>
     *   <li>{@code count >= 0}</li>
     *   <li>{@code min <= max}</li>
     * </ul>
     * @apiNote
     * The enforcement of argument correctness means that the retrieved set of
     * recorded values obtained from a {@code IntSummaryStatistics} source
     * instance may not be a legal set of arguments for this constructor due to
     * arithmetic overflow of the source's recorded count of values.
     * The consistent argument conditions are not sufficient to prevent the
     * creation of an internally inconsistent instance.  An example of such a
     * state would be an instance with: {@code count} = 2, {@code min} = 1,
     * {@code max} = 2, and {@code sum} = 0.
     *
     * @param count the count of values
     * @param min the minimum value
     * @param max the maximum value
     * @param sum the sum of all values
     * @throws IllegalArgumentException if the arguments are inconsistent
     */
    public IntSummaryStatistics(long count, int min, int max, long sum) throws IllegalArgumentException {
        if (count < 0L) {
            throw new IllegalArgumentException("Negative count value");
        } else if (count > 0L) {
            if (min > max)
                throw new IllegalArgumentException("Minimum greater than maximum");

            this.count = count;
            this.sum = sum;
            this.min = min;
            this.max = max;
        }
        // Use default field values if count == 0
    }

    /**
     * Records a new value into the summary information
     *
     * @param value the input value
     */
    @Override
    public void accept(int value) {
        ++count;
        sum += value;
        min = Math.min(min, value);
        max = Math.max(max, value);
    }

    /**
     * Combines the state of another {@code IntSummaryStatistics} into this one.
     *
     * @param other another {@code IntSummaryStatistics}
     * @throws NullPointerException if {@code other} is null
     */
    public void combine(IntSummaryStatistics other) {
        count += other.count;
        sum += other.sum;
        min = Math.min(min, other.min);
        max = Math.max(max, other.max);
    }

    /**
     * Returns the count of values recorded.
     *
     * @return the count of values
     */
    public final long getCount() {
        return count;
    }

    /**
     * Returns the sum of values recorded, or zero if no values have been
     * recorded.
     *
     * @return the sum of values, or zero if none
     */
    public final long getSum() {
        return sum;
    }

    /**
     * Returns the minimum value recorded, or {@code Integer.MAX_VALUE} if no
     * values have been recorded.
     *
     * @return the minimum value, or {@code Integer.MAX_VALUE} if none
     */
    public final int getMin() {
        return min;
    }

    /**
     * Returns the maximum value recorded, or {@code Integer.MIN_VALUE} if no
     * values have been recorded.
     *
     * @return the maximum value, or {@code Integer.MIN_VALUE} if none
     */
    public final int getMax() {
        return max;
    }

    /**
     * Returns the arithmetic mean of values recorded, or zero if no values have been
     * recorded.
     *
     * @return the arithmetic mean of values, or zero if none
     */
    public final double getAverage() {
        return getCount() > 0 ? (double) getSum() / getCount() : 0.0d;
    }

    /**
     * Returns a non-empty string representation of this object suitable for
     * debugging. The exact presentation format is unspecified and may vary
     * between implementations and versions.
     */
    @Override
    public String toString() {
        return this.getClass().getSimpleName() + "{count=" + getCount() + ", sum=" + getSum() + ", min=" + getMin() + ", average=" + getAverage() + ", max=" + getMax() + "}";
    }
}

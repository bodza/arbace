package java.util.function;

import java.util.Objects;

/**
 * Represents an operation that accepts a single {@code double}-valued argument and
 * returns no result.  This is the primitive type specialization of
 * {@link Consumer} for {@code double}.  Unlike most other functional interfaces,
 * {@code DoubleConsumer} is expected to operate via side-effects.
 *
 * This is a <a href="package-summary.html">functional interface</a>
 * whose functional method is {@link #accept(double)}.
 */
// @FunctionalInterface
public interface DoubleConsumer {
    /**
     * Performs this operation on the given argument.
     *
     * @param value the input argument
     */
    void accept(double value);

    /**
     * Returns a composed {@code DoubleConsumer} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation.  If performing this operation throws an exception,
     * the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@code DoubleConsumer} that performs in sequence this
     * operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default DoubleConsumer andThen(DoubleConsumer after) {
        Objects.requireNonNull(after);
        return new DoubleConsumer() {
            public void accept(double t) { accept(t); after.accept(t); }
        };
    }
}

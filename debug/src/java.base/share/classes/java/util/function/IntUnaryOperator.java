package java.util.function;

import java.util.Objects;

/**
 * Represents an operation on a single {@code int}-valued operand that produces
 * an {@code int}-valued result.  This is the primitive type specialization of
 * {@link UnaryOperator} for {@code int}.
 *
 * This is a <a href="package-summary.html">functional interface</a>
 * whose functional method is {@link #applyAsInt(int)}.
 */
// @FunctionalInterface
public interface IntUnaryOperator {
    /**
     * Applies this operator to the given operand.
     *
     * @param operand the operand
     * @return the operator result
     */
    int applyAsInt(int operand);

    /**
     * Returns a composed operator that first applies the {@code before}
     * operator to its input, and then applies this operator to the result.
     * If evaluation of either operator throws an exception, it is relayed to
     * the caller of the composed operator.
     *
     * @param before the operator to apply before this operator is applied
     * @return a composed operator that first applies the {@code before}
     * operator and then applies this operator
     * @throws NullPointerException if before is null
     */
    default IntUnaryOperator compose(IntUnaryOperator before) {
        Objects.requireNonNull(before);
        return new IntUnaryOperator() {
            public int applyAsInt(int v) { return applyAsInt(before.applyAsInt(v)); }
        };
    }

    /**
     * Returns a composed operator that first applies this operator to
     * its input, and then applies the {@code after} operator to the result.
     * If evaluation of either operator throws an exception, it is relayed to
     * the caller of the composed operator.
     *
     * @param after the operator to apply after this operator is applied
     * @return a composed operator that first applies this operator and then
     * applies the {@code after} operator
     * @throws NullPointerException if after is null
     */
    default IntUnaryOperator andThen(IntUnaryOperator after) {
        Objects.requireNonNull(after);
        return new IntUnaryOperator() {
            public int applyAsInt(int t) { return after.applyAsInt(applyAsInt(t)); }
        };
    }

    /**
     * Returns a unary operator that always returns its input argument.
     *
     * @return a unary operator that always returns its input argument
     */
    static IntUnaryOperator identity() {
        return new IntUnaryOperator() {
            public int applyAsInt(int t) { return t; }
        };
    }
}

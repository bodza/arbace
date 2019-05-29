package java.util.function;

import java.util.Objects;

/**
 * Represents an operation on a single {@code double}-valued operand that produces
 * a {@code double}-valued result.  This is the primitive type specialization of
 * {@link UnaryOperator} for {@code double}.
 *
 * This is a <a href="package-summary.html">functional interface</a>
 * whose functional method is {@link #applyAsDouble(double)}.
 */
// @FunctionalInterface
public interface DoubleUnaryOperator {
    /**
     * Applies this operator to the given operand.
     *
     * @param operand the operand
     * @return the operator result
     */
    double applyAsDouble(double operand);

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
    default DoubleUnaryOperator compose(DoubleUnaryOperator before) {
        Objects.requireNonNull(before);
        return new DoubleUnaryOperator() {
            public double applyAsDouble(double v) { return applyAsDouble(before.applyAsDouble(v)); }
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
    default DoubleUnaryOperator andThen(DoubleUnaryOperator after) {
        Objects.requireNonNull(after);
        return new DoubleUnaryOperator() {
            public double applyAsDouble(double t) { return after.applyAsDouble(applyAsDouble(t)); }
        };
    }

    /**
     * Returns a unary operator that always returns its input argument.
     *
     * @return a unary operator that always returns its input argument
     */
    static DoubleUnaryOperator identity() {
        return new DoubleUnaryOperator() {
            public double applyAsDouble(double t) { return t; }
        };
    }
}

package java.util.function;

/**
 * Represents a supplier of {@code double}-valued results.  This is the
 * {@code double}-producing primitive specialization of {@link Supplier}.
 *
 * There is no requirement that a distinct result be returned each
 * time the supplier is invoked.
 *
 * This is a <a href="package-summary.html">functional interface</a>
 * whose functional method is {@link #getAsDouble()}.
 */
@FunctionalInterface
public interface DoubleSupplier {
    /**
     * Gets a result.
     *
     * @return a result
     */
    double getAsDouble();
}

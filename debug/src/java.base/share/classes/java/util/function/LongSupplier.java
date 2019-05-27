package java.util.function;

/**
 * Represents a supplier of {@code long}-valued results.  This is the
 * {@code long}-producing primitive specialization of {@link Supplier}.
 *
 * There is no requirement that a distinct result be returned each
 * time the supplier is invoked.
 *
 * This is a <a href="package-summary.html">functional interface</a>
 * whose functional method is {@link #getAsLong()}.
 */
// @FunctionalInterface
public interface LongSupplier {
    /**
     * Gets a result.
     *
     * @return a result
     */
    long getAsLong();
}

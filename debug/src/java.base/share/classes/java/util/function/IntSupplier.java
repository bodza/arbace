package java.util.function;

/**
 * Represents a supplier of {@code int}-valued results.  This is the
 * {@code int}-producing primitive specialization of {@link Supplier}.
 *
 * There is no requirement that a distinct result be returned each
 * time the supplier is invoked.
 *
 * This is a <a href="package-summary.html">functional interface</a>
 * whose functional method is {@link #getAsInt()}.
 */
@FunctionalInterface
public interface IntSupplier {
    /**
     * Gets a result.
     *
     * @return a result
     */
    int getAsInt();
}

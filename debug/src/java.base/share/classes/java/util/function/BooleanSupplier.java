package java.util.function;

/**
 * Represents a supplier of {@code boolean}-valued results.  This is the
 * {@code boolean}-producing primitive specialization of {@link Supplier}.
 *
 * There is no requirement that a new or distinct result be returned each
 * time the supplier is invoked.
 *
 * This is a <a href="package-summary.html">functional interface</a>
 * whose functional method is {@link #getAsBoolean()}.
 */
@FunctionalInterface
public interface BooleanSupplier {
    /**
     * Gets a result.
     *
     * @return a result
     */
    boolean getAsBoolean();
}

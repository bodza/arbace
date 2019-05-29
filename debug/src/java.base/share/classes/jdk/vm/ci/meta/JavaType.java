package jdk.vm.ci.meta;

/**
 * Represents a resolved or unresolved type. Types include primitives, objects, {@code void}, and
 * arrays thereof.
 */
public interface JavaType {
    /**
     * Returns the name of this type in internal form. The following are examples of strings
     * returned by this method:
     *
     * <pre>
     *     "Ljava/lang/Object;"
     *     "I"
     *     "[[B"
     * </pre>
     */
    String getName();
}

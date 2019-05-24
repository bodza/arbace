package java.lang.reflect;

/**
 * {@code AnnotatedWildcardType} represents the potentially annotated use of a
 * wildcard type argument, whose upper or lower bounds may themselves represent
 * annotated uses of types.
 */
public interface AnnotatedWildcardType extends AnnotatedType {
    /**
     * Returns the potentially annotated lower bounds of this wildcard type.
     * If no lower bound is explicitly declared, the lower bound is the
     * type of null. In this case, a zero length array is returned.
     *
     * @return the potentially annotated lower bounds of this wildcard type or
     * an empty array if no lower bound is explicitly declared.
     */
    AnnotatedType[] getAnnotatedLowerBounds();

    /**
     * Returns the potentially annotated upper bounds of this wildcard type.
     * If no upper bound is explicitly declared, the upper bound is
     * unannotated {@code Object}
     *
     * @return the potentially annotated upper bounds of this wildcard type
     */
    AnnotatedType[] getAnnotatedUpperBounds();

    /**
     * Returns the potentially annotated type that this type is a member of, if
     * this type represents a nested type. For example, if this type is
     * {@code @TA O<T>.I<S>}, return a representation of {@code @TA O<T>}.
     *
     * Returns {@code null} for an {@code AnnotatedType} that is an instance
     *     of {@code AnnotatedWildcardType}.
     *
     * @return {@code null}
     */
    @Override
    AnnotatedType getAnnotatedOwnerType();
}

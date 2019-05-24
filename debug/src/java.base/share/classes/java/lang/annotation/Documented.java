package java.lang.annotation;

/**
 * If the annotation {@code @Documented} is present on the declaration
 * of an annotation type <i>A</i>, then any {@code @A} annotation on
 * an element is considered part of the element's public contract.
 *
 * In more detail, when an annotation type <i>A</i> is annotated with
 * {@code Documented}, the presence and value of annotations of type
 * <i>A</i> are a part of the public contract of the elements <i>A</i>
 * annotates.
 *
 * Conversely, if an annotation type <i>B</i> is <em>not</em>
 * annotated with {@code Documented}, the presence and value of
 * <i>B</i> annotations are <em>not</em> part of the public contract
 * of the elements <i>B</i> annotates.
 *
 * Concretely, if an annotation type is annotated with {@code
 * Documented}, by default a tool like javadoc will display
 * annotations of that type in its output while annotations of
 * annotation types without {@code Documented} will not be displayed.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.ANNOTATION_TYPE)
public @interface Documented {
}

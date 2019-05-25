package jdk.internal.reflect;

import static java.lang.annotation.ElementType.*;

/**
 * A method annotated @CallerSensitive is sensitive to its calling class,
 * via {@link jdk.internal.reflect.Reflection#getCallerClass Reflection.getCallerClass},
 * or via some equivalent.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({METHOD})
public @interface CallerSensitive {
}

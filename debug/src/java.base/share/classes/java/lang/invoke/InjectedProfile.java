package java.lang.invoke;

import java.lang.annotation.*;

/**
 * Internal marker for some methods in the JSR 292 implementation.
 */
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
@interface InjectedProfile {
}

package jdk.internal.vm.annotation;

import java.lang.annotation.*;

/**
 * A method or constructor may be annotated as "force inline" if the standard
 * inlining metrics are to be ignored when the HotSpot VM inlines the method
 * or constructor.
 *
 * This annotation must be used sparingly.  It is useful when the only
 * reasonable alternative is to bind the name of a specific method or
 * constructor into the HotSpot VM for special handling by the inlining policy.
 * This annotation must not be relied on as an alternative to avoid tuning the
 * VM's inlining policy.  In a few cases, it may act as a temporary workaround
 * until the profiling and inlining performed by the HotSpot VM is sufficiently
 * improved.
 *
 * @implNote
 * This annotation only takes effect for methods or constructors of classes
 * loaded by the boot loader.  Annotations on methods or constructors of classes
 * loaded outside of the boot loader are ignored.
 */
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface ForceInline {
}

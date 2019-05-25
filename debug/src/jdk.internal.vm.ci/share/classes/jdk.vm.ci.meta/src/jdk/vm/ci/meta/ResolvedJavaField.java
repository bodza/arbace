package jdk.vm.ci.meta;

import java.lang.reflect.Modifier;

/**
 * Represents a reference to a resolved Java field.
 */
public interface ResolvedJavaField extends JavaField {
    /**
     * {@inheritDoc}
     *
     * Only the {@linkplain Modifier#fieldModifiers() field flags} specified in the JVM
     * specification will be included in the returned mask.
     */
    @Override
    int getModifiers();

    int getOffset();
}

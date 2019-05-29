package jdk.vm.ci.meta;

/**
 * Represents a reference to a Java field, either resolved or unresolved fields.
 */
public interface JavaField {
    /**
     * Returns the name of this field.
     */
    String getName();
}

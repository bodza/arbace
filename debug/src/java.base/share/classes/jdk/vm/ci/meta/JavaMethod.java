package jdk.vm.ci.meta;

/**
 * Represents a reference to a Java method, either resolved or unresolved.
 */
public interface JavaMethod {
    /**
     * Returns the name of this method.
     */
    String getName();
}

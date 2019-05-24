package jdk.internal.org.objectweb.asm.tree.analysis;

/**
 * An immutable symbolic value for semantic interpretation of bytecode.
 */
public interface Value {
    /**
     * Returns the size of this value in words.
     *
     * @return either 1 or 2.
     */
    int getSize();
}

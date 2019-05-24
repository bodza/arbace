package sun.reflect.generics.tree;

/**
 * Common superinterface for generic signatures. These are the signatures
 * of complete class and method/constructor declarations.
 */
public interface Signature extends Tree {
    FormalTypeParameter[] getFormalTypeParameters();
}

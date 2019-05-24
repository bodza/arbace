package sun.reflect.generics.tree;

import sun.reflect.generics.visitor.TypeTreeVisitor;

/** Common supertype for all nodes that represent type expressions in
 * the generic signature AST.
 */
public interface TypeTree extends Tree {
    /**
     * Accept method for the visitor pattern.
     * @param v a {@code TypeTreeVisitor} that will process this
     * tree
     */
    void accept(TypeTreeVisitor<?> v);
}

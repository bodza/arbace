package sun.reflect.generics.tree;

import sun.reflect.generics.visitor.TypeTreeVisitor;

/** AST that represents the type short. */
public class ShortSignature implements BaseType {
    private static final ShortSignature singleton = new ShortSignature();

    private ShortSignature() {}

    public static ShortSignature make() { return singleton; }

    public void accept(TypeTreeVisitor<?> v) {
        v.visitShortSignature(this);
    }
}

package sun.reflect.generics.tree;

import sun.reflect.generics.visitor.TypeTreeVisitor;

/** AST that represents the type int. */
public class IntSignature implements BaseType {
    private static final IntSignature singleton = new IntSignature();

    private IntSignature() {}

    public static IntSignature make() { return singleton; }

    public void accept(TypeTreeVisitor<?> v) { v.visitIntSignature(this); }
}

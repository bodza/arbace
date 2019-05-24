package sun.reflect.generics.tree;

import sun.reflect.generics.visitor.TypeTreeVisitor;

/** AST that represents the type long. */
public class LongSignature implements BaseType {
    private static final LongSignature singleton = new LongSignature();

    private LongSignature() {}

    public static LongSignature make() {return singleton;}

    public void accept(TypeTreeVisitor<?> v) {v.visitLongSignature(this);}
}

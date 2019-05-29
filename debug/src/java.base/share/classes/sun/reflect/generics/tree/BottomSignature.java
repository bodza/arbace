package sun.reflect.generics.tree;

import sun.reflect.generics.visitor.TypeTreeVisitor;

public class BottomSignature implements FieldTypeSignature {
    private static final BottomSignature singleton = new BottomSignature();

    private BottomSignature() {}

    public static BottomSignature make() { return singleton; }

    public void accept(TypeTreeVisitor<?> v) { v.visitBottomSignature(this); }
}

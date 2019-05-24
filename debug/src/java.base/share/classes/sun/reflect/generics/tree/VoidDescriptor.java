package sun.reflect.generics.tree;

import sun.reflect.generics.visitor.TypeTreeVisitor;

/** AST that represents the pseudo-type void. */
public class VoidDescriptor implements ReturnType {
    private static final VoidDescriptor singleton = new VoidDescriptor();

    private VoidDescriptor() {}

    public static VoidDescriptor make() {return singleton;}

    public void accept(TypeTreeVisitor<?> v) {v.visitVoidDescriptor(this);}
}

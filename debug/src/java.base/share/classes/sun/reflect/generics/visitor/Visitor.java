package sun.reflect.generics.visitor;

import sun.reflect.generics.tree.*;

public interface Visitor<T> extends TypeTreeVisitor<T> {
    void visitClassSignature(ClassSignature cs);
    void visitMethodTypeSignature(MethodTypeSignature ms);
}

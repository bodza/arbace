package jdk.internal.org.objectweb.asm.commons;

import jdk.internal.org.objectweb.asm.Opcodes;
import jdk.internal.org.objectweb.asm.signature.SignatureVisitor;

/**
 * A {@link SignatureVisitor} adapter for type mapping.
 *
 * @deprecated use {@link SignatureRemapper} instead.
 */
@Deprecated
public class RemappingSignatureAdapter extends SignatureVisitor {
    private final SignatureVisitor v;

    private final Remapper remapper;

    private String className;

    public RemappingSignatureAdapter(final SignatureVisitor v, final Remapper remapper) {
        this(Opcodes.ASM6, v, remapper);
    }

    protected RemappingSignatureAdapter(final int api, final SignatureVisitor v, final Remapper remapper) {
        super(api);
        this.v = v;
        this.remapper = remapper;
    }

    @Override
    public void visitClassType(String name) {
        className = name;
        v.visitClassType(remapper.mapType(name));
    }

    @Override
    public void visitInnerClassType(String name) {
        String remappedOuter = remapper.mapType(className) + '$';
        className = className + '$' + name;
        String remappedName = remapper.mapType(className);
        int index = remappedName.startsWith(remappedOuter) ? remappedOuter.length() : remappedName.lastIndexOf('$') + 1;
        v.visitInnerClassType(remappedName.substring(index));
    }

    @Override
    public void visitFormalTypeParameter(String name) {
        v.visitFormalTypeParameter(name);
    }

    @Override
    public void visitTypeVariable(String name) {
        v.visitTypeVariable(name);
    }

    @Override
    public SignatureVisitor visitArrayType() {
        v.visitArrayType();
        return this;
    }

    @Override
    public void visitBaseType(char descriptor) {
        v.visitBaseType(descriptor);
    }

    @Override
    public SignatureVisitor visitClassBound() {
        v.visitClassBound();
        return this;
    }

    @Override
    public SignatureVisitor visitExceptionType() {
        v.visitExceptionType();
        return this;
    }

    @Override
    public SignatureVisitor visitInterface() {
        v.visitInterface();
        return this;
    }

    @Override
    public SignatureVisitor visitInterfaceBound() {
        v.visitInterfaceBound();
        return this;
    }

    @Override
    public SignatureVisitor visitParameterType() {
        v.visitParameterType();
        return this;
    }

    @Override
    public SignatureVisitor visitReturnType() {
        v.visitReturnType();
        return this;
    }

    @Override
    public SignatureVisitor visitSuperclass() {
        v.visitSuperclass();
        return this;
    }

    @Override
    public void visitTypeArgument() {
        v.visitTypeArgument();
    }

    @Override
    public SignatureVisitor visitTypeArgument(char wildcard) {
        v.visitTypeArgument(wildcard);
        return this;
    }

    @Override
    public void visitEnd() {
        v.visitEnd();
    }
}

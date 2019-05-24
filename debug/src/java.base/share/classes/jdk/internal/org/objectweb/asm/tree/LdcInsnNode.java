package jdk.internal.org.objectweb.asm.tree;

import java.util.Map;

import jdk.internal.org.objectweb.asm.MethodVisitor;
import jdk.internal.org.objectweb.asm.Opcodes;

/**
 * A node that represents an LDC instruction.
 */
public class LdcInsnNode extends AbstractInsnNode {
    /**
     * The constant to be loaded on the stack. This parameter must be a non null
     * {@link Integer}, a {@link Float}, a {@link Long}, a {@link Double}, a
     * {@link String} or a {@link jdk.internal.org.objectweb.asm.Type}.
     */
    public Object cst;

    /**
     * Constructs a new {@link LdcInsnNode}.
     *
     * @param cst
     *            the constant to be loaded on the stack. This parameter must be
     *            a non null {@link Integer}, a {@link Float}, a {@link Long}, a
     *            {@link Double} or a {@link String}.
     */
    public LdcInsnNode(final Object cst) {
        super(Opcodes.LDC);
        this.cst = cst;
    }

    @Override
    public int getType() {
        return LDC_INSN;
    }

    @Override
    public void accept(final MethodVisitor mv) {
        mv.visitLdcInsn(cst);
        acceptAnnotations(mv);
    }

    @Override
    public AbstractInsnNode clone(final Map<LabelNode, LabelNode> labels) {
        return new LdcInsnNode(cst).cloneAnnotations(this);
    }
}

package jdk.internal.org.objectweb.asm.tree;

import java.util.Map;

import jdk.internal.org.objectweb.asm.MethodVisitor;
import jdk.internal.org.objectweb.asm.Opcodes;

/**
 * A node that represents an IINC instruction.
 */
public class IincInsnNode extends AbstractInsnNode {
    /**
     * Index of the local variable to be incremented.
     */
    public int var;

    /**
     * Amount to increment the local variable by.
     */
    public int incr;

    /**
     * Constructs a new {@link IincInsnNode}.
     *
     * @param var
     *            index of the local variable to be incremented.
     * @param incr
     *            increment amount to increment the local variable by.
     */
    public IincInsnNode(final int var, final int incr) {
        super(Opcodes.IINC);
        this.var = var;
        this.incr = incr;
    }

    @Override
    public int getType() {
        return IINC_INSN;
    }

    @Override
    public void accept(final MethodVisitor mv) {
        mv.visitIincInsn(var, incr);
        acceptAnnotations(mv);
    }

    @Override
    public AbstractInsnNode clone(final Map<LabelNode, LabelNode> labels) {
        return new IincInsnNode(var, incr).cloneAnnotations(this);
    }
}

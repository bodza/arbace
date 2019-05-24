package jdk.internal.org.objectweb.asm.tree;

import java.util.Map;

import jdk.internal.org.objectweb.asm.MethodVisitor;
import jdk.internal.org.objectweb.asm.Opcodes;

/**
 * A node that represents a MULTIANEWARRAY instruction.
 */
public class MultiANewArrayInsnNode extends AbstractInsnNode {
    /**
     * An array type descriptor (see {@link jdk.internal.org.objectweb.asm.Type}).
     */
    public String desc;

    /**
     * Number of dimensions of the array to allocate.
     */
    public int dims;

    /**
     * Constructs a new {@link MultiANewArrayInsnNode}.
     *
     * @param desc
     *            an array type descriptor (see {@link jdk.internal.org.objectweb.asm.Type}).
     * @param dims
     *            number of dimensions of the array to allocate.
     */
    public MultiANewArrayInsnNode(final String desc, final int dims) {
        super(Opcodes.MULTIANEWARRAY);
        this.desc = desc;
        this.dims = dims;
    }

    @Override
    public int getType() {
        return MULTIANEWARRAY_INSN;
    }

    @Override
    public void accept(final MethodVisitor mv) {
        mv.visitMultiANewArrayInsn(desc, dims);
        acceptAnnotations(mv);
    }

    @Override
    public AbstractInsnNode clone(final Map<LabelNode, LabelNode> labels) {
        return new MultiANewArrayInsnNode(desc, dims).cloneAnnotations(this);
    }
}

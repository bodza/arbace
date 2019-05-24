package jdk.internal.org.objectweb.asm.tree;

import java.util.Map;

import jdk.internal.org.objectweb.asm.MethodVisitor;

/**
 * A node that represents a field instruction. A field instruction is an
 * instruction that loads or stores the value of a field of an object.
 */
public class FieldInsnNode extends AbstractInsnNode {
    /**
     * The internal name of the field's owner class (see
     * {@link jdk.internal.org.objectweb.asm.Type#getInternalName() getInternalName}).
     */
    public String owner;

    /**
     * The field's name.
     */
    public String name;

    /**
     * The field's descriptor (see {@link jdk.internal.org.objectweb.asm.Type}).
     */
    public String desc;

    /**
     * Constructs a new {@link FieldInsnNode}.
     *
     * @param opcode
     *            the opcode of the type instruction to be constructed. This
     *            opcode must be GETSTATIC, PUTSTATIC, GETFIELD or PUTFIELD.
     * @param owner
     *            the internal name of the field's owner class (see
     *            {@link jdk.internal.org.objectweb.asm.Type#getInternalName()
     *            getInternalName}).
     * @param name
     *            the field's name.
     * @param desc
     *            the field's descriptor (see {@link jdk.internal.org.objectweb.asm.Type}).
     */
    public FieldInsnNode(final int opcode, final String owner, final String name, final String desc) {
        super(opcode);
        this.owner = owner;
        this.name = name;
        this.desc = desc;
    }

    /**
     * Sets the opcode of this instruction.
     *
     * @param opcode
     *            the new instruction opcode. This opcode must be GETSTATIC,
     *            PUTSTATIC, GETFIELD or PUTFIELD.
     */
    public void setOpcode(final int opcode) {
        this.opcode = opcode;
    }

    @Override
    public int getType() {
        return FIELD_INSN;
    }

    @Override
    public void accept(final MethodVisitor mv) {
        mv.visitFieldInsn(opcode, owner, name, desc);
        acceptAnnotations(mv);
    }

    @Override
    public AbstractInsnNode clone(final Map<LabelNode, LabelNode> labels) {
        return new FieldInsnNode(opcode, owner, name, desc).cloneAnnotations(this);
    }
}

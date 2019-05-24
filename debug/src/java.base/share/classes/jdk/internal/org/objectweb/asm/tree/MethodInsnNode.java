package jdk.internal.org.objectweb.asm.tree;

import java.util.Map;

import jdk.internal.org.objectweb.asm.MethodVisitor;
import jdk.internal.org.objectweb.asm.Opcodes;

/**
 * A node that represents a method instruction. A method instruction is an
 * instruction that invokes a method.
 */
public class MethodInsnNode extends AbstractInsnNode {
    /**
     * The internal name of the method's owner class (see
     * {@link jdk.internal.org.objectweb.asm.Type#getInternalName() getInternalName}).
     */
    public String owner;

    /**
     * The method's name.
     */
    public String name;

    /**
     * The method's descriptor (see {@link jdk.internal.org.objectweb.asm.Type}).
     */
    public String desc;

    /**
     * If the method's owner class if an interface.
     */
    public boolean itf;

    /**
     * Constructs a new {@link MethodInsnNode}.
     *
     * @param opcode
     *            the opcode of the type instruction to be constructed. This
     *            opcode must be INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC or
     *            INVOKEINTERFACE.
     * @param owner
     *            the internal name of the method's owner class (see
     *            {@link jdk.internal.org.objectweb.asm.Type#getInternalName()
     *            getInternalName}).
     * @param name
     *            the method's name.
     * @param desc
     *            the method's descriptor (see {@link jdk.internal.org.objectweb.asm.Type}).
     */
    @Deprecated
    public MethodInsnNode(final int opcode, final String owner, final String name, final String desc) {
        this(opcode, owner, name, desc, opcode == Opcodes.INVOKEINTERFACE);
    }

    /**
     * Constructs a new {@link MethodInsnNode}.
     *
     * @param opcode
     *            the opcode of the type instruction to be constructed. This
     *            opcode must be INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC or
     *            INVOKEINTERFACE.
     * @param owner
     *            the internal name of the method's owner class (see
     *            {@link jdk.internal.org.objectweb.asm.Type#getInternalName()
     *            getInternalName}).
     * @param name
     *            the method's name.
     * @param desc
     *            the method's descriptor (see {@link jdk.internal.org.objectweb.asm.Type}).
     * @param itf
     *            if the method's owner class is an interface.
     */
    public MethodInsnNode(final int opcode, final String owner, final String name, final String desc, final boolean itf) {
        super(opcode);
        this.owner = owner;
        this.name = name;
        this.desc = desc;
        this.itf = itf;
    }

    /**
     * Sets the opcode of this instruction.
     *
     * @param opcode
     *            the new instruction opcode. This opcode must be INVOKEVIRTUAL,
     *            INVOKESPECIAL, INVOKESTATIC or INVOKEINTERFACE.
     */
    public void setOpcode(final int opcode) {
        this.opcode = opcode;
    }

    @Override
    public int getType() {
        return METHOD_INSN;
    }

    @Override
    public void accept(final MethodVisitor mv) {
        mv.visitMethodInsn(opcode, owner, name, desc, itf);
        acceptAnnotations(mv);
    }

    @Override
    public AbstractInsnNode clone(final Map<LabelNode, LabelNode> labels) {
        return new MethodInsnNode(opcode, owner, name, desc, itf);
    }
}

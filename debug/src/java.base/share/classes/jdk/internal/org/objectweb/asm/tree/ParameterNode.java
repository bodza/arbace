package jdk.internal.org.objectweb.asm.tree;

import jdk.internal.org.objectweb.asm.MethodVisitor;

/**
 * A node that represents a parameter access and name.
 */
public class ParameterNode {
    /**
     * The parameter's name.
     */
    public String name;

    /**
     * The parameter's access flags (see {@link jdk.internal.org.objectweb.asm.Opcodes}).
     * Valid values are <tt>ACC_FINAL</tt>, <tt>ACC_SYNTHETIC</tt> and <tt>ACC_MANDATED</tt>.
     */
    public int access;

    /**
     * Constructs a new {@link ParameterNode}.
     *
     * @param access
     *            The parameter's access flags. Valid values are
     *            <tt>ACC_FINAL</tt>, <tt>ACC_SYNTHETIC</tt> or/and
     *            <tt>ACC_MANDATED</tt> (see {@link jdk.internal.org.objectweb.asm.Opcodes}).
     * @param name
     *            the parameter's name.
     */
    public ParameterNode(final String name, final int access) {
        this.name = name;
        this.access = access;
    }

    /**
     * Makes the given visitor visit this parameter declaration.
     *
     * @param mv
     *            a method visitor.
     */
    public void accept(final MethodVisitor mv) {
        mv.visitParameter(name, access);
    }
}

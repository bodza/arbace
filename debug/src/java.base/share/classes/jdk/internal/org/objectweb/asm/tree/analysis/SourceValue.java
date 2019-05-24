package jdk.internal.org.objectweb.asm.tree.analysis;

import java.util.Set;

import jdk.internal.org.objectweb.asm.tree.AbstractInsnNode;

/**
 * A {@link Value} that is represented by its type in a two types type system.
 * This type system distinguishes the ONEWORD and TWOWORDS types.
 */
public class SourceValue implements Value {
    /**
     * The size of this value.
     */
    public final int size;

    /**
     * The instructions that can produce this value. For example, for the Java
     * code below, the instructions that can produce the value of <tt>i</tt> at
     * line 5 are the txo ISTORE instructions at line 1 and 3:
     *
     * <pre>
     * 1: i = 0;
     * 2: if (...) {
     * 3:   i = 1;
     * 4: }
     * 5: return i;
     * </pre>
     *
     * This field is a set of {@link AbstractInsnNode} objects.
     */
    public final Set<AbstractInsnNode> insns;

    public SourceValue(final int size) {
        this(size, SmallSet.<AbstractInsnNode> emptySet());
    }

    public SourceValue(final int size, final AbstractInsnNode insn) {
        this.size = size;
        this.insns = new SmallSet<AbstractInsnNode>(insn, null);
    }

    public SourceValue(final int size, final Set<AbstractInsnNode> insns) {
        this.size = size;
        this.insns = insns;
    }

    public int getSize() {
        return size;
    }

    @Override
    public boolean equals(final Object value) {
        if (!(value instanceof SourceValue)) {
            return false;
        }
        SourceValue v = (SourceValue) value;
        return size == v.size && insns.equals(v.insns);
    }

    @Override
    public int hashCode() {
        return insns.hashCode();
    }
}

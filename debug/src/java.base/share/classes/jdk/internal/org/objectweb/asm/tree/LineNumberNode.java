package jdk.internal.org.objectweb.asm.tree;

import java.util.Map;

import jdk.internal.org.objectweb.asm.MethodVisitor;

/**
 * A node that represents a line number declaration. These nodes are pseudo
 * instruction nodes in order to be inserted in an instruction list.
 */
public class LineNumberNode extends AbstractInsnNode {
    /**
     * A line number. This number refers to the source file from which the class
     * was compiled.
     */
    public int line;

    /**
     * The first instruction corresponding to this line number.
     */
    public LabelNode start;

    /**
     * Constructs a new {@link LineNumberNode}.
     *
     * @param line
     *            a line number. This number refers to the source file from
     *            which the class was compiled.
     * @param start
     *            the first instruction corresponding to this line number.
     */
    public LineNumberNode(final int line, final LabelNode start) {
        super(-1);
        this.line = line;
        this.start = start;
    }

    @Override
    public int getType() {
        return LINE;
    }

    @Override
    public void accept(final MethodVisitor mv) {
        mv.visitLineNumber(line, start.getLabel());
    }

    @Override
    public AbstractInsnNode clone(final Map<LabelNode, LabelNode> labels) {
        return new LineNumberNode(line, clone(start, labels));
    }
}

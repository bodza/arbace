package jdk.internal.org.objectweb.asm.tree;

import java.util.Map;

import jdk.internal.org.objectweb.asm.Label;
import jdk.internal.org.objectweb.asm.MethodVisitor;

/**
 * An {@link AbstractInsnNode} that encapsulates a {@link Label}.
 */
public class LabelNode extends AbstractInsnNode {
    private Label label;

    public LabelNode() {
        super(-1);
    }

    public LabelNode(final Label label) {
        super(-1);
        this.label = label;
    }

    @Override
    public int getType() {
        return LABEL;
    }

    public Label getLabel() {
        if (label == null) {
            label = new Label();
        }
        return label;
    }

    @Override
    public void accept(final MethodVisitor cv) {
        cv.visitLabel(getLabel());
    }

    @Override
    public AbstractInsnNode clone(final Map<LabelNode, LabelNode> labels) {
        return labels.get(this);
    }

    public void resetLabel() {
        label = null;
    }
}

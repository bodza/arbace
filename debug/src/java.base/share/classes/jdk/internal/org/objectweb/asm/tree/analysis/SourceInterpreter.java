package jdk.internal.org.objectweb.asm.tree.analysis;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jdk.internal.org.objectweb.asm.Opcodes;
import jdk.internal.org.objectweb.asm.Type;
import jdk.internal.org.objectweb.asm.tree.AbstractInsnNode;
import jdk.internal.org.objectweb.asm.tree.FieldInsnNode;
import jdk.internal.org.objectweb.asm.tree.InvokeDynamicInsnNode;
import jdk.internal.org.objectweb.asm.tree.LdcInsnNode;
import jdk.internal.org.objectweb.asm.tree.MethodInsnNode;

/**
 * An {@link Interpreter} for {@link SourceValue} values.
 */
public class SourceInterpreter extends Interpreter<SourceValue> implements
        Opcodes {
    public SourceInterpreter() {
        super(ASM6);
    }

    protected SourceInterpreter(final int api) {
        super(api);
    }

    @Override
    public SourceValue newValue(final Type type) {
        if (type == Type.VOID_TYPE) {
            return null;
        }
        return new SourceValue(type == null ? 1 : type.getSize());
    }

    @Override
    public SourceValue newOperation(final AbstractInsnNode insn) {
        int size;
        switch (insn.getOpcode()) {
        case LCONST_0:
        case LCONST_1:
        case DCONST_0:
        case DCONST_1:
            size = 2;
            break;
        case LDC:
            Object cst = ((LdcInsnNode) insn).cst;
            size = cst instanceof Long || cst instanceof Double ? 2 : 1;
            break;
        case GETSTATIC:
            size = Type.getType(((FieldInsnNode) insn).desc).getSize();
            break;
        default:
            size = 1;
        }
        return new SourceValue(size, insn);
    }

    @Override
    public SourceValue copyOperation(final AbstractInsnNode insn, final SourceValue value) {
        return new SourceValue(value.getSize(), insn);
    }

    @Override
    public SourceValue unaryOperation(final AbstractInsnNode insn, final SourceValue value) {
        int size;
        switch (insn.getOpcode()) {
        case LNEG:
        case DNEG:
        case I2L:
        case I2D:
        case L2D:
        case F2L:
        case F2D:
        case D2L:
            size = 2;
            break;
        case GETFIELD:
            size = Type.getType(((FieldInsnNode) insn).desc).getSize();
            break;
        default:
            size = 1;
        }
        return new SourceValue(size, insn);
    }

    @Override
    public SourceValue binaryOperation(final AbstractInsnNode insn, final SourceValue value1, final SourceValue value2) {
        int size;
        switch (insn.getOpcode()) {
        case LALOAD:
        case DALOAD:
        case LADD:
        case DADD:
        case LSUB:
        case DSUB:
        case LMUL:
        case DMUL:
        case LDIV:
        case DDIV:
        case LREM:
        case DREM:
        case LSHL:
        case LSHR:
        case LUSHR:
        case LAND:
        case LOR:
        case LXOR:
            size = 2;
            break;
        default:
            size = 1;
        }
        return new SourceValue(size, insn);
    }

    @Override
    public SourceValue ternaryOperation(final AbstractInsnNode insn, final SourceValue value1, final SourceValue value2, final SourceValue value3) {
        return new SourceValue(1, insn);
    }

    @Override
    public SourceValue naryOperation(final AbstractInsnNode insn, final List<? extends SourceValue> values) {
        int size;
        int opcode = insn.getOpcode();
        if (opcode == MULTIANEWARRAY) {
            size = 1;
        } else {
            String desc = (opcode == INVOKEDYNAMIC) ? ((InvokeDynamicInsnNode) insn).desc
                    : ((MethodInsnNode) insn).desc;
            size = Type.getReturnType(desc).getSize();
        }
        return new SourceValue(size, insn);
    }

    @Override
    public void returnOperation(final AbstractInsnNode insn, final SourceValue value, final SourceValue expected) {
    }

    @Override
    public SourceValue merge(final SourceValue d, final SourceValue w) {
        if (d.insns instanceof SmallSet && w.insns instanceof SmallSet) {
            Set<AbstractInsnNode> s = ((SmallSet<AbstractInsnNode>) d.insns).union((SmallSet<AbstractInsnNode>) w.insns);
            if (s == d.insns && d.size == w.size) {
                return d;
            } else {
                return new SourceValue(Math.min(d.size, w.size), s);
            }
        }
        if (d.size != w.size || !d.insns.containsAll(w.insns)) {
            HashSet<AbstractInsnNode> s = new HashSet<AbstractInsnNode>();
            s.addAll(d.insns);
            s.addAll(w.insns);
            return new SourceValue(Math.min(d.size, w.size), s);
        }
        return d;
    }
}

package jdk.vm.ci.code;

import jdk.vm.ci.meta.AllocatableValue;
import jdk.vm.ci.meta.ValueKind;

/**
 * Represents a compiler spill slot or an outgoing stack-based argument in a method's frame or an
 * incoming stack-based argument in a method's {@linkplain #isInCallerFrame() caller's frame}.
 */
public final class StackSlot extends AllocatableValue {
    private final int offset;
    private final boolean addFrameSize;

    /**
     * Gets a {@link StackSlot} instance representing a stack slot at a given index holding a value
     * of a given kind.
     *
     * @param kind The kind of the value stored in the stack slot.
     * @param offset The offset of the stack slot (in bytes)
     * @param addFrameSize Specifies if the offset is relative to the stack pointer, or the
     *            beginning of the frame (stack pointer + total frame size).
     */
    public static StackSlot get(ValueKind<?> kind, int offset, boolean addFrameSize) {
        return new StackSlot(kind, offset, addFrameSize);
    }

    /**
     * Private constructor to enforce use of {@link #get(ValueKind, int, boolean)} so that a cache
     * can be used.
     */
    private StackSlot(ValueKind<?> kind, int offset, boolean addFrameSize) {
        super(kind);
        this.offset = offset;
        this.addFrameSize = addFrameSize;
    }

    /**
     * Gets the offset of this stack slot, relative to the stack pointer.
     *
     * @return The offset of this slot (in bytes).
     */
    public int getOffset(int totalFrameSize) {
        int result = offset + (addFrameSize ? totalFrameSize : 0);
        return result;
    }

    public boolean isInCallerFrame() {
        return addFrameSize && offset >= 0;
    }

    public int getRawOffset() {
        return offset;
    }

    public boolean getRawAddFrameSize() {
        return addFrameSize;
    }

    /**
     * Gets this stack slot used to pass an argument from the perspective of a caller.
     */
    public StackSlot asOutArg() {
        if (addFrameSize) {
            return get(getValueKind(), offset, false);
        }
        return this;
    }

    /**
     * Gets this stack slot used to pass an argument from the perspective of a callee.
     */
    public StackSlot asInArg() {
        if (!addFrameSize) {
            return get(getValueKind(), offset, true);
        }
        return this;
    }

    @Override
    public int hashCode() {
        final int prime = 37;
        int result = super.hashCode();
        result = prime * result + (addFrameSize ? 1231 : 1237);
        result = prime * result + offset;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof StackSlot) {
            StackSlot other = (StackSlot) obj;
            return super.equals(obj) && addFrameSize == other.addFrameSize && offset == other.offset;
        }
        return false;
    }
}

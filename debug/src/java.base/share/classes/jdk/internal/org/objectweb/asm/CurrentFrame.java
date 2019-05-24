package jdk.internal.org.objectweb.asm;

/**
 * Information about the input stack map frame at the "current" instruction of a
 * method. This is implemented as a Frame subclass for a "basic block"
 * containing only one instruction.
 */
class CurrentFrame extends Frame {
    /**
     * Sets this CurrentFrame to the input stack map frame of the next "current"
     * instruction, i.e. the instruction just after the given one. It is assumed
     * that the value of this object when this method is called is the stack map
     * frame status just before the given instruction is executed.
     */
    @Override
    void execute(int opcode, int arg, ClassWriter cw, Item item) {
        super.execute(opcode, arg, cw, item);
        Frame successor = new Frame();
        merge(cw, successor, 0);
        set(successor);
        owner.inputStackTop = 0;
    }
}

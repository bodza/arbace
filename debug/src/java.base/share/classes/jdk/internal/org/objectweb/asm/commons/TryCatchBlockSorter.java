package jdk.internal.org.objectweb.asm.commons;

import java.util.Collections;
import java.util.Comparator;

import jdk.internal.org.objectweb.asm.MethodVisitor;
import jdk.internal.org.objectweb.asm.Opcodes;
import jdk.internal.org.objectweb.asm.tree.MethodNode;
import jdk.internal.org.objectweb.asm.tree.TryCatchBlockNode;

/**
 * A {@link MethodVisitor} adapter to sort the exception handlers. The handlers
 * are sorted in a method innermost-to-outermost. This allows the programmer to
 * add handlers without worrying about ordering them correctly with respect to
 * existing, in-code handlers.
 *
 * Behavior is only defined for properly-nested handlers. If any "try" blocks
 * overlap (something that isn't possible in Java code) then this may not do
 * what you want. In fact, this adapter just sorts by the length of the "try"
 * block, taking advantage of the fact that a given try block must be larger
 * than any block it contains).
 */
public class TryCatchBlockSorter extends MethodNode {
    public TryCatchBlockSorter(final MethodVisitor mv, final int access, final String name, final String desc, final String signature, final String[] exceptions) {
        this(Opcodes.ASM6, mv, access, name, desc, signature, exceptions);
    }

    protected TryCatchBlockSorter(final int api, final MethodVisitor mv, final int access, final String name, final String desc, final String signature, final String[] exceptions) {
        super(api, access, name, desc, signature, exceptions);
        this.mv = mv;
    }

    @Override
    public void visitEnd() {
        // Compares TryCatchBlockNodes by the length of their "try" block.
        Comparator<TryCatchBlockNode> comp = new Comparator<TryCatchBlockNode>() {
            public int compare(TryCatchBlockNode t1, TryCatchBlockNode t2) {
                int len1 = blockLength(t1);
                int len2 = blockLength(t2);
                return len1 - len2;
            }

            private int blockLength(TryCatchBlockNode block) {
                int startidx = instructions.indexOf(block.start);
                int endidx = instructions.indexOf(block.end);
                return endidx - startidx;
            }
        };
        Collections.sort(tryCatchBlocks, comp);
        // Updates the 'target' of each try catch block annotation.
        for (int i = 0; i < tryCatchBlocks.size(); ++i) {
            tryCatchBlocks.get(i).updateIndex(i);
        }
        if (mv != null) {
            accept(mv);
        }
    }
}

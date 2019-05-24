package jdk.internal.org.objectweb.asm.tree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jdk.internal.org.objectweb.asm.Label;
import jdk.internal.org.objectweb.asm.MethodVisitor;
import jdk.internal.org.objectweb.asm.Opcodes;
import jdk.internal.org.objectweb.asm.TypePath;
import jdk.internal.org.objectweb.asm.TypeReference;

/**
 * A node that represents a type annotation on a local or resource variable.
 */
public class LocalVariableAnnotationNode extends TypeAnnotationNode {
    /**
     * The fist instructions corresponding to the continuous ranges that make
     * the scope of this local variable (inclusive). Must not be <tt>null</tt>.
     */
    public List<LabelNode> start;

    /**
     * The last instructions corresponding to the continuous ranges that make
     * the scope of this local variable (exclusive). This list must have the
     * same size as the 'start' list. Must not be <tt>null</tt>.
     */
    public List<LabelNode> end;

    /**
     * The local variable's index in each range. This list must have the same
     * size as the 'start' list. Must not be <tt>null</tt>.
     */
    public List<Integer> index;

    /**
     * Constructs a new {@link LocalVariableAnnotationNode}. <i>Subclasses must
     * not use this constructor</i>. Instead, they must use the
     * {@link #LocalVariableAnnotationNode(int, TypePath, LabelNode[], LabelNode[], int[], String)}
     * version.
     *
     * @param typeRef
     *            a reference to the annotated type. See {@link TypeReference}.
     * @param typePath
     *            the path to the annotated type argument, wildcard bound, array
     *            element type, or static inner type within 'typeRef'. May be
     *            <tt>null</tt> if the annotation targets 'typeRef' as a whole.
     * @param start
     *            the fist instructions corresponding to the continuous ranges
     *            that make the scope of this local variable (inclusive).
     * @param end
     *            the last instructions corresponding to the continuous ranges
     *            that make the scope of this local variable (exclusive). This
     *            array must have the same size as the 'start' array.
     * @param index
     *            the local variable's index in each range. This array must have
     *            the same size as the 'start' array.
     * @param desc
     *            the class descriptor of the annotation class.
     */
    public LocalVariableAnnotationNode(int typeRef, TypePath typePath, LabelNode[] start, LabelNode[] end, int[] index, String desc) {
        this(Opcodes.ASM6, typeRef, typePath, start, end, index, desc);
    }

    /**
     * Constructs a new {@link LocalVariableAnnotationNode}.
     *
     * @param api
     *            the ASM API version implemented by this visitor. Must be one
     *            of {@link Opcodes#ASM4}, {@link Opcodes#ASM5} or {@link Opcodes#ASM6}.
     * @param typeRef
     *            a reference to the annotated type. See {@link TypeReference}.
     * @param start
     *            the fist instructions corresponding to the continuous ranges
     *            that make the scope of this local variable (inclusive).
     * @param end
     *            the last instructions corresponding to the continuous ranges
     *            that make the scope of this local variable (exclusive). This
     *            array must have the same size as the 'start' array.
     * @param index
     *            the local variable's index in each range. This array must have
     *            the same size as the 'start' array.
     * @param typePath
     *            the path to the annotated type argument, wildcard bound, array
     *            element type, or static inner type within 'typeRef'. May be
     *            <tt>null</tt> if the annotation targets 'typeRef' as a whole.
     * @param desc
     *            the class descriptor of the annotation class.
     */
    public LocalVariableAnnotationNode(int api, int typeRef, TypePath typePath, LabelNode[] start, LabelNode[] end, int[] index, String desc) {
        super(api, typeRef, typePath, desc);
        this.start = new ArrayList<LabelNode>(start.length);
        this.start.addAll(Arrays.asList(start));
        this.end = new ArrayList<LabelNode>(end.length);
        this.end.addAll(Arrays.asList(end));
        this.index = new ArrayList<Integer>(index.length);
        for (int i : index) {
            this.index.add(i);
        }
    }

    /**
     * Makes the given visitor visit this type annotation.
     *
     * @param mv
     *            the visitor that must visit this annotation.
     * @param visible
     *            <tt>true</tt> if the annotation is visible at runtime.
     */
    public void accept(final MethodVisitor mv, boolean visible) {
        Label[] start = new Label[this.start.size()];
        Label[] end = new Label[this.end.size()];
        int[] index = new int[this.index.size()];
        for (int i = 0; i < start.length; ++i) {
            start[i] = this.start.get(i).getLabel();
            end[i] = this.end.get(i).getLabel();
            index[i] = this.index.get(i);
        }
        accept(mv.visitLocalVariableAnnotation(typeRef, typePath, start, end,
                index, desc, visible));
    }
}

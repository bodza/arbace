package jdk.internal.org.objectweb.asm.tree;

import java.util.ArrayList;
import java.util.List;

import jdk.internal.org.objectweb.asm.AnnotationVisitor;
import jdk.internal.org.objectweb.asm.Attribute;
import jdk.internal.org.objectweb.asm.ClassVisitor;
import jdk.internal.org.objectweb.asm.FieldVisitor;
import jdk.internal.org.objectweb.asm.Opcodes;
import jdk.internal.org.objectweb.asm.TypePath;

/**
 * A node that represents a field.
 */
public class FieldNode extends FieldVisitor {
    /**
     * The field's access flags (see {@link jdk.internal.org.objectweb.asm.Opcodes}). This
     * field also indicates if the field is synthetic and/or deprecated.
     */
    public int access;

    /**
     * The field's name.
     */
    public String name;

    /**
     * The field's descriptor (see {@link jdk.internal.org.objectweb.asm.Type}).
     */
    public String desc;

    /**
     * The field's signature. May be <tt>null</tt>.
     */
    public String signature;

    /**
     * The field's initial value. This field, which may be <tt>null</tt> if the
     * field does not have an initial value, must be an {@link Integer}, a
     * {@link Float}, a {@link Long}, a {@link Double} or a {@link String}.
     */
    public Object value;

    /**
     * The runtime visible annotations of this field. This list is a list of
     * {@link AnnotationNode} objects. May be <tt>null</tt>.
     *
     * @associates jdk.internal.org.objectweb.asm.tree.AnnotationNode
     * @label visible
     */
    public List<AnnotationNode> visibleAnnotations;

    /**
     * The runtime invisible annotations of this field. This list is a list of
     * {@link AnnotationNode} objects. May be <tt>null</tt>.
     *
     * @associates jdk.internal.org.objectweb.asm.tree.AnnotationNode
     * @label invisible
     */
    public List<AnnotationNode> invisibleAnnotations;

    /**
     * The runtime visible type annotations of this field. This list is a list
     * of {@link TypeAnnotationNode} objects. May be <tt>null</tt>.
     *
     * @associates jdk.internal.org.objectweb.asm.tree.TypeAnnotationNode
     * @label visible
     */
    public List<TypeAnnotationNode> visibleTypeAnnotations;

    /**
     * The runtime invisible type annotations of this field. This list is a list
     * of {@link TypeAnnotationNode} objects. May be <tt>null</tt>.
     *
     * @associates jdk.internal.org.objectweb.asm.tree.TypeAnnotationNode
     * @label invisible
     */
    public List<TypeAnnotationNode> invisibleTypeAnnotations;

    /**
     * The non standard attributes of this field. This list is a list of
     * {@link Attribute} objects. May be <tt>null</tt>.
     *
     * @associates jdk.internal.org.objectweb.asm.Attribute
     */
    public List<Attribute> attrs;

    /**
     * Constructs a new {@link FieldNode}. <i>Subclasses must not use this
     * constructor</i>. Instead, they must use the
     * {@link #FieldNode(int, int, String, String, String, Object)} version.
     *
     * @param access
     *            the field's access flags (see
     *            {@link jdk.internal.org.objectweb.asm.Opcodes}). This parameter also
     *            indicates if the field is synthetic and/or deprecated.
     * @param name
     *            the field's name.
     * @param desc
     *            the field's descriptor (see {@link jdk.internal.org.objectweb.asm.Type
     *            Type}).
     * @param signature
     *            the field's signature.
     * @param value
     *            the field's initial value. This parameter, which may be
     *            <tt>null</tt> if the field does not have an initial value,
     *            must be an {@link Integer}, a {@link Float}, a {@link Long}, a
     *            {@link Double} or a {@link String}.
     * @throws IllegalStateException
     *             If a subclass calls this constructor.
     */
    public FieldNode(final int access, final String name, final String desc, final String signature, final Object value) {
        this(Opcodes.ASM6, access, name, desc, signature, value);
        if (getClass() != FieldNode.class) {
            throw new IllegalStateException();
        }
    }

    /**
     * Constructs a new {@link FieldNode}. <i>Subclasses must not use this
     * constructor</i>.
     *
     * @param api
     *            the ASM API version implemented by this visitor. Must be one
     *            of {@link Opcodes#ASM4} or {@link Opcodes#ASM5}.
     * @param access
     *            the field's access flags (see
     *            {@link jdk.internal.org.objectweb.asm.Opcodes}). This parameter also
     *            indicates if the field is synthetic and/or deprecated.
     * @param name
     *            the field's name.
     * @param desc
     *            the field's descriptor (see {@link jdk.internal.org.objectweb.asm.Type
     *            Type}).
     * @param signature
     *            the field's signature.
     * @param value
     *            the field's initial value. This parameter, which may be
     *            <tt>null</tt> if the field does not have an initial value,
     *            must be an {@link Integer}, a {@link Float}, a {@link Long}, a
     *            {@link Double} or a {@link String}.
     */
    public FieldNode(final int api, final int access, final String name, final String desc, final String signature, final Object value) {
        super(api);
        this.access = access;
        this.name = name;
        this.desc = desc;
        this.signature = signature;
        this.value = value;
    }

    // Implementation of the FieldVisitor abstract class

    @Override
    public AnnotationVisitor visitAnnotation(final String desc, final boolean visible) {
        AnnotationNode an = new AnnotationNode(desc);
        if (visible) {
            if (visibleAnnotations == null) {
                visibleAnnotations = new ArrayList<AnnotationNode>(1);
            }
            visibleAnnotations.add(an);
        } else {
            if (invisibleAnnotations == null) {
                invisibleAnnotations = new ArrayList<AnnotationNode>(1);
            }
            invisibleAnnotations.add(an);
        }
        return an;
    }

    @Override
    public AnnotationVisitor visitTypeAnnotation(int typeRef, TypePath typePath, String desc, boolean visible) {
        TypeAnnotationNode an = new TypeAnnotationNode(typeRef, typePath, desc);
        if (visible) {
            if (visibleTypeAnnotations == null) {
                visibleTypeAnnotations = new ArrayList<TypeAnnotationNode>(1);
            }
            visibleTypeAnnotations.add(an);
        } else {
            if (invisibleTypeAnnotations == null) {
                invisibleTypeAnnotations = new ArrayList<TypeAnnotationNode>(1);
            }
            invisibleTypeAnnotations.add(an);
        }
        return an;
    }

    @Override
    public void visitAttribute(final Attribute attr) {
        if (attrs == null) {
            attrs = new ArrayList<Attribute>(1);
        }
        attrs.add(attr);
    }

    @Override
    public void visitEnd() {
    }

    // Accept methods

    /**
     * Checks that this field node is compatible with the given ASM API version.
     * This methods checks that this node, and all its nodes recursively, do not
     * contain elements that were introduced in more recent versions of the ASM
     * API than the given version.
     *
     * @param api
     *            an ASM API version. Must be one of {@link Opcodes#ASM4},
     *            {@link Opcodes#ASM5} or {@link Opcodes#ASM6}.
     */
    public void check(final int api) {
        if (api == Opcodes.ASM4) {
            if (visibleTypeAnnotations != null && visibleTypeAnnotations.size() > 0) {
                throw new RuntimeException();
            }
            if (invisibleTypeAnnotations != null && invisibleTypeAnnotations.size() > 0) {
                throw new RuntimeException();
            }
        }
    }

    /**
     * Makes the given class visitor visit this field.
     *
     * @param cv
     *            a class visitor.
     */
    public void accept(final ClassVisitor cv) {
        FieldVisitor fv = cv.visitField(access, name, desc, signature, value);
        if (fv == null) {
            return;
        }
        int i, n;
        n = visibleAnnotations == null ? 0 : visibleAnnotations.size();
        for (i = 0; i < n; ++i) {
            AnnotationNode an = visibleAnnotations.get(i);
            an.accept(fv.visitAnnotation(an.desc, true));
        }
        n = invisibleAnnotations == null ? 0 : invisibleAnnotations.size();
        for (i = 0; i < n; ++i) {
            AnnotationNode an = invisibleAnnotations.get(i);
            an.accept(fv.visitAnnotation(an.desc, false));
        }
        n = visibleTypeAnnotations == null ? 0 : visibleTypeAnnotations.size();
        for (i = 0; i < n; ++i) {
            TypeAnnotationNode an = visibleTypeAnnotations.get(i);
            an.accept(fv.visitTypeAnnotation(an.typeRef, an.typePath, an.desc,
                    true));
        }
        n = invisibleTypeAnnotations == null ? 0 : invisibleTypeAnnotations.size();
        for (i = 0; i < n; ++i) {
            TypeAnnotationNode an = invisibleTypeAnnotations.get(i);
            an.accept(fv.visitTypeAnnotation(an.typeRef, an.typePath, an.desc,
                    false));
        }
        n = attrs == null ? 0 : attrs.size();
        for (i = 0; i < n; ++i) {
            fv.visitAttribute(attrs.get(i));
        }
        fv.visitEnd();
    }
}

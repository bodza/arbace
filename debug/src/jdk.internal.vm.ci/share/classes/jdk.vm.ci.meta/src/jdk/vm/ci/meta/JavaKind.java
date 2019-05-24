package jdk.vm.ci.meta;

import java.lang.reflect.Array;

/**
 * Denotes the basic kinds of types in CRI, including the all the Java primitive types, for example,
 * {@link JavaKind#Int} for {@code int} and {@link JavaKind#Object} for all object types. A kind has
 * a single character short name, a Java name, and a set of flags further describing its behavior.
 */
public enum JavaKind {
    /** The primitive boolean kind, represented as an int on the stack. */
    Boolean('Z', 4, "boolean", 1, true, java.lang.Boolean.TYPE, java.lang.Boolean.class),

    /** The primitive byte kind, represented as an int on the stack. */
    Byte('B', 8, "byte", 1, true, java.lang.Byte.TYPE, java.lang.Byte.class),

    /** The primitive short kind, represented as an int on the stack. */
    Short('S', 9, "short", 1, true, java.lang.Short.TYPE, java.lang.Short.class),

    /** The primitive char kind, represented as an int on the stack. */
    Char('C', 5, "char", 1, true, java.lang.Character.TYPE, java.lang.Character.class),

    /** The primitive int kind, represented as an int on the stack. */
    Int('I', 10, "int", 1, true, java.lang.Integer.TYPE, java.lang.Integer.class),

    /** The primitive float kind. */
    Float('F', 6, "float", 1, false, java.lang.Float.TYPE, java.lang.Float.class),

    /** The primitive long kind. */
    Long('J', 11, "long", 2, false, java.lang.Long.TYPE, java.lang.Long.class),

    /** The primitive double kind. */
    Double('D', 7, "double", 2, false, java.lang.Double.TYPE, java.lang.Double.class),

    /** The Object kind, also used for arrays. */
    Object('A', 12, "Object", 1, false, null, null),

    /** The void kind. */
    Void('V', 14, "void", 0, false, java.lang.Void.TYPE, java.lang.Void.class),

    /** The non-type. */
    Illegal('-', 99, "illegal", 0, false, null, null);

    private final char typeChar;
    private final String javaName;
    private final boolean isStackInt;
    private final Class<?> primitiveJavaClass;
    private final Class<?> boxedJavaClass;
    private final int slotCount;
    private final int basicType;

    JavaKind(char typeChar, int basicType, String javaName, int slotCount, boolean isStackInt, Class<?> primitiveJavaClass, Class<?> boxedJavaClass) {
        this.typeChar = typeChar;
        this.javaName = javaName;
        this.slotCount = slotCount;
        this.isStackInt = isStackInt;
        this.primitiveJavaClass = primitiveJavaClass;
        this.boxedJavaClass = boxedJavaClass;
        this.basicType = basicType;
    }

    /**
     * Returns the number of stack slots occupied by this kind according to the Java bytecodes
     * specification.
     */
    public int getSlotCount() {
        return this.slotCount;
    }

    /**
     * Returns whether this kind occupied two stack slots.
     */
    public boolean needsTwoSlots() {
        return this.slotCount == 2;
    }

    /**
     * Returns the kind that represents this kind when on the Java operand stack.
     *
     * @return the kind used on the operand stack
     */
    public JavaKind getStackKind() {
        if (isStackInt) {
            return Int;
        }
        return this;
    }

    /**
     * Checks whether this represent an Object of some sort.
     *
     * @return {@code true} if this is {@link #Object}.
     */
    public boolean isObject() {
        return this == JavaKind.Object;
    }

    /**
     * Returns the kind of a word given the size of a word in bytes.
     *
     * @param wordSizeInBytes the size of a word in bytes
     * @return the kind representing a word value
     */
    public static JavaKind fromWordSize(int wordSizeInBytes) {
        if (wordSizeInBytes == 8) {
            return JavaKind.Long;
        } else {
            return JavaKind.Int;
        }
    }

    /**
     * Returns the Kind representing the given Java class.
     *
     * @param klass the class
     * @return the kind
     */
    public static JavaKind fromJavaClass(Class<?> klass) {
        if (klass == Boolean.primitiveJavaClass) {
            return Boolean;
        } else if (klass == Byte.primitiveJavaClass) {
            return Byte;
        } else if (klass == Short.primitiveJavaClass) {
            return Short;
        } else if (klass == Char.primitiveJavaClass) {
            return Char;
        } else if (klass == Int.primitiveJavaClass) {
            return Int;
        } else if (klass == Long.primitiveJavaClass) {
            return Long;
        } else if (klass == Float.primitiveJavaClass) {
            return Float;
        } else if (klass == Double.primitiveJavaClass) {
            return Double;
        } else if (klass == Void.primitiveJavaClass) {
            return Void;
        } else {
            return Object;
        }
    }

    /**
     * Gets the minimum value that can be represented as a value of this kind.
     *
     * @return the minimum value represented as a {@code long}
     */
    public long getMinValue() {
        switch (this) {
            case Boolean:
                return 0;
            case Byte:
                return java.lang.Byte.MIN_VALUE;
            case Char:
                return java.lang.Character.MIN_VALUE;
            case Short:
                return java.lang.Short.MIN_VALUE;
            case Int:
                return java.lang.Integer.MIN_VALUE;
            case Long:
                return java.lang.Long.MIN_VALUE;
            case Float:
                return java.lang.Float.floatToRawIntBits(java.lang.Float.MIN_VALUE);
            case Double:
                return java.lang.Double.doubleToRawLongBits(java.lang.Double.MIN_VALUE);
            default:
                throw new IllegalArgumentException("illegal call to minValue on " + this);
        }
    }

    /**
     * Gets the maximum value that can be represented as a value of this kind.
     *
     * @return the maximum value represented as a {@code long}
     */
    public long getMaxValue() {
        switch (this) {
            case Boolean:
                return 1;
            case Byte:
                return java.lang.Byte.MAX_VALUE;
            case Char:
                return java.lang.Character.MAX_VALUE;
            case Short:
                return java.lang.Short.MAX_VALUE;
            case Int:
                return java.lang.Integer.MAX_VALUE;
            case Long:
                return java.lang.Long.MAX_VALUE;
            case Float:
                return java.lang.Float.floatToRawIntBits(java.lang.Float.MAX_VALUE);
            case Double:
                return java.lang.Double.doubleToRawLongBits(java.lang.Double.MAX_VALUE);
            default:
                throw new IllegalArgumentException("illegal call to maxValue on " + this);
        }
    }

    /**
     * Number of bytes that are necessary to represent a value of this kind.
     *
     * @return the number of bytes
     */
    public int getByteCount() {
        if (this == Boolean) {
            return 1;
        } else {
            return getBitCount() >> 3;
        }
    }

    /**
     * Number of bits that are necessary to represent a value of this kind.
     *
     * @return the number of bits
     */
    public int getBitCount() {
        switch (this) {
            case Boolean:
                return 1;
            case Byte:
                return 8;
            case Char:
            case Short:
                return 16;
            case Float:
                return 32;
            case Int:
                return 32;
            case Double:
                return 64;
            case Long:
                return 64;
            default:
                throw new IllegalArgumentException("illegal call to bits on " + this);
        }
    }
}

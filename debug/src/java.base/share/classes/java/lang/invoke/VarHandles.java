package java.lang.invoke;

import static java.lang.invoke.MethodHandleStatics.UNSAFE;

final class VarHandles {
    static VarHandle makeFieldHandle(MemberName f, Class<?> refc, Class<?> type, boolean isWriteAllowedOnFinalFields) {
        if (!f.isStatic()) {
            long foffset = MethodHandleNatives.objectFieldOffset(f);
            if (!type.isPrimitive()) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleObjects.FieldInstanceReadOnly(refc, foffset, type)
                       : new VarHandleObjects.FieldInstanceReadWrite(refc, foffset, type);
            }
            else if (type == boolean.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleBooleans.FieldInstanceReadOnly(refc, foffset)
                       : new VarHandleBooleans.FieldInstanceReadWrite(refc, foffset);
            }
            else if (type == byte.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleBytes.FieldInstanceReadOnly(refc, foffset)
                       : new VarHandleBytes.FieldInstanceReadWrite(refc, foffset);
            }
            else if (type == short.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleShorts.FieldInstanceReadOnly(refc, foffset)
                       : new VarHandleShorts.FieldInstanceReadWrite(refc, foffset);
            }
            else if (type == char.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleChars.FieldInstanceReadOnly(refc, foffset)
                       : new VarHandleChars.FieldInstanceReadWrite(refc, foffset);
            }
            else if (type == int.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleInts.FieldInstanceReadOnly(refc, foffset)
                       : new VarHandleInts.FieldInstanceReadWrite(refc, foffset);
            }
            else if (type == long.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleLongs.FieldInstanceReadOnly(refc, foffset)
                       : new VarHandleLongs.FieldInstanceReadWrite(refc, foffset);
            }
            else if (type == float.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleFloats.FieldInstanceReadOnly(refc, foffset)
                       : new VarHandleFloats.FieldInstanceReadWrite(refc, foffset);
            }
            else if (type == double.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleDoubles.FieldInstanceReadOnly(refc, foffset)
                       : new VarHandleDoubles.FieldInstanceReadWrite(refc, foffset);
            }
            else {
                throw new UnsupportedOperationException();
            }
        }
        else {
            // TODO This is not lazy on first invocation
            // and might cause some circular initialization issues

            // Replace with something similar to direct method handles
            // where a barrier is used then elided after use

            if (UNSAFE.shouldBeInitialized(refc))
                UNSAFE.ensureClassInitialized(refc);

            Object base = MethodHandleNatives.staticFieldBase(f);
            long foffset = MethodHandleNatives.staticFieldOffset(f);
            if (!type.isPrimitive()) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleObjects.FieldStaticReadOnly(base, foffset, type)
                       : new VarHandleObjects.FieldStaticReadWrite(base, foffset, type);
            }
            else if (type == boolean.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleBooleans.FieldStaticReadOnly(base, foffset)
                       : new VarHandleBooleans.FieldStaticReadWrite(base, foffset);
            }
            else if (type == byte.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleBytes.FieldStaticReadOnly(base, foffset)
                       : new VarHandleBytes.FieldStaticReadWrite(base, foffset);
            }
            else if (type == short.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleShorts.FieldStaticReadOnly(base, foffset)
                       : new VarHandleShorts.FieldStaticReadWrite(base, foffset);
            }
            else if (type == char.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleChars.FieldStaticReadOnly(base, foffset)
                       : new VarHandleChars.FieldStaticReadWrite(base, foffset);
            }
            else if (type == int.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleInts.FieldStaticReadOnly(base, foffset)
                       : new VarHandleInts.FieldStaticReadWrite(base, foffset);
            }
            else if (type == long.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleLongs.FieldStaticReadOnly(base, foffset)
                       : new VarHandleLongs.FieldStaticReadWrite(base, foffset);
            }
            else if (type == float.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleFloats.FieldStaticReadOnly(base, foffset)
                       : new VarHandleFloats.FieldStaticReadWrite(base, foffset);
            }
            else if (type == double.class) {
                return f.isFinal() && !isWriteAllowedOnFinalFields
                       ? new VarHandleDoubles.FieldStaticReadOnly(base, foffset)
                       : new VarHandleDoubles.FieldStaticReadWrite(base, foffset);
            }
            else {
                throw new UnsupportedOperationException();
            }
        }
    }

    static VarHandle makeArrayElementHandle(Class<?> arrayClass) {
        if (!arrayClass.isArray())
            throw new IllegalArgumentException("not an array: " + arrayClass);

        Class<?> componentType = arrayClass.getComponentType();

        int aoffset = UNSAFE.arrayBaseOffset(arrayClass);
        int ascale = UNSAFE.arrayIndexScale(arrayClass);
        int ashift = 31 - Integer.numberOfLeadingZeros(ascale);

        if (!componentType.isPrimitive()) {
            return new VarHandleObjects.Array(aoffset, ashift, arrayClass);
        }
        else if (componentType == boolean.class) {
            return new VarHandleBooleans.Array(aoffset, ashift);
        }
        else if (componentType == byte.class) {
            return new VarHandleBytes.Array(aoffset, ashift);
        }
        else if (componentType == short.class) {
            return new VarHandleShorts.Array(aoffset, ashift);
        }
        else if (componentType == char.class) {
            return new VarHandleChars.Array(aoffset, ashift);
        }
        else if (componentType == int.class) {
            return new VarHandleInts.Array(aoffset, ashift);
        }
        else if (componentType == long.class) {
            return new VarHandleLongs.Array(aoffset, ashift);
        }
        else if (componentType == float.class) {
            return new VarHandleFloats.Array(aoffset, ashift);
        }
        else if (componentType == double.class) {
            return new VarHandleDoubles.Array(aoffset, ashift);
        }
        else {
            throw new UnsupportedOperationException();
        }
    }

    static VarHandle byteArrayViewHandle(Class<?> viewArrayClass, boolean be) {
        if (!viewArrayClass.isArray())
            throw new IllegalArgumentException("not an array: " + viewArrayClass);

        Class<?> viewComponentType = viewArrayClass.getComponentType();

        if (viewComponentType == long.class) {
            return new VarHandleByteArrayAsLongs.ArrayHandle(be);
        }
        else if (viewComponentType == int.class) {
            return new VarHandleByteArrayAsInts.ArrayHandle(be);
        }
        else if (viewComponentType == short.class) {
            return new VarHandleByteArrayAsShorts.ArrayHandle(be);
        }
        else if (viewComponentType == char.class) {
            return new VarHandleByteArrayAsChars.ArrayHandle(be);
        }
        else if (viewComponentType == double.class) {
            return new VarHandleByteArrayAsDoubles.ArrayHandle(be);
        }
        else if (viewComponentType == float.class) {
            return new VarHandleByteArrayAsFloats.ArrayHandle(be);
        }

        throw new UnsupportedOperationException();
    }

    static VarHandle makeByteBufferViewHandle(Class<?> viewArrayClass, boolean be) {
        if (!viewArrayClass.isArray())
            throw new IllegalArgumentException("not an array: " + viewArrayClass);

        Class<?> viewComponentType = viewArrayClass.getComponentType();

        if (viewComponentType == long.class) {
            return new VarHandleByteArrayAsLongs.ByteBufferHandle(be);
        }
        else if (viewComponentType == int.class) {
            return new VarHandleByteArrayAsInts.ByteBufferHandle(be);
        }
        else if (viewComponentType == short.class) {
            return new VarHandleByteArrayAsShorts.ByteBufferHandle(be);
        }
        else if (viewComponentType == char.class) {
            return new VarHandleByteArrayAsChars.ByteBufferHandle(be);
        }
        else if (viewComponentType == double.class) {
            return new VarHandleByteArrayAsDoubles.ByteBufferHandle(be);
        }
        else if (viewComponentType == float.class) {
            return new VarHandleByteArrayAsFloats.ByteBufferHandle(be);
        }

        throw new UnsupportedOperationException();
    }
}

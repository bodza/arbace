package jdk.internal.reflect;

import java.lang.reflect.Field;

class UnsafeQualifiedDoubleFieldAccessorImpl extends UnsafeQualifiedFieldAccessorImpl {
    UnsafeQualifiedDoubleFieldAccessorImpl(Field field, boolean isReadOnly) {
        super(field, isReadOnly);
    }

    public Object get(Object obj) throws IllegalArgumentException {
        return Double.valueOf(getDouble(obj));
    }

    public boolean getBoolean(Object obj) throws IllegalArgumentException {
        throw newGetBooleanIllegalArgumentException();
    }

    public byte getByte(Object obj) throws IllegalArgumentException {
        throw newGetByteIllegalArgumentException();
    }

    public char getChar(Object obj) throws IllegalArgumentException {
        throw newGetCharIllegalArgumentException();
    }

    public short getShort(Object obj) throws IllegalArgumentException {
        throw newGetShortIllegalArgumentException();
    }

    public int getInt(Object obj) throws IllegalArgumentException {
        throw newGetIntIllegalArgumentException();
    }

    public long getLong(Object obj) throws IllegalArgumentException {
        throw newGetLongIllegalArgumentException();
    }

    public float getFloat(Object obj) throws IllegalArgumentException {
        throw newGetFloatIllegalArgumentException();
    }

    public double getDouble(Object obj) throws IllegalArgumentException {
        ensureObj(obj);
        return unsafe.getDoubleVolatile(obj, fieldOffset);
    }

    public void set(Object obj, Object value) throws IllegalArgumentException, IllegalAccessException {
        ensureObj(obj);
        if (isReadOnly) {
            throwFinalFieldIllegalAccessException(value);
        }
        if (value == null) {
            throwSetIllegalArgumentException(value);
        }
        if (value instanceof Byte) {
            unsafe.putDoubleVolatile(obj, fieldOffset, ((Byte) value).byteValue());
            return;
        }
        if (value instanceof Short) {
            unsafe.putDoubleVolatile(obj, fieldOffset, ((Short) value).shortValue());
            return;
        }
        if (value instanceof Character) {
            unsafe.putDoubleVolatile(obj, fieldOffset, ((Character) value).charValue());
            return;
        }
        if (value instanceof Integer) {
            unsafe.putDoubleVolatile(obj, fieldOffset, ((Integer) value).intValue());
            return;
        }
        if (value instanceof Long) {
            unsafe.putDoubleVolatile(obj, fieldOffset, ((Long) value).longValue());
            return;
        }
        if (value instanceof Float) {
            unsafe.putDoubleVolatile(obj, fieldOffset, ((Float) value).floatValue());
            return;
        }
        if (value instanceof Double) {
            unsafe.putDoubleVolatile(obj, fieldOffset, ((Double) value).doubleValue());
            return;
        }
        throwSetIllegalArgumentException(value);
    }

    public void setBoolean(Object obj, boolean z) throws IllegalArgumentException, IllegalAccessException {
        throwSetIllegalArgumentException(z);
    }

    public void setByte(Object obj, byte b) throws IllegalArgumentException, IllegalAccessException {
        setDouble(obj, b);
    }

    public void setChar(Object obj, char c) throws IllegalArgumentException, IllegalAccessException {
        setDouble(obj, c);
    }

    public void setShort(Object obj, short s) throws IllegalArgumentException, IllegalAccessException {
        setDouble(obj, s);
    }

    public void setInt(Object obj, int i) throws IllegalArgumentException, IllegalAccessException {
        setDouble(obj, i);
    }

    public void setLong(Object obj, long l) throws IllegalArgumentException, IllegalAccessException {
        setDouble(obj, l);
    }

    public void setFloat(Object obj, float f) throws IllegalArgumentException, IllegalAccessException {
        setDouble(obj, f);
    }

    public void setDouble(Object obj, double d) throws IllegalArgumentException, IllegalAccessException {
        ensureObj(obj);
        if (isReadOnly) {
            throwFinalFieldIllegalAccessException(d);
        }
        unsafe.putDoubleVolatile(obj, fieldOffset, d);
    }
}
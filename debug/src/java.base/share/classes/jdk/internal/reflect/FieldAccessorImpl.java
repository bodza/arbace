package jdk.internal.reflect;

/**
  * Package-private implementation of the FieldAccessor interface
  * which has access to all classes and all fields, regardless of
  * language restrictions. See MagicAccessorImpl.
  */
abstract class FieldAccessorImpl extends MagicAccessorImpl implements FieldAccessor {
    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract Object get(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract boolean getBoolean(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract byte getByte(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract char getChar(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract short getShort(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract int getInt(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract long getLong(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract float getFloat(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract double getDouble(Object obj) throws IllegalArgumentException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void set(Object obj, Object value) throws IllegalArgumentException, IllegalAccessException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void setBoolean(Object obj, boolean z) throws IllegalArgumentException, IllegalAccessException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void setByte(Object obj, byte b) throws IllegalArgumentException, IllegalAccessException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void setChar(Object obj, char c) throws IllegalArgumentException, IllegalAccessException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void setShort(Object obj, short s) throws IllegalArgumentException, IllegalAccessException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void setInt(Object obj, int i) throws IllegalArgumentException, IllegalAccessException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void setLong(Object obj, long l) throws IllegalArgumentException, IllegalAccessException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void setFloat(Object obj, float f) throws IllegalArgumentException, IllegalAccessException;

    /** Matches specification in {@link java.lang.reflect.Field} */
    public abstract void setDouble(Object obj, double d) throws IllegalArgumentException, IllegalAccessException;
}

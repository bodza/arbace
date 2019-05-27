package java.lang.reflect;

import jdk.internal.reflect.FieldAccessor;
import jdk.internal.reflect.Reflection;
import sun.reflect.generics.repository.FieldRepository;
import sun.reflect.generics.factory.CoreReflectionFactory;
import sun.reflect.generics.factory.GenericsFactory;
import sun.reflect.generics.scope.ClassScope;
import java.util.Map;
import java.util.Objects;

/**
 * A {@code Field} provides information about, and dynamic access to, a
 * single field of a class or an interface.  The reflected field may
 * be a class (static) field or an instance field.
 *
 * A {@code Field} permits widening conversions to occur during a get or
 * set access operation, but throws an {@code IllegalArgumentException} if a
 * narrowing conversion would occur.
 */
public final class Field extends AccessibleObject implements Member {
    private Class<?>            clazz;
    private int                 slot;
    // This is guaranteed to be interned by the VM in the 1.4
    // reflection implementation
    private String              name;
    private Class<?>            type;
    private int                 modifiers;
    // Generics support
    private transient String    signature;
    // generic info repository; lazily initialized
    private transient FieldRepository genericInfo;
    // Cached field accessor created without override
    private FieldAccessor fieldAccessor;
    // For sharing of FieldAccessors. This branching structure is
    // currently only two levels deep (i.e., one root Field and
    // potentially many Field objects pointing to it.)
    //
    // If this branching structure would ever contain cycles, deadlocks can
    // occur in annotation code.
    private Field               root;

    // Generics infrastructure

    private String getGenericSignature() {return signature;}

    // Accessor for factory
    private GenericsFactory getFactory() {
        Class<?> c = getDeclaringClass();
        // create scope and factory
        return CoreReflectionFactory.make(c, ClassScope.make(c));
    }

    // Accessor for generic info repository
    private FieldRepository getGenericInfo() {
        // lazily initialize repository if necessary
        if (genericInfo == null) {
            // create and cache generic info repository
            genericInfo = FieldRepository.make(getGenericSignature(), getFactory());
        }
        return genericInfo; // return cached repository
    }

    /**
     * Package-private constructor used by ReflectAccess to enable
     * instantiation of these objects in Java code from the java.lang
     * package via sun.reflect.LangReflectAccess.
     */
    Field(Class<?> declaringClass, String name, Class<?> type, int modifiers, int slot, String signature) {
        this.clazz = declaringClass;
        this.name = name;
        this.type = type;
        this.modifiers = modifiers;
        this.slot = slot;
        this.signature = signature;
    }

    /**
     * Package-private routine (exposed to java.lang.Class via
     * ReflectAccess) which returns a copy of this Field. The copy's
     * "root" field points to this Field.
     */
    Field copy() {
        // This routine enables sharing of FieldAccessor objects
        // among Field objects which refer to the same underlying
        // method in the VM. (All of this contortion is only necessary
        // because of the "accessibility" bit in AccessibleObject,
        // which implicitly requires that new java.lang.reflect
        // objects be fabricated for each reflective call on Class
        // objects.)
        if (this.root != null)
            throw new IllegalArgumentException("Can not copy a non-root Field");

        Field res = new Field(clazz, name, type, modifiers, slot, signature);
        res.root = this;
        // Might as well eagerly propagate this if already present
        res.fieldAccessor = fieldAccessor;

        return res;
    }

    /**
     * Returns the {@code Class} object representing the class or interface
     * that declares the field represented by this {@code Field} object.
     */
    // @Override
    public Class<?> getDeclaringClass() {
        return clazz;
    }

    /**
     * Returns the name of the field represented by this {@code Field} object.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the Java language modifiers for the field represented
     * by this {@code Field} object, as an integer. The {@code Modifier} class should
     * be used to decode the modifiers.
     */
    public int getModifiers() {
        return modifiers;
    }

    /**
     * Returns {@code true} if this field represents an element of
     * an enumerated type; returns {@code false} otherwise.
     *
     * @return {@code true} if and only if this field represents an element of
     * an enumerated type.
     */
    public boolean isEnumConstant() {
        return (getModifiers() & Modifier.ENUM) != 0;
    }

    /**
     * Returns {@code true} if this field is a synthetic
     * field; returns {@code false} otherwise.
     *
     * @return true if and only if this field is a synthetic
     * field as defined by the Java Language Specification.
     */
    public boolean isSynthetic() {
        return Modifier.isSynthetic(getModifiers());
    }

    /**
     * Returns a {@code Class} object that identifies the
     * declared type for the field represented by this
     * {@code Field} object.
     *
     * @return a {@code Class} object identifying the declared
     * type of the field represented by this object
     */
    public Class<?> getType() {
        return type;
    }

    /**
     * Returns a {@code Type} object that represents the declared type for
     * the field represented by this {@code Field} object.
     *
     * If the {@code Type} is a parameterized type, the
     * {@code Type} object returned must accurately reflect the
     * actual type parameters used in the source code.
     *
     * If the type of the underlying field is a type variable or a
     * parameterized type, it is created. Otherwise, it is resolved.
     *
     * @return a {@code Type} object that represents the declared type for
     *     the field represented by this {@code Field} object
     * @throws GenericSignatureFormatError if the generic field
     *     signature does not conform to the format specified in
     *     <cite>The Java&trade; Virtual Machine Specification</cite>
     * @throws TypeNotPresentException if the generic type
     *     signature of the underlying field refers to a non-existent
     *     type declaration
     * @throws MalformedParameterizedTypeException if the generic
     *     signature of the underlying field refers to a parameterized type
     *     that cannot be instantiated for any reason
     */
    public Type getGenericType() {
        if (getGenericSignature() != null)
            return getGenericInfo().getGenericType();
        else
            return getType();
    }

    /**
     * Compares this {@code Field} against the specified object.  Returns
     * true if the objects are the same.  Two {@code Field} objects are the same if
     * they were declared by the same class and have the same name
     * and type.
     */
    public boolean equals(Object obj) {
        if (obj != null && obj instanceof Field) {
            Field other = (Field)obj;
            return (getDeclaringClass() == other.getDeclaringClass())
                && (getName() == other.getName())
                && (getType() == other.getType());
        }
        return false;
    }

    /**
     * Returns a hashcode for this {@code Field}.  This is computed as the
     * exclusive-or of the hashcodes for the underlying field's
     * declaring class name and its name.
     */
    public int hashCode() {
        return getDeclaringClass().getName().hashCode() ^ getName().hashCode();
    }

    /**
     * Returns a string describing this {@code Field}.  The format is
     * the access modifiers for the field, if any, followed
     * by the field type, followed by a space, followed by
     * the fully-qualified name of the class declaring the field,
     * followed by a period, followed by the name of the field.
     * For example:
     * <pre>
     *    public static final int java.lang.Thread.MIN_PRIORITY
     *    private int java.io.FileDescriptor.fd
     * </pre>
     *
     * The modifiers are placed in canonical order as specified by
     * "The Java Language Specification".  This is {@code public},
     * {@code protected} or {@code private} first, and then other
     * modifiers in the following order: {@code static}, {@code final},
     * {@code transient}, {@code volatile}.
     *
     * @return a string describing this {@code Field}
     */
    public String toString() {
        int mod = getModifiers();
        return String.str(((mod == 0) ? "" : String.str(Modifier.toString(mod), " ")), getType().getTypeName(), " ", getDeclaringClass().getTypeName(), ".", getName());
    }

    /**
     * Returns a string describing this {@code Field}, including
     * its generic type.  The format is the access modifiers for the
     * field, if any, followed by the generic field type, followed by
     * a space, followed by the fully-qualified name of the class
     * declaring the field, followed by a period, followed by the name
     * of the field.
     *
     * The modifiers are placed in canonical order as specified by
     * "The Java Language Specification".  This is {@code public},
     * {@code protected} or {@code private} first, and then other
     * modifiers in the following order: {@code static}, {@code final},
     * {@code transient}, {@code volatile}.
     *
     * @return a string describing this {@code Field}, including
     * its generic type
     */
    public String toGenericString() {
        int mod = getModifiers();
        Type fieldType = getGenericType();
        return String.str(((mod == 0) ? "" : String.str(Modifier.toString(mod), " ")), fieldType.getTypeName(), " ", getDeclaringClass().getTypeName(), ".", getName());
    }

    /**
     * Returns the value of the field represented by this {@code Field}, on
     * the specified object. The value is automatically wrapped in an
     * object if it has a primitive type.
     *
     * The underlying field's value is obtained as follows:
     *
     * If the underlying field is a static field, the {@code obj} argument
     * is ignored; it may be null.
     *
     * Otherwise, the underlying field is an instance field.  If the
     * specified {@code obj} argument is null, the method throws a
     * {@code NullPointerException}. If the specified object is not an
     * instance of the class or interface declaring the underlying
     * field, the method throws an {@code IllegalArgumentException}.
     *
     * If this {@code Field} object is enforcing Java language access control, and
     * the underlying field is inaccessible, the method throws an
     * {@code IllegalAccessException}.
     * If the underlying field is static, the class that declared the
     * field is initialized if it has not already been initialized.
     *
     * Otherwise, the value is retrieved from the underlying instance
     * or static field.  If the field has a primitive type, the value
     * is wrapped in an object before being returned, otherwise it is
     * returned as is.
     *
     * If the field is hidden in the type of {@code obj},
     * the field's value is obtained according to the preceding rules.
     *
     * @param obj object from which the represented field's value is
     * to be extracted
     * @return the value of the represented field in object
     * {@code obj}; primitive values are wrapped in an appropriate
     * object before being returned
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof).
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public Object get(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).get(obj);
    }

    /**
     * Gets the value of a static or instance {@code boolean} field.
     *
     * @param obj the object to extract the {@code boolean} value from
     * @return the value of the {@code boolean} field
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not
     *              an instance of the class or interface declaring the
     *              underlying field (or a subclass or implementor
     *              thereof), or if the field value cannot be
     *              converted to the type {@code boolean} by a
     *              widening conversion.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public boolean getBoolean(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).getBoolean(obj);
    }

    /**
     * Gets the value of a static or instance {@code byte} field.
     *
     * @param obj the object to extract the {@code byte} value from
     * @return the value of the {@code byte} field
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not
     *              an instance of the class or interface declaring the
     *              underlying field (or a subclass or implementor
     *              thereof), or if the field value cannot be
     *              converted to the type {@code byte} by a
     *              widening conversion.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public byte getByte(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).getByte(obj);
    }

    /**
     * Gets the value of a static or instance field of type
     * {@code char} or of another primitive type convertible to
     * type {@code char} via a widening conversion.
     *
     * @param obj the object to extract the {@code char} value from
     * @return the value of the field converted to type {@code char}
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not
     *              an instance of the class or interface declaring the
     *              underlying field (or a subclass or implementor
     *              thereof), or if the field value cannot be
     *              converted to the type {@code char} by a
     *              widening conversion.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public char getChar(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).getChar(obj);
    }

    /**
     * Gets the value of a static or instance field of type
     * {@code short} or of another primitive type convertible to
     * type {@code short} via a widening conversion.
     *
     * @param obj the object to extract the {@code short} value from
     * @return the value of the field converted to type {@code short}
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not
     *              an instance of the class or interface declaring the
     *              underlying field (or a subclass or implementor
     *              thereof), or if the field value cannot be
     *              converted to the type {@code short} by a
     *              widening conversion.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public short getShort(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).getShort(obj);
    }

    /**
     * Gets the value of a static or instance field of type
     * {@code int} or of another primitive type convertible to
     * type {@code int} via a widening conversion.
     *
     * @param obj the object to extract the {@code int} value from
     * @return the value of the field converted to type {@code int}
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not
     *              an instance of the class or interface declaring the
     *              underlying field (or a subclass or implementor
     *              thereof), or if the field value cannot be
     *              converted to the type {@code int} by a
     *              widening conversion.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public int getInt(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).getInt(obj);
    }

    /**
     * Gets the value of a static or instance field of type
     * {@code long} or of another primitive type convertible to
     * type {@code long} via a widening conversion.
     *
     * @param obj the object to extract the {@code long} value from
     * @return the value of the field converted to type {@code long}
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not
     *              an instance of the class or interface declaring the
     *              underlying field (or a subclass or implementor
     *              thereof), or if the field value cannot be
     *              converted to the type {@code long} by a
     *              widening conversion.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public long getLong(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).getLong(obj);
    }

    /**
     * Gets the value of a static or instance field of type
     * {@code float} or of another primitive type convertible to
     * type {@code float} via a widening conversion.
     *
     * @param obj the object to extract the {@code float} value from
     * @return the value of the field converted to type {@code float}
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not
     *              an instance of the class or interface declaring the
     *              underlying field (or a subclass or implementor
     *              thereof), or if the field value cannot be
     *              converted to the type {@code float} by a
     *              widening conversion.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public float getFloat(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).getFloat(obj);
    }

    /**
     * Gets the value of a static or instance field of type
     * {@code double} or of another primitive type convertible to
     * type {@code double} via a widening conversion.
     *
     * @param obj the object to extract the {@code double} value from
     * @return the value of the field converted to type {@code double}
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is inaccessible.
     * @throws IllegalArgumentException  if the specified object is not
     *              an instance of the class or interface declaring the
     *              underlying field (or a subclass or implementor
     *              thereof), or if the field value cannot be
     *              converted to the type {@code double} by a
     *              widening conversion.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public double getDouble(Object obj) throws IllegalArgumentException, IllegalAccessException {
        return getFieldAccessor(obj).getDouble(obj);
    }

    /**
     * Sets the field represented by this {@code Field} object on the
     * specified object argument to the specified new value. The new
     * value is automatically unwrapped if the underlying field has a
     * primitive type.
     *
     * The operation proceeds as follows:
     *
     * If the underlying field is static, the {@code obj} argument is
     * ignored; it may be null.
     *
     * Otherwise the underlying field is an instance field.  If the
     * specified object argument is null, the method throws a
     * {@code NullPointerException}.  If the specified object argument is not
     * an instance of the class or interface declaring the underlying
     * field, the method throws an {@code IllegalArgumentException}.
     *
     * If this {@code Field} object is enforcing Java language access control, and
     * the underlying field is inaccessible, the method throws an
     * {@code IllegalAccessException}.
     *
     * If the underlying field is of a primitive type, an unwrapping
     * conversion is attempted to convert the new value to a value of
     * a primitive type.  If this attempt fails, the method throws an
     * {@code IllegalArgumentException}.
     *
     * If, after possible unwrapping, the new value cannot be
     * converted to the type of the underlying field by an identity or
     * widening conversion, the method throws an
     * {@code IllegalArgumentException}.
     *
     * If the underlying field is static, the class that declared the
     * field is initialized if it has not already been initialized.
     *
     * The field is set to the possibly unwrapped and widened new value.
     *
     * If the field is hidden in the type of {@code obj},
     * the field's value is set according to the preceding rules.
     *
     * @param obj the object whose field should be modified
     * @param value the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void set(Object obj, Object value) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).set(obj, value);
    }

    /**
     * Sets the value of a field as a {@code boolean} on the specified object.
     * This method is equivalent to
     * {@code set(obj, zObj)},
     * where {@code zObj} is a {@code Boolean} object and
     * {@code zObj.booleanValue() == z}.
     *
     * @param obj the object whose field should be modified
     * @param z   the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void setBoolean(Object obj, boolean z) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).setBoolean(obj, z);
    }

    /**
     * Sets the value of a field as a {@code byte} on the specified object.
     * This method is equivalent to
     * {@code set(obj, bObj)},
     * where {@code bObj} is a {@code Byte} object and
     * {@code bObj.byteValue() == b}.
     *
     * @param obj the object whose field should be modified
     * @param b   the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void setByte(Object obj, byte b) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).setByte(obj, b);
    }

    /**
     * Sets the value of a field as a {@code char} on the specified object.
     * This method is equivalent to
     * {@code set(obj, cObj)},
     * where {@code cObj} is a {@code Character} object and
     * {@code cObj.charValue() == c}.
     *
     * @param obj the object whose field should be modified
     * @param c   the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void setChar(Object obj, char c) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).setChar(obj, c);
    }

    /**
     * Sets the value of a field as a {@code short} on the specified object.
     * This method is equivalent to
     * {@code set(obj, sObj)},
     * where {@code sObj} is a {@code Short} object and
     * {@code sObj.shortValue() == s}.
     *
     * @param obj the object whose field should be modified
     * @param s   the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void setShort(Object obj, short s) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).setShort(obj, s);
    }

    /**
     * Sets the value of a field as an {@code int} on the specified object.
     * This method is equivalent to
     * {@code set(obj, iObj)},
     * where {@code iObj} is an {@code Integer} object and
     * {@code iObj.intValue() == i}.
     *
     * @param obj the object whose field should be modified
     * @param i   the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void setInt(Object obj, int i) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).setInt(obj, i);
    }

    /**
     * Sets the value of a field as a {@code long} on the specified object.
     * This method is equivalent to
     * {@code set(obj, lObj)},
     * where {@code lObj} is a {@code Long} object and
     * {@code lObj.longValue() == l}.
     *
     * @param obj the object whose field should be modified
     * @param l   the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void setLong(Object obj, long l) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).setLong(obj, l);
    }

    /**
     * Sets the value of a field as a {@code float} on the specified object.
     * This method is equivalent to
     * {@code set(obj, fObj)},
     * where {@code fObj} is a {@code Float} object and
     * {@code fObj.floatValue() == f}.
     *
     * @param obj the object whose field should be modified
     * @param f   the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void setFloat(Object obj, float f) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).setFloat(obj, f);
    }

    /**
     * Sets the value of a field as a {@code double} on the specified object.
     * This method is equivalent to
     * {@code set(obj, dObj)},
     * where {@code dObj} is a {@code Double} object and
     * {@code dObj.doubleValue() == d}.
     *
     * @param obj the object whose field should be modified
     * @param d   the new value for the field of {@code obj}
     * being modified
     *
     * @throws IllegalAccessException    if this {@code Field} object
     *              is enforcing Java language access control and the underlying
     *              field is either inaccessible or final.
     * @throws IllegalArgumentException  if the specified object is not an
     *              instance of the class or interface declaring the underlying
     *              field (or a subclass or implementor thereof),
     *              or if an unwrapping conversion fails.
     * @throws NullPointerException      if the specified object is null
     *              and the field is an instance field.
     * @throws ExceptionInInitializerError if the initialization provoked
     *              by this method fails.
     */
    // @CallerSensitive
    // @ForceInline // to ensure Reflection.getCallerClass optimization
    public void setDouble(Object obj, double d) throws IllegalArgumentException, IllegalAccessException {
        getFieldAccessor(obj).setDouble(obj, d);
    }

    // security check is done before calling this method
    private FieldAccessor getFieldAccessor(Object obj) throws IllegalAccessException {
        FieldAccessor a = fieldAccessor;
        return (a != null) ? a : acquireFieldAccessor();
    }

    // NOTE that there is no synchronization used here. It is correct
    // (though not efficient) to generate more than one FieldAccessor
    // for a given Field. However, avoiding synchronization will
    // probably make the implementation more scalable.
    private FieldAccessor acquireFieldAccessor() {
        // First check to see if one has been created yet, and take it
        // if so
        FieldAccessor tmp = null;
        if (root != null)
            tmp = root.getFieldAccessor();
        if (tmp != null) {
            fieldAccessor = tmp;
        } else {
            // Otherwise fabricate one and propagate it up to the root
            tmp = reflectionFactory.newFieldAccessor(this, false);
            setFieldAccessor(tmp);
        }

        return tmp;
    }

    // Returns FieldAccessor for this Field object, not looking up
    // the chain to the root
    private FieldAccessor getFieldAccessor() {
        return fieldAccessor;
    }

    // Sets the FieldAccessor for this Field object and
    // (recursively) its root
    private void setFieldAccessor(FieldAccessor accessor) {
        fieldAccessor = accessor;
        // Propagate up
        if (root != null) {
            root.setFieldAccessor(accessor);
        }
    }

    // @Override
    Field getRoot() {
        return root;
    }
}

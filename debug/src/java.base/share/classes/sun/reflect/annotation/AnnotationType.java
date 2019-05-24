package sun.reflect.annotation;

import java.lang.annotation.*;
import java.lang.reflect.*;
import java.util.*;

/**
 * Represents an annotation type at run time.  Used to type-check annotations
 * and apply member defaults.
 */
public class AnnotationType {
    /**
     * Member name -> type mapping. Note that primitive types
     * are represented by the class objects for the corresponding wrapper
     * types.  This matches the return value that must be used for a
     * dynamic proxy, allowing for a simple isInstance test.
     */
    private final Map<String, Class<?>> memberTypes;

    /**
     * Member name -> default value mapping.
     */
    private final Map<String, Object> memberDefaults;

    /**
     * Member name -> Method object mapping. This (and its associated
     * accessor) are used only to generate AnnotationTypeMismatchExceptions.
     */
    private final Map<String, Method> members;

    /**
     * The retention policy for this annotation type.
     */
    private final RetentionPolicy retention;

    /**
     * Whether this annotation type is inherited.
     */
    private final boolean inherited;

    /**
     * Returns an AnnotationType instance for the specified annotation type.
     *
     * @throws IllegalArgumentException if the specified class object
     *         does not represent a valid annotation type
     */
    public static AnnotationType getInstance(Class<? extends Annotation> annotationClass)
    {
        AnnotationType result = annotationClass.getAnnotationType(); // volatile read
        if (result == null) {
            result = new AnnotationType(annotationClass);
            // try to CAS the AnnotationType: null -> result
            if (!annotationClass.casAnnotationType(null, result)) {
                // somebody was quicker -> read it's result
                result = annotationClass.getAnnotationType();
                assert result != null;
            }
        }

        return result;
    }

    /**
     * Sole constructor.
     *
     * @param annotationClass the class object for the annotation type
     * @throws IllegalArgumentException if the specified class object for
     *         does not represent a valid annotation type
     */
    private AnnotationType(final Class<? extends Annotation> annotationClass) {
        if (!annotationClass.isAnnotation())
            throw new IllegalArgumentException("Not an annotation type");

        // Initialize memberTypes and defaultValues
        Method[] methods = annotationClass.getDeclaredMethods();

        memberTypes = new HashMap<>(methods.length+1, 1.0f);
        memberDefaults = new HashMap<>(0);
        members = new HashMap<>(methods.length+1, 1.0f);

        for (Method method : methods) {
            if (Modifier.isPublic(method.getModifiers()) && Modifier.isAbstract(method.getModifiers()) && !method.isSynthetic()) {
                if (method.getParameterTypes().length != 0) {
                    throw new IllegalArgumentException(method + " has params");
                }
                String name = method.getName();
                Class<?> type = method.getReturnType();
                memberTypes.put(name, invocationHandlerReturnType(type));
                members.put(name, method);

                Object defaultValue = method.getDefaultValue();
                if (defaultValue != null) {
                    memberDefaults.put(name, defaultValue);
                }
            }
        }

        // Initialize retention, & inherited fields.  Special treatment
        // of the corresponding annotation types breaks infinite recursion.
        if (annotationClass != Retention.class && annotationClass != Inherited.class) {
            Map<Class<? extends Annotation>, Annotation> metaAnnotations =
                AnnotationParser.parseSelectAnnotations(
                    annotationClass.getRawAnnotations(),
                    annotationClass.getConstantPool(),
                    annotationClass,
                    Retention.class, Inherited.class
                );
            Retention ret = (Retention) metaAnnotations.get(Retention.class);
            retention = (ret == null ? RetentionPolicy.CLASS : ret.value());
            inherited = metaAnnotations.containsKey(Inherited.class);
        }
        else {
            retention = RetentionPolicy.RUNTIME;
            inherited = false;
        }
    }

    /**
     * Returns the type that must be returned by the invocation handler
     * of a dynamic proxy in order to have the dynamic proxy return
     * the specified type (which is assumed to be a legal member type
     * for an annotation).
     */
    public static Class<?> invocationHandlerReturnType(Class<?> type) {
        // Translate primitives to wrappers
        if (type == byte.class)
            return Byte.class;
        if (type == char.class)
            return Character.class;
        if (type == double.class)
            return Double.class;
        if (type == float.class)
            return Float.class;
        if (type == int.class)
            return Integer.class;
        if (type == long.class)
            return Long.class;
        if (type == short.class)
            return Short.class;
        if (type == boolean.class)
            return Boolean.class;

        // Otherwise, just return declared type
        return type;
    }

    /**
     * Returns member types for this annotation type
     * (member name {@literal ->} type mapping).
     */
    public Map<String, Class<?>> memberTypes() {
        return memberTypes;
    }

    /**
     * Returns members of this annotation type
     * (member name {@literal ->} associated Method object mapping).
     */
    public Map<String, Method> members() {
        return members;
    }

    /**
     * Returns the default values for this annotation type
     * (Member name {@literal ->} default value mapping).
     */
    public Map<String, Object> memberDefaults() {
        return memberDefaults;
    }

    /**
     * Returns the retention policy for this annotation type.
     */
    public RetentionPolicy retention() {
        return retention;
    }

    /**
     * Returns true if this annotation type is inherited.
     */
    public boolean isInherited() {
        return inherited;
    }

    /**
     * For debugging.
     */
    public String toString() {
        return "Annotation Type:\n" +
               "   Member types: " + memberTypes + "\n" +
               "   Member defaults: " + memberDefaults + "\n" +
               "   Retention policy: " + retention + "\n" +
               "   Inherited: " + inherited;
    }
}

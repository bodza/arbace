package jdk.internal.reflect;

import java.lang.reflect.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import jdk.internal.loader.ClassLoaders;
import jdk.internal.misc.VM;

/**
 * Common utility routines used by both java.lang and
 * java.lang.reflect
 */
public class Reflection {
    /**
     * Used to filter out fields and methods from certain classes from public
     * view, where they are sensitive or they may contain VM-internal objects.
     * These Maps are updated very rarely. Rather than synchronize on
     * each access, we use copy-on-write
     */
    private static volatile Map<Class<?>,String[]> fieldFilterMap;
    private static volatile Map<Class<?>,String[]> methodFilterMap;

    static {
        Map<Class<?>,String[]> map = new HashMap<Class<?>,String[]>();
        map.put(Reflection.class,
            new String[] {"fieldFilterMap", "methodFilterMap"});
        map.put(System.class, new String[] {"security"});
        map.put(Class.class, new String[] {"classLoader"});
        fieldFilterMap = map;

        methodFilterMap = new HashMap<>();
    }

    /**
     * Returns the class of the caller of the method calling this method,
     * ignoring frames associated with java.lang.reflect.Method.invoke()
     * and its implementation.
     */
    // @CallerSensitive
    // @HotSpotIntrinsicCandidate
    public static native Class<?> getCallerClass();

    /**
     * Retrieves the access flags written to the class file. For
     * inner classes these flags may differ from those returned by
     * Class.getModifiers(), which searches the InnerClasses
     * attribute to find the source-level access flags. This is used
     * instead of Class.getModifiers() for run-time access checks due
     * to compatibility reasons; see 4471811. Only the values of the
     * low 13 bits (i.e., a mask of 0x1FFF) are guaranteed to be
     * valid.
     */
    // @HotSpotIntrinsicCandidate
    public static native int getClassAccessFlags(Class<?> c);

    /**
     * Returns true if two classes in the same package.
     */
    private static boolean isSameClassPackage(Class<?> c1, Class<?> c2) {
        if (c1.getClassLoader() != c2.getClassLoader())
            return false;
        return Objects.equals(c1.getPackageName(), c2.getPackageName());
    }

    static boolean isSubclassOf(Class<?> queryClass, Class<?> ofClass) {
        while (queryClass != null) {
            if (queryClass == ofClass) {
                return true;
            }
            queryClass = queryClass.getSuperclass();
        }
        return false;
    }

    // fieldNames must contain only interned Strings
    public static synchronized void registerFieldsToFilter(Class<?> containingClass, String ... fieldNames) {
        fieldFilterMap = registerFilter(fieldFilterMap, containingClass, fieldNames);
    }

    // methodNames must contain only interned Strings
    public static synchronized void registerMethodsToFilter(Class<?> containingClass, String ... methodNames) {
        methodFilterMap = registerFilter(methodFilterMap, containingClass, methodNames);
    }

    private static Map<Class<?>,String[]> registerFilter(Map<Class<?>,String[]> map, Class<?> containingClass, String ... names) {
        if (map.get(containingClass) != null) {
            throw new IllegalArgumentException(String.str("Filter already registered: ", containingClass));
        }
        map = new HashMap<Class<?>,String[]>(map);
        map.put(containingClass, names);
        return map;
    }

    public static Field[] filterFields(Class<?> containingClass, Field[] fields) {
        if (fieldFilterMap == null) {
            // Bootstrapping
            return fields;
        }
        return (Field[])filter(fields, fieldFilterMap.get(containingClass));
    }

    public static Method[] filterMethods(Class<?> containingClass, Method[] methods) {
        if (methodFilterMap == null) {
            // Bootstrapping
            return methods;
        }
        return (Method[])filter(methods, methodFilterMap.get(containingClass));
    }

    private static Member[] filter(Member[] members, String[] filteredNames) {
        if ((filteredNames == null) || (members.length == 0)) {
            return members;
        }
        int numNewMembers = 0;
        for (Member member : members) {
            boolean shouldSkip = false;
            for (String filteredName : filteredNames) {
                if (member.getName() == filteredName) {
                    shouldSkip = true;
                    break;
                }
            }
            if (!shouldSkip) {
                ++numNewMembers;
            }
        }
        Member[] newMembers = (Member[])Array.newInstance(members[0].getClass(), numNewMembers);
        int destIdx = 0;
        for (Member member : members) {
            boolean shouldSkip = false;
            for (String filteredName : filteredNames) {
                if (member.getName() == filteredName) {
                    shouldSkip = true;
                    break;
                }
            }
            if (!shouldSkip) {
                newMembers[destIdx++] = member;
            }
        }
        return newMembers;
    }

    /**
     * Returns an IllegalAccessException with an exception message based on
     * the access that is denied.
     */
    public static IllegalAccessException newIllegalAccessException(Class<?> currentClass, Class<?> memberClass, Class<?> targetClass, int modifiers) throws IllegalAccessException {
        return new IllegalAccessException(String.str(currentClass, " cannot access a member of ", memberClass, " with modifiers \"", Modifier.toString(modifiers), "\""));
    }

    /**
     * Returns true if {@code currentClass} and {@code memberClass}
     * are nestmates - that is, if they have the same nesthost as
     * determined by the VM.
     */
    public static native boolean areNestMates(Class<?> currentClass, Class<?> memberClass);
}

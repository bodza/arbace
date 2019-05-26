package java.lang.reflect;

import jdk.internal.reflect.MethodAccessor;
import jdk.internal.reflect.ConstructorAccessor;

/**
 * Package-private class implementing the sun.reflect.LangReflectAccess interface,
 * allowing the java.lang package to instantiate objects in this package.
 */
class ReflectAccess implements jdk.internal.reflect.LangReflectAccess {
    public Field newField(Class<?> declaringClass, String name, Class<?> type, int modifiers, int slot, String signature) {
        return new Field(declaringClass, name, type, modifiers, slot, signature);
    }

    public Method newMethod(Class<?> declaringClass, String name, Class<?>[] parameterTypes, Class<?> returnType, Class<?>[] checkedExceptions, int modifiers, int slot, String signature) {
        return new Method(declaringClass, name, parameterTypes, returnType, checkedExceptions, modifiers, slot, signature);
    }

    public <T> Constructor<T> newConstructor(Class<T> declaringClass, Class<?>[] parameterTypes, Class<?>[] checkedExceptions, int modifiers, int slot, String signature) {
        return new Constructor<>(declaringClass, parameterTypes, checkedExceptions, modifiers, slot, signature);
    }

    public MethodAccessor getMethodAccessor(Method m) {
        return m.getMethodAccessor();
    }

    public void setMethodAccessor(Method m, MethodAccessor accessor) {
        m.setMethodAccessor(accessor);
    }

    public ConstructorAccessor getConstructorAccessor(Constructor<?> c) {
        return c.getConstructorAccessor();
    }

    public void setConstructorAccessor(Constructor<?> c, ConstructorAccessor accessor) {
        c.setConstructorAccessor(accessor);
    }

    public int getConstructorSlot(Constructor<?> c) {
        return c.getSlot();
    }

    public String getConstructorSignature(Constructor<?> c) {
        return c.getSignature();
    }

    public Class<?>[] getExecutableSharedParameterTypes(Executable ex) {
        return ex.getSharedParameterTypes();
    }

    //
    // Copying routines, needed to quickly fabricate new Field,
    // Method, and Constructor objects from templates
    //
    public Method copyMethod(Method arg) {
        return arg.copy();
    }

    public Method leafCopyMethod(Method arg) {
        return arg.leafCopy();
    }

    public Field copyField(Field arg) {
        return arg.copy();
    }

    public <T> Constructor<T> copyConstructor(Constructor<T> arg) {
        return arg.copy();
    }

    @SuppressWarnings("unchecked")
    public <T extends AccessibleObject> T getRoot(T obj) {
        return (T) obj.getRoot();
    }
}

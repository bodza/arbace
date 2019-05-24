package sun.nio.ch;

import java.io.*;
import java.lang.reflect.*;

class Reflect {

    private Reflect() { }

    private static class ReflectionError extends Error {
        ReflectionError(Throwable x) {
            super(x);
        }
    }

    private static void setAccessible(final AccessibleObject ao) {
        ao.setAccessible(true);
    }

    static Constructor<?> lookupConstructor(String className, Class<?>[] paramTypes)
    {
        try {
            Class<?> cl = Class.forName(className);
            Constructor<?> c = cl.getDeclaredConstructor(paramTypes);
            setAccessible(c);
            return c;
        } catch (ClassNotFoundException | NoSuchMethodException x) {
            throw new ReflectionError(x);
        }
    }

    static Object invoke(Constructor<?> c, Object[] args) {
        try {
            return c.newInstance(args);
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException x) {
            throw new ReflectionError(x);
        }
    }

    static Method lookupMethod(String className, String methodName, Class<?>... paramTypes)
    {
        try {
            Class<?> cl = Class.forName(className);
            Method m = cl.getDeclaredMethod(methodName, paramTypes);
            setAccessible(m);
            return m;
        } catch (ClassNotFoundException | NoSuchMethodException x) {
            throw new ReflectionError(x);
        }
    }

    static Object invoke(Method m, Object ob, Object[] args) {
        try {
            return m.invoke(ob, args);
        } catch (IllegalAccessException | InvocationTargetException x) {
            throw new ReflectionError(x);
        }
    }

    static Object invokeIO(Method m, Object ob, Object[] args) throws IOException
    {
        try {
            return m.invoke(ob, args);
        } catch (IllegalAccessException x) {
            throw new ReflectionError(x);
        } catch (InvocationTargetException x) {
            if (IOException.class.isInstance(x.getCause()))
                throw (IOException)x.getCause();
            throw new ReflectionError(x);
        }
    }

    static Field lookupField(String className, String fieldName) {
        try {
            Class<?> cl = Class.forName(className);
            Field f = cl.getDeclaredField(fieldName);
            setAccessible(f);
            return f;
        } catch (ClassNotFoundException | NoSuchFieldException x) {
            throw new ReflectionError(x);
        }
    }

    static Object get(Object ob, Field f) {
        try {
            return f.get(ob);
        } catch (IllegalAccessException x) {
            throw new ReflectionError(x);
        }
    }

    static Object get(Field f) {
        return get(null, f);
    }

    static void set(Object ob, Field f, Object val) {
        try {
            f.set(ob, val);
        } catch (IllegalAccessException x) {
            throw new ReflectionError(x);
        }
    }

    static void setInt(Object ob, Field f, int val) {
        try {
            f.setInt(ob, val);
        } catch (IllegalAccessException x) {
            throw new ReflectionError(x);
        }
    }

    static void setBoolean(Object ob, Field f, boolean val) {
        try {
            f.setBoolean(ob, val);
        } catch (IllegalAccessException x) {
            throw new ReflectionError(x);
        }
    }
}

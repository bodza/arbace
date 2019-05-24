package sun.reflect.misc;

import java.lang.reflect.Constructor;

public final class ConstructorUtil {
    private ConstructorUtil() {
    }

    public static Constructor<?> getConstructor(Class<?> cls, Class<?>[] params) throws NoSuchMethodException {
        return cls.getConstructor(params);
    }

    public static Constructor<?>[] getConstructors(Class<?> cls) {
        return cls.getConstructors();
    }
}

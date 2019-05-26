package sun.reflect.misc;

import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import jdk.internal.reflect.Reflection;

public final class ReflectUtil {
    private ReflectUtil() {
    }

    /**
     * Checks if {@code Class cls} is a VM-anonymous class
     * as defined by {@link jdk.internal.misc.Unsafe#defineAnonymousClass}
     * (not to be confused with a Java Language anonymous inner class).
     */
    public static boolean isVMAnonymousClass(Class<?> cls) {
        return cls.getName().indexOf('/') > -1;
    }
}

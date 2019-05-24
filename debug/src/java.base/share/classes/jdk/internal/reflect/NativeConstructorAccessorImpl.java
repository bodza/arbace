package jdk.internal.reflect;

import java.lang.reflect.*;
import sun.reflect.misc.ReflectUtil;

/**
  * Used only for the first few invocations of a Constructor;
  * afterward, switches to bytecode-based implementation
  */
class NativeConstructorAccessorImpl extends ConstructorAccessorImpl {
    private final Constructor<?> c;
    private DelegatingConstructorAccessorImpl parent;
    private int numInvocations;

    NativeConstructorAccessorImpl(Constructor<?> c) {
        this.c = c;
    }

    public Object newInstance(Object[] args) throws InstantiationException, IllegalArgumentException, InvocationTargetException
    {
        // We can't inflate a constructor belonging to a vm-anonymous class
        // because that kind of class can't be referred to by name, hence can't
        // be found from the generated bytecode.
        if (++numInvocations > ReflectionFactory.inflationThreshold() && !ReflectUtil.isVMAnonymousClass(c.getDeclaringClass())) {
            ConstructorAccessorImpl acc = (ConstructorAccessorImpl)
                new MethodAccessorGenerator().
                    generateConstructor(c.getDeclaringClass(),
                                        c.getParameterTypes(),
                                        c.getExceptionTypes(),
                                        c.getModifiers());
            parent.setDelegate(acc);
        }

        return newInstance0(c, args);
    }

    void setParent(DelegatingConstructorAccessorImpl parent) {
        this.parent = parent;
    }

    private static native Object newInstance0(Constructor<?> c, Object[] args) throws InstantiationException, IllegalArgumentException, InvocationTargetException;
}

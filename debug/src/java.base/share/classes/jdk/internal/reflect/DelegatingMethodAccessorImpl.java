package jdk.internal.reflect;

import java.lang.reflect.InvocationTargetException;

/**
 * Delegates its invocation to another MethodAccessorImpl and can
 * change its delegate at run time.
 */
class DelegatingMethodAccessorImpl extends MethodAccessorImpl {
    private MethodAccessorImpl delegate;

    DelegatingMethodAccessorImpl(MethodAccessorImpl delegate) {
        setDelegate(delegate);
    }

    public Object invoke(Object obj, Object[] args) throws IllegalArgumentException, InvocationTargetException {
        return delegate.invoke(obj, args);
    }

    void setDelegate(MethodAccessorImpl delegate) {
        this.delegate = delegate;
    }
}

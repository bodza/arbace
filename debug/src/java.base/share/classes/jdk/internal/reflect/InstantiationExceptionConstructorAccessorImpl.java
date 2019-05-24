package jdk.internal.reflect;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/**
  * Throws an InstantiationException with given error message upon
  * newInstance() call
  */
class InstantiationExceptionConstructorAccessorImpl extends ConstructorAccessorImpl {
    private final String message;

    InstantiationExceptionConstructorAccessorImpl(String message) {
        this.message = message;
    }

    public Object newInstance(Object[] args) throws InstantiationException, IllegalArgumentException, InvocationTargetException
    {
        if (message == null) {
            throw new InstantiationException();
        }
        throw new InstantiationException(message);
    }
}

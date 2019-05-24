package jdk.internal.reflect;

import java.lang.reflect.InvocationTargetException;

/**
 * Package-private implementation of the MethodAccessor interface
 * which has access to all classes and all fields, regardless of
 * language restrictions. See MagicAccessor.
 *
 * This class is known to the VM; do not change its name without
 * also changing the VM's code.
 *
 * NOTE: ALL methods of subclasses are skipped during security
 * walks up the stack. The assumption is that the only such methods
 * that will persistently show up on the stack are the implementing
 * methods for java.lang.reflect.Method.invoke().
 */
abstract class MethodAccessorImpl extends MagicAccessorImpl implements MethodAccessor {
    /** Matches specification in {@link java.lang.reflect.Method} */
    public abstract Object invoke(Object obj, Object[] args) throws IllegalArgumentException, InvocationTargetException;
}

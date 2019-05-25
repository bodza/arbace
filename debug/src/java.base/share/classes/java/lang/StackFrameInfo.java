package java.lang;

import static java.lang.StackWalker.Option.*;
import java.lang.StackWalker.StackFrame;

class StackFrameInfo implements StackFrame {
    private final byte RETAIN_CLASS_REF = 0x01;

    private final byte flags;
    private final Object memberName;
    private final short bci;
    private volatile StackTraceElement ste;

    /*
     * Create StackFrameInfo for StackFrameTraverser and LiveStackFrameTraverser
     * to use
     */
    StackFrameInfo(StackWalker walker) {
        this.flags = walker.retainClassRef ? RETAIN_CLASS_REF : 0;
        this.bci = -1;
        this.memberName = new MemberName();
    }

    // package-private called by StackStreamFactory to skip
    // the capability check
    Class<?> declaringClass() {
        return memberName.getDeclaringClass();
    }

    // ----- implementation of StackFrame methods

    @Override
    public String getClassName() {
        return declaringClass().getName();
    }

    @Override
    public Class<?> getDeclaringClass() {
        ensureRetainClassRefEnabled();
        return declaringClass();
    }

    @Override
    public String getMethodName() {
        return memberName.getName();
    }

    @Override
    public String getDescriptor() {
        return memberName.getMethodDescriptor();
    }

    @Override
    public int getByteCodeIndex() {
        // bci not available for native methods
        if (isNativeMethod())
            return -1;

        return bci;
    }

    @Override
    public boolean isNativeMethod() {
        return memberName.isNative();
    }

    @Override
    public String toString() {
        return toStackTraceElement().toString();
    }

    @Override
    public StackTraceElement toStackTraceElement() {
        StackTraceElement s = ste;
        if (s == null) {
            synchronized (this) {
                s = ste;
                if (s == null) {
                    ste = s = StackTraceElement.of(this);
                }
            }
        }
        return s;
    }

    private void ensureRetainClassRefEnabled() {
        if ((flags & RETAIN_CLASS_REF) == 0) {
            throw new UnsupportedOperationException("No access to RETAIN_CLASS_REFERENCE");
        }
    }
}

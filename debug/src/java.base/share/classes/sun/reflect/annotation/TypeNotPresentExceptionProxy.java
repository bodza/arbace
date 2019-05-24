package sun.reflect.annotation;
import java.lang.annotation.*;

/**
 * ExceptionProxy for TypeNotPresentException.
 */
public class TypeNotPresentExceptionProxy extends ExceptionProxy {
    final String typeName;
    final Throwable cause;

    public TypeNotPresentExceptionProxy(String typeName, Throwable cause) {
        this.typeName = typeName;
        this.cause = cause;
    }

    protected RuntimeException generateException() {
        return new TypeNotPresentException(typeName, cause);
    }

    public String typeName() {
        return typeName;
    }

    public Throwable getCause() {
        return cause;
    }

    @Override
    public String toString() {
        return typeName + ".class /* Warning: type not present! */";
    }
}

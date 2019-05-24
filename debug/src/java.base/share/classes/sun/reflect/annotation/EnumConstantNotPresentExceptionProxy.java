package sun.reflect.annotation;

/**
 * ExceptionProxy for EnumConstantNotPresentException.
 */
public class EnumConstantNotPresentExceptionProxy extends ExceptionProxy {
    final Class<? extends Enum<?>> enumType;
    final String constName;

    public EnumConstantNotPresentExceptionProxy(Class<? extends Enum<?>> enumType, String constName) {
        this.enumType = enumType;
        this.constName = constName;
    }

    protected RuntimeException generateException() {
        return new EnumConstantNotPresentException(enumType, constName);
    }

    @Override
    public String toString() {
        return constName + " /* Warning: constant not present! */";
    }
}

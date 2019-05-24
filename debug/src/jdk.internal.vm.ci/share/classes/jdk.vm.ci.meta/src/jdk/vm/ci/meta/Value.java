package jdk.vm.ci.meta;

/**
 * Abstract base class for values.
 */
public abstract class Value {
    public static final Value[] NO_VALUES = new Value[0];

    public static final AllocatableValue ILLEGAL = new IllegalValue();

    private static final class IllegalValue extends AllocatableValue {
        private IllegalValue() {
            super(ValueKind.Illegal);
        }

        @Override
        public boolean equals(Object other) {
            // Due to de-serialization this object may exist multiple times. So we compare classes
            // instead of the individual objects. (This anonymous class has always the same meaning)
            return other instanceof IllegalValue;
        }
    }

    private final ValueKind<?> valueKind;

    /**
     * Initializes a new value of the specified kind.
     *
     * @param valueKind the kind
     */
    protected Value(ValueKind<?> valueKind) {
        this.valueKind = valueKind;
    }

    public final ValueKind<?> getValueKind() {
        return valueKind;
    }

    public final <K extends ValueKind<K>> K getValueKind(Class<K> cls) {
        return cls.cast(valueKind);
    }

    /**
     * Returns the platform specific kind used to store this value.
     */
    public final PlatformKind getPlatformKind() {
        return valueKind.getPlatformKind();
    }

    @Override
    public int hashCode() {
        return 41 + valueKind.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Value) {
            Value that = (Value) obj;
            return valueKind.equals(that.valueKind);
        }
        return false;
    }

    /**
     * Checks if this value is identical to {@code other}.
     *
     * Warning: Use with caution! Usually equivalence {@link #equals(Object)} is sufficient and
     * should be used.
     */
    public final boolean identityEquals(Value other) {
        return this == other;
    }
}

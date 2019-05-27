package jdk.vm.ci.meta;

/**
 * Represents the type of {@link Value values}. This class can be extended by compilers to track
 * additional information about values.
 */
public abstract class ValueKind<K extends ValueKind<K>> {
    private enum IllegalKind implements PlatformKind {
        ILLEGAL;

        private final EnumKey<IllegalKind> key = new EnumKey<>(this);

        // @Override
        public Key getKey() {
            return key;
        }

        // @Override
        public int getSizeInBytes() {
            return 0;
        }

        // @Override
        public int getVectorLength() {
            return 0;
        }
    }

    private static class IllegalValueKind extends ValueKind<IllegalValueKind> {
        IllegalValueKind() {
            super(IllegalKind.ILLEGAL);
        }

        // @Override
        public IllegalValueKind changeType(PlatformKind newPlatformKind) {
            return this;
        }
    }

    /**
     * The non-type.
     */
    public static final ValueKind<?> Illegal = new IllegalValueKind();

    private final PlatformKind platformKind;

    public ValueKind(PlatformKind platformKind) {
        this.platformKind = platformKind;
    }

    public final PlatformKind getPlatformKind() {
        return platformKind;
    }

    /**
     * Create a new {@link ValueKind} with a different {@link PlatformKind}. Subclasses must
     * override this to preserve the additional information added by the compiler.
     */
    public abstract K changeType(PlatformKind newPlatformKind);
}

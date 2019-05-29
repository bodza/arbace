package jdk.vm.ci.meta;

/**
 * Implementation of {@link JavaType} for unresolved HotSpot classes.
 */
public final class UnresolvedJavaType implements JavaType {
    private final String name;

    // @Override
    public String getName() {
        return name;
    }

    private UnresolvedJavaType(String name) {
        this.name = name;
    }

    /**
     * Creates an unresolved type for a valid {@link JavaType#getName() type name}.
     */
    public static UnresolvedJavaType create(String name) {
        return new UnresolvedJavaType(name);
    }

    // @Override
    public int hashCode() {
        return getName().hashCode();
    }

    // @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || !(obj instanceof UnresolvedJavaType)) {
            return false;
        }
        UnresolvedJavaType that = (UnresolvedJavaType) obj;
        return this.getName().equals(that.getName());
    }
}

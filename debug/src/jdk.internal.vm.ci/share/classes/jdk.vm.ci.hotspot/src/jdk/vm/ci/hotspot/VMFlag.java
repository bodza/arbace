package jdk.vm.ci.hotspot;

/**
 * Describes a VM flag exposed via {@link HotSpotVMConfigAccess}.
 */
public final class VMFlag {
    /**
     * The name of the flag.
     */
    public final String name;

    /**
     * The C++ type of the flag.
     */
    public final String type;

    /**
     * The flag's value.
     */
    public final Object value;

    VMFlag(String name, String type, Object value) {
        this.name = name;
        this.type = type;
        this.value = value;
    }
}

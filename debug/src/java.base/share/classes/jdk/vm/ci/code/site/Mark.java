package jdk.vm.ci.code.site;

import java.util.Objects;

/**
 * Associates arbitrary information with a position in machine code. For example, HotSpot specific
 * code in a compiler backend may use this to denote the position of a safepoint, exception handler
 * entry point, verified entry point etc.
 */
public final class Mark extends Site {
    /**
     * An object denoting extra semantic information about the machine code position of this mark.
     */
    public final Object id;

    /**
     * Creates a mark that associates {@code id} with the machine code position {@code pcOffset}.
     *
     * @param pcOffset
     * @param id
     */
    public Mark(int pcOffset, Object id) {
        super(pcOffset);
        this.id = id;
    }

    // @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof Mark) {
            Mark that = (Mark) obj;
            if (this.pcOffset == that.pcOffset && Objects.equals(this.id, that.id)) {
                return true;
            }
        }
        return false;
    }
}

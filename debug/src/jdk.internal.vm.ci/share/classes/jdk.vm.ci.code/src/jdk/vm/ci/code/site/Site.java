package jdk.vm.ci.code.site;

/**
 * Represents a code position with associated additional information.
 */
public abstract class Site {
    /**
     * The position (or offset) of this site with respect to the start of the target method.
     */
    public final int pcOffset;

    public Site(int pos) {
        this.pcOffset = pos;
    }

    @Override
    public final int hashCode() {
        throw new UnsupportedOperationException("hashCode");
    }

    @Override
    public abstract boolean equals(Object obj);
}

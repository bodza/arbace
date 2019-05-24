package jdk.vm.ci.code.site;

import java.util.Map;
import java.util.Objects;

import jdk.vm.ci.code.Register;

/**
 * Represents an infopoint with associated debug info. Note that safepoints are also infopoints.
 */
public class Infopoint extends Site implements Comparable<Infopoint> {
    public final InfopointReason reason;

    public Infopoint(int pcOffset, InfopointReason reason) {
        super(pcOffset);
        this.reason = reason;
    }

    @Override
    public int compareTo(Infopoint o) {
        if (pcOffset < o.pcOffset) {
            return -1;
        } else if (pcOffset > o.pcOffset) {
            return 1;
        }
        return this.reason.compareTo(o.reason);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj != null && obj.getClass() == getClass()) {
            Infopoint that = (Infopoint) obj;
            if (this.pcOffset == that.pcOffset && Objects.equals(this.reason, that.reason)) {
                return true;
            }
        }
        return false;
    }
}

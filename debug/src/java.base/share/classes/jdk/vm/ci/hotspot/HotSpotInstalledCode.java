package jdk.vm.ci.hotspot;

import jdk.internal.misc.Unsafe;
import jdk.vm.ci.code.InstalledCode;

/**
 * Implementation of {@link InstalledCode} for HotSpot.
 */
public abstract class HotSpotInstalledCode extends InstalledCode {
    /**
     * Total size of the code blob.
     */
    private int size;

    /**
     * Start address of the code.
     */
    private long codeStart;

    /**
     * Size of the code.
     */
    private int codeSize;

    public HotSpotInstalledCode(String name) {
        super(name);
    }

    /**
     * @return the total size of this code blob
     */
    public int getSize() {
        return size;
    }

    // @Override
    public long getStart() {
        return codeStart;
    }
}

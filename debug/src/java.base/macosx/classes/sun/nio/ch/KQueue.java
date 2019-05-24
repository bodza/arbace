package sun.nio.ch;

import java.io.IOException;
import jdk.internal.misc.Unsafe;

/**
 * Provides access to the BSD kqueue facility.
 */
class KQueue {
    private KQueue() { }

    private static final Unsafe unsafe = Unsafe.getUnsafe();

    /**
     * struct kevent {
     *        uintptr_t       ident; // identifier for this event, usually the fd
     *        int16_t         filter; // filter for event
     *        uint16_t        flags; // general flags
     *        uint32_t        fflags; // filter-specific flags
     *        intptr_t        data; // filter-specific data
     *        void            *udata; // opaque user data identifier
     * };
     */
    private static final int SIZEOF_KQUEUEEVENT = keventSize();
    private static final int OFFSET_IDENT       = identOffset();
    private static final int OFFSET_FILTER      = filterOffset();
    private static final int OFFSET_FLAGS       = flagsOffset();

    // filters
    static final int EVFILT_READ  = -1;
    static final int EVFILT_WRITE = -2;

    // flags
    static final int EV_ADD     = 0x0001;
    static final int EV_DELETE  = 0x0002;
    static final int EV_ONESHOT = 0x0010;
    static final int EV_CLEAR   = 0x0020;

    /**
     * Allocates a poll array to handle up to {@code count} events.
     */
    static long allocatePollArray(int count) {
        return unsafe.allocateMemory(count * SIZEOF_KQUEUEEVENT);
    }

    /**
     * Free a poll array
     */
    static void freePollArray(long address) {
        unsafe.freeMemory(address);
    }

    /**
     * Returns kevent[i].
     */
    static long getEvent(long address, int i) {
        return address + (SIZEOF_KQUEUEEVENT*i);
    }

    /**
     * Returns the file descriptor from a kevent (assuming it is in the ident field)
     */
    static int getDescriptor(long address) {
        return unsafe.getInt(address + OFFSET_IDENT);
    }

    static short getFilter(long address) {
        return unsafe.getShort(address + OFFSET_FILTER);
    }

    static short getFlags(long address) {
        return unsafe.getShort(address + OFFSET_FLAGS);
    }

    // -- Native methods --

    private static native int keventSize();

    private static native int identOffset();

    private static native int filterOffset();

    private static native int flagsOffset();

    static native int create() throws IOException;

    static native int register(int kqfd, int fd, int filter, int flags);

    static native int poll(int kqfd, long pollAddress, int nevents, long timeout) throws IOException;

    static {
        IOUtil.load();
    }
}

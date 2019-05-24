package sun.nio.ch;

import java.io.IOException;
import jdk.internal.misc.Unsafe;

/**
 * Provides access to the Linux epoll facility.
 */
class EPoll {
    private EPoll() { }

    private static final Unsafe unsafe = Unsafe.getUnsafe();

    /**
     * typedef union epoll_data {
     *     void *ptr;
     *     int fd;
     *     __uint32_t u32;
     *     __uint64_t u64;
     *  } epoll_data_t;
     *
     * struct epoll_event {
     *     __uint32_t events;
     *     epoll_data_t data;
     * }
     */
    private static final int SIZEOF_EPOLLEVENT = eventSize();
    private static final int OFFSETOF_EVENTS   = eventsOffset();
    private static final int OFFSETOF_FD       = dataOffset();

    // opcodes
    static final int EPOLL_CTL_ADD  = 1;
    static final int EPOLL_CTL_DEL  = 2;
    static final int EPOLL_CTL_MOD  = 3;

    // events
    static final int EPOLLIN   = 0x1;
    static final int EPOLLOUT  = 0x4;

    // flags
    static final int EPOLLONESHOT   = (1 << 30);

    /**
     * Allocates a poll array to handle up to {@code count} events.
     */
    static long allocatePollArray(int count) {
        return unsafe.allocateMemory(count * SIZEOF_EPOLLEVENT);
    }

    /**
     * Free a poll array
     */
    static void freePollArray(long address) {
        unsafe.freeMemory(address);
    }

    /**
     * Returns event[i];
     */
    static long getEvent(long address, int i) {
        return address + (SIZEOF_EPOLLEVENT*i);
    }

    /**
     * Returns event->data.fd
     */
    static int getDescriptor(long eventAddress) {
        return unsafe.getInt(eventAddress + OFFSETOF_FD);
    }

    /**
     * Returns event->events
     */
    static int getEvents(long eventAddress) {
        return unsafe.getInt(eventAddress + OFFSETOF_EVENTS);
    }

    // -- Native methods --

    private static native int eventSize();

    private static native int eventsOffset();

    private static native int dataOffset();

    static native int create() throws IOException;

    static native int ctl(int epfd, int opcode, int fd, int events);

    static native int wait(int epfd, long pollAddress, int numfds, int timeout) throws IOException;

    static {
        IOUtil.load();
    }
}

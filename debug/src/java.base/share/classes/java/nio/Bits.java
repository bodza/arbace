package java.nio;

import jdk.internal.misc.Unsafe;
import jdk.internal.misc.VM;

import java.util.concurrent.atomic.AtomicLong;

/**
 * Access to bits, native and otherwise.
 */
class Bits {
    private Bits() { }

    // -- Swapping --

    static short swap(short x) {
        return Short.reverseBytes(x);
    }

    static char swap(char x) {
        return Character.reverseBytes(x);
    }

    static int swap(int x) {
        return Integer.reverseBytes(x);
    }

    static long swap(long x) {
        return Long.reverseBytes(x);
    }

    // -- Unsafe access --

    private static final Unsafe UNSAFE = Unsafe.getUnsafe();

    // -- Processor and memory-system properties --

    private static int PAGE_SIZE = -1;

    static int pageSize() {
        if (PAGE_SIZE == -1)
            PAGE_SIZE = UNSAFE.pageSize();
        return PAGE_SIZE;
    }

    static int pageCount(long size) {
        return (int)(size + (long)pageSize() - 1L) / pageSize();
    }

    private static boolean UNALIGNED = UNSAFE.unalignedAccess();

    static boolean unaligned() {
        return UNALIGNED;
    }
}

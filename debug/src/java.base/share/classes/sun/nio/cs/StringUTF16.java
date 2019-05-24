package sun.nio.cs;

import static jdk.internal.misc.Unsafe.ARRAY_BYTE_BASE_OFFSET;
import static jdk.internal.misc.Unsafe.ARRAY_BYTE_INDEX_SCALE;

class StringUTF16 {
    public static char getChar(byte[] val, int index) {
        return unsafe.getChar(val, ARRAY_BYTE_BASE_OFFSET + ARRAY_BYTE_INDEX_SCALE * index * 2L);
    }

    private static final jdk.internal.misc.Unsafe unsafe = jdk.internal.misc.Unsafe.getUnsafe();
}

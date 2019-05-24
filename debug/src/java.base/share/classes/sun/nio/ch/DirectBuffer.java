package sun.nio.ch;

import jdk.internal.ref.Cleaner;

public interface DirectBuffer {
    public long address();

    public Object attachment();

    public Cleaner cleaner();
}

package sun.nio.ch;

import java.net.SocketOption;

/**
 * Defines socket options that are supported by the implementation
 * but not defined in StandardSocketOptions.
 */
class ExtendedSocketOption {
    private ExtendedSocketOption() { }

    static final SocketOption<Boolean> SO_OOBINLINE =
        new SocketOption<Boolean>() {
            public String name() { return "SO_OOBINLINE"; }
            public Class<Boolean> type() { return Boolean.class; }
            public String toString() { return name(); }
        };
}

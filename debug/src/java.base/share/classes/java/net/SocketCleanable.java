package java.net;

import jdk.internal.ref.CleanerFactory;
import jdk.internal.ref.PhantomCleanable;

import java.io.FileDescriptor;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.ref.Cleaner;

/**
 * Cleanable for a socket/datagramsocket FileDescriptor when it becomes phantom reachable.
 * Create a cleanup if the raw fd != -1. Windows closes sockets using the fd.
 * Subclassed from {@code PhantomCleanable} so that {@code clear} can be
 * called to disable the cleanup when the socket fd is closed by any means
 * other than calling {@link FileDescriptor#close}.
 * Otherwise, it might incorrectly close the handle or fd after it has been reused.
 */
final class SocketCleanable extends PhantomCleanable<FileDescriptor> {
    // Native function to call NET_SocketClose(fd)
    // Used only for last chance cleanup.
    private static native void cleanupClose0(int fd) throws IOException;

    // The raw fd to close
    private final int fd;

    /**
     * Register a socket specific Cleanable with the FileDescriptor
     * if the FileDescriptor is non-null and the raw fd is != -1.
     *
     * @param fdo the FileDescriptor; may be null
     */
    static void register(FileDescriptor fdo) {
        if (fdo != null && fdo.valid()) {
            int fd = fdo.fd;
            fdo.registerCleanup(new SocketCleanable(fdo, CleanerFactory.cleaner(), fd));
        }
    }

    /**
     * Unregister a Cleanable from the FileDescriptor.
     * @param fdo the FileDescriptor; may be null
     */
    static void unregister(FileDescriptor fdo) {
        if (fdo != null) {
            fdo.unregisterCleanup();
        }
    }

    /**
     * Constructor for a phantom cleanable reference.
     *
     * @param obj     the object to monitor
     * @param cleaner the cleaner
     * @param fd      file descriptor to close
     */
    private SocketCleanable(FileDescriptor obj, Cleaner cleaner, int fd) {
        super(obj, cleaner);
        this.fd = fd;
    }

    /**
     * Close the native handle or fd.
     */
    @Override
    protected void performCleanup() {
        try {
            cleanupClose0(fd);
        } catch (IOException ioe) {
            throw new UncheckedIOException("close", ioe);
        }
    }
}

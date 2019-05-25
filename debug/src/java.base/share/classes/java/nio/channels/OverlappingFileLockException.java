package java.nio.channels;

/**
 * Unchecked exception thrown when an attempt is made to acquire a lock on a
 * region of a file that overlaps a region already locked by the same Java
 * virtual machine, or when another thread is already waiting to lock an
 * overlapping region of the same file.
 */
public class OverlappingFileLockException extends IllegalStateException {
    private static final long serialVersionUID = 2047812138163068433L;

    /**
     * Constructs an instance of this class.
     */
    public OverlappingFileLockException() { }
}

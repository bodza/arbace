package jdk.internal.perf;

import java.nio.ByteBuffer;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import jdk.internal.ref.CleanerFactory;

/**
 * The Perf class provides the ability to attach to an instrumentation
 * buffer maintained by a Java virtual machine. The instrumentation
 * buffer may be for the Java virtual machine running the methods of
 * this class or it may be for another Java virtual machine on the
 * same system.
 *
 * In addition, this class provides methods to create instrumentation
 * objects in the instrumentation buffer for the Java virtual machine
 * that is running these methods. It also contains methods for acquiring
 * the value of a platform specific high resolution clock for time
 * stamp and interval measurement purposes.
 */
public final class Perf {
    private static Perf instance;

    private static final int PERF_MODE_RO = 0;
    private static final int PERF_MODE_RW = 1;

    private Perf() { } // prevent instantiation

    /**
     * Return a reference to the singleton Perf instance.
     *
     * The getPerf() method returns the singleton instance of the Perf
     * class. The returned object provides the caller with the capability
     * for accessing the instrumentation buffer for this or another local
     * Java virtual machine.
     *
     * Access to the returned <code>Perf</code> object should be protected
     * by its caller and not passed on to untrusted code. This object can
     * be used to attach to the instrumentation buffer provided by this Java
     * virtual machine or for those of other Java virtual machines running
     * on the same system. The instrumentation buffer may contain senstitive
     * information. API's built on top of this interface may want to provide
     * finer grained access control to the contents of individual
     * instrumentation objects contained within the buffer.
     *
     * Please note that the <em>"sun.misc.Perf.getPerf"</em> permission
     * is not a JDK specified permission.
     *
     * @return A reference to the singleton Perf instance.
     */
    public static Perf getPerf() {
        return instance;
    }

    /**
     * Attach to the instrumentation buffer for the specified Java virtual
     * machine.
     *
     * This method will attach to the instrumentation buffer for the
     * specified virtual machine. It returns a <code>ByteBuffer</code> object
     * that is initialized to access the instrumentation buffer for the
     * indicated Java virtual machine. The <code>lvmid</code> parameter is
     * a integer value that uniquely identifies the target local Java virtual
     * machine. It is typically, but not necessarily, the process id of
     * the target Java virtual machine.
     *
     * If the <code>lvmid</code> identifies a Java virtual machine different
     * from the one running this method, then the coherency characteristics
     * of the buffer are implementation dependent. Implementations that do
     * not support named, coherent, shared memory may return a
     * <code>ByteBuffer</code> object that contains only a snap shot of the
     * data in the instrumentation buffer. Implementations that support named,
     * coherent, shared memory, may return a <code>ByteBuffer</code> object
     * that will be changing dynamically over time as the target Java virtual
     * machine updates its mapping of this buffer.
     *
     * If the <code>lvmid</code> is 0 or equal to the actual <code>lvmid</code>
     * for the Java virtual machine running this method, then the returned
     * <code>ByteBuffer</code> object will always be coherent and dynamically
     * changing.
     *
     * The attach mode specifies the access permissions requested for the
     * instrumentation buffer of the target virtual machine. The permitted
     * access permissions are:
     * <ul>
     * <li>"r"  - Read only access. This Java virtual machine has only
     * read access to the instrumentation buffer for the target Java
     * virtual machine.
     * <li>"rw"  - Read/Write access. This Java virtual machine has read and
     * write access to the instrumentation buffer for the target Java virtual
     * machine. This mode is currently not supported and is reserved for
     * future enhancements.
     * </ul>
     *
     * @param lvmid            an integer that uniquely identifies the
     *                           target local Java virtual machine.
     * @param mode             a string indicating the attach mode.
     * @return ByteBuffer       a direct allocated byte buffer
     * @throws IllegalArgumentException  The lvmid or mode was invalid.
     * @throws IOException      An I/O error occurred while trying to acquire
     *                           the instrumentation buffer.
     * @throws OutOfMemoryError The instrumentation buffer could not be mapped
     *                           into the virtual machine's address space.
     */
    public ByteBuffer attach(int lvmid, String mode) throws IllegalArgumentException, IOException {
        if (mode.compareTo("r") == 0) {
            return attachImpl(null, lvmid, PERF_MODE_RO);
        }
        else if (mode.compareTo("rw") == 0) {
            return attachImpl(null, lvmid, PERF_MODE_RW);
        }
        else {
            throw new IllegalArgumentException("unknown mode");
        }
    }

    /**
     * Attach to the instrumentation buffer for the specified Java virtual
     * machine owned by the given user.
     *
     * This method behaves just as the <code>attach(int lvmid, String mode)
     * </code> method, except that it only searches for Java virtual machines
     * owned by the specified user.
     *
     * @param user             A <code>String</code> object containing the
     *                           name of the user that owns the target Java
     *                           virtual machine.
     * @param lvmid            an integer that uniquely identifies the
     *                           target local Java virtual machine.
     * @param mode             a string indicating the attach mode.
     * @return ByteBuffer       a direct allocated byte buffer
     * @throws IllegalArgumentException  The lvmid or mode was invalid.
     * @throws IOException      An I/O error occurred while trying to acquire
     *                           the instrumentation buffer.
     * @throws OutOfMemoryError The instrumentation buffer could not be mapped
     *                           into the virtual machine's address space.
     */
    public ByteBuffer attach(String user, int lvmid, String mode) throws IllegalArgumentException, IOException {
        if (mode.compareTo("r") == 0) {
            return attachImpl(user, lvmid, PERF_MODE_RO);
        }
        else if (mode.compareTo("rw") == 0) {
            return attachImpl(user, lvmid, PERF_MODE_RW);
        }
        else {
            throw new IllegalArgumentException("unknown mode");
        }
    }

    /**
     * Call the implementation specific attach method.
     *
     * This method calls into the Java virtual machine to perform the platform
     * specific attach method. Buffers returned from this method are
     * internally managed as <code>PhantomRefereces</code> to provide for
     * guaranteed, secure release of the native resources.
     *
     * @param user             A <code>String</code> object containing the
     *                           name of the user that owns the target Java
     *                           virtual machine.
     * @param lvmid            an integer that uniquely identifies the
     *                           target local Java virtual machine.
     * @param mode             a string indicating the attach mode.
     * @return ByteBuffer       a direct allocated byte buffer
     * @throws IllegalArgumentException  The lvmid or mode was invalid.
     * @throws IOException      An I/O error occurred while trying to acquire
     *                           the instrumentation buffer.
     * @throws OutOfMemoryError The instrumentation buffer could not be mapped
     *                           into the virtual machine's address space.
     */
    private ByteBuffer attachImpl(String user, int lvmid, int mode) throws IllegalArgumentException, IOException {
        final ByteBuffer b = attach(user, lvmid, mode);

        if (lvmid == 0) {
            // The native instrumentation buffer for this Java virtual
            // machine is never unmapped.
            return b;
        }
        else {
            // This is an instrumentation buffer for another Java virtual
            // machine with native resources that need to be managed. We
            // create a duplicate of the native ByteBuffer and manage it
            // with a Cleaner. When the duplicate becomes phantom reachable,
            // the native resources will be released.

            final ByteBuffer dup = b.duplicate();

            CleanerFactory.cleaner()
                          .register(dup, new CleanerAction(instance, b));
            return dup;
        }
    }

    private static class CleanerAction implements Runnable {
        private final ByteBuffer bb;
        private final Perf perf;
        CleanerAction(Perf perf, ByteBuffer bb) {
            this.perf = perf;
            this.bb = bb;
        }

        public void run() {
            try {
                perf.detach(bb);
            } catch (Throwable th) {
                // avoid crashing the reference handler thread,
                // but provide for some diagnosability
                // assert false : th.toString();
            }
        }
    }

    /**
     * Native method to perform the implementation specific attach mechanism.
     *
     * The implementation of this method may return distinct or identical
     * <code>ByteBuffer</code> objects for two distinct calls requesting
     * attachment to the same Java virtual machine.
     *
     * For the Sun HotSpot JVM, two distinct calls to attach to the same
     * target Java virtual machine will result in two distinct ByteBuffer
     * objects returned by this method. This may change in a future release.
     *
     * @param user             A <code>String</code> object containing the
     *                           name of the user that owns the target Java
     *                           virtual machine.
     * @param lvmid            an integer that uniquely identifies the
     *                           target local Java virtual machine.
     * @param mode             a string indicating the attach mode.
     * @return ByteBuffer       a direct allocated byte buffer
     * @throws IllegalArgumentException  The lvmid or mode was invalid.
     * @throws IOException      An I/O error occurred while trying to acquire
     *                           the instrumentation buffer.
     * @throws OutOfMemoryError The instrumentation buffer could not be mapped
     *                           into the virtual machine's address space.
     */
    private native ByteBuffer attach(String user, int lvmid, int mode) throws IllegalArgumentException, IOException;

    /**
     * Native method to perform the implementation specific detach mechanism.
     *
     * If this method is passed a <code>ByteBuffer</code> object that is
     * not created by the <code>attach</code> method, then the results of
     * this method are undefined, with unpredictable and potentially damaging
     * effects to the Java virtual machine. To prevent accidental or malicious
     * use of this method, all native ByteBuffer created by the <code>
     * attach</code> method are managed internally as PhantomReferences
     * and resources are freed by the system.
     *
     * If this method is passed a <code>ByteBuffer</code> object created
     * by the <code>attach</code> method with a lvmid for the Java virtual
     * machine running this method (lvmid=0, for example), then the detach
     * request is silently ignored.
     *
     * @param bb  A direct allocated byte buffer created by the
     *                    <code>attach</code> method.
     */
    private native void detach(ByteBuffer bb);

    /**
     * Create a <code>long</code> scalar entry in the instrumentation buffer
     * with the given variability characteristic, units, and initial value.
     *
     * Access to the instrument is provided through the returned <code>
     * ByteBuffer</code> object. Typically, this object should be wrapped
     * with <code>LongBuffer</code> view object.
     *
     * @param variability the variability characteristic for this entry.
     * @param units       the units for this entry.
     * @param name        the name of this entry.
     * @param value       the initial value for this entry.
     * @return ByteBuffer  a direct allocated ByteBuffer object that
     *                      allows write access to a native memory location
     *                      containing a <code>long</code> value.
     *
     * see sun.misc.perf.Variability
     * see sun.misc.perf.Units
     */
    public native ByteBuffer createLong(String name, int variability, int units, long value);

    /**
     * Create a <code>String</code> entry in the instrumentation buffer with
     * the given variability characteristic, units, and initial value.
     *
     * The maximum length of the <code>String</code> stored in this string
     * instrument is given in by <code>maxLength</code> parameter. Updates
     * to this instrument with <code>String</code> values with lengths greater
     * than <code>maxLength</code> will be truncated to <code>maxLength</code>.
     * The truncated value will be terminated by a null character.
     *
     * The underlying implementation may further limit the length of the
     * value, but will continue to preserve the null terminator.
     *
     * Access to the instrument is provided through the returned <code>
     * ByteBuffer</code> object.
     *
     * @param variability the variability characteristic for this entry.
     * @param units       the units for this entry.
     * @param name        the name of this entry.
     * @param value       the initial value for this entry.
     * @param maxLength   the maximum string length for this string
     *                      instrument.
     * @return ByteBuffer  a direct allocated ByteBuffer that allows
     *                      write access to a native memory location
     *                      containing a <code>long</code> value.
     *
     * see sun.misc.perf.Variability
     * see sun.misc.perf.Units
     */
    public ByteBuffer createString(String name, int variability, int units, String value, int maxLength) {
        byte[] v = getBytes(value);
        byte[] v1 = new byte[v.length+1];
        System.arraycopy(v, 0, v1, 0, v.length);
        v1[v.length] = '\0';
        return createByteArray(name, variability, units, v1, Math.max(v1.length, maxLength));
    }

    /**
     * Create a <code>String</code> entry in the instrumentation buffer with
     * the given variability characteristic, units, and initial value.
     *
     * The maximum length of the <code>String</code> stored in this string
     * instrument is implied by the length of the <code>value</code> parameter.
     * Subsequent updates to the value of this instrument will be truncated
     * to this implied maximum length. The truncated value will be terminated
     * by a null character.
     *
     * The underlying implementation may further limit the length of the
     * initial or subsequent value, but will continue to preserve the null
     * terminator.
     *
     * Access to the instrument is provided through the returned <code>
     * ByteBuffer</code> object.
     *
     * @param variability the variability characteristic for this entry.
     * @param units       the units for this entry.
     * @param name        the name of this entry.
     * @param value       the initial value for this entry.
     * @return ByteBuffer  a direct allocated ByteBuffer that allows
     *                      write access to a native memory location
     *                      containing a <code>long</code> value.
     *
     * see sun.misc.perf.Variability
     * see sun.misc.perf.Units
     */
    public ByteBuffer createString(String name, int variability, int units, String value) {
        byte[] v = getBytes(value);
        byte[] v1 = new byte[v.length+1];
        System.arraycopy(v, 0, v1, 0, v.length);
        v1[v.length] = '\0';
        return createByteArray(name, variability, units, v1, v1.length);
    }

    /**
     * Create a <code>byte</code> vector entry in the instrumentation buffer
     * with the given variability characteristic, units, and initial value.
     *
     * The <code>maxLength</code> parameter limits the size of the byte
     * array instrument such that the initial or subsequent updates beyond
     * this length are silently ignored. No special handling of truncated
     * updates is provided.
     *
     * The underlying implementation may further limit the length of the
     * length of the initial or subsequent value.
     *
     * Access to the instrument is provided through the returned <code>
     * ByteBuffer</code> object.
     *
     * @param variability the variability characteristic for this entry.
     * @param units       the units for this entry.
     * @param name        the name of this entry.
     * @param value       the initial value for this entry.
     * @param maxLength   the maximum length of this byte array.
     * @return ByteBuffer  a direct allocated byte buffer that allows
     *                      write access to a native memory location
     *                      containing a <code>long</code> value.
     *
     * see sun.misc.perf.Variability
     * see sun.misc.perf.Units
     */
    public native ByteBuffer createByteArray(String name, int variability, int units, byte[] value, int maxLength);

    /**
     * convert string to an array of UTF-8 bytes
     */
    private static byte[] getBytes(String s) {
        byte[] bytes = null;

        try {
            bytes = s.getBytes("UTF-8");
        }
        catch (UnsupportedEncodingException e) {
            // ignore, UTF-8 encoding is always known
        }

        return bytes;
    }

    /**
     * Return the value of the High Resolution Counter.
     *
     * The High Resolution Counter returns the number of ticks since
     * since the start of the Java virtual machine. The resolution of
     * the counter is machine dependent and can be determined from the
     * value return by the {@link #highResFrequency} method.
     *
     * @return the number of ticks of machine dependent resolution since
     *          the start of the Java virtual machine.
     */
    public native long highResCounter();

    /**
     * Returns the frequency of the High Resolution Counter, in ticks per
     * second.
     *
     * This value can be used to convert the value of the High Resolution
     * Counter, as returned from a call to the {@link #highResCounter} method,
     * into the number of seconds since the start of the Java virtual machine.
     *
     * @return the frequency of the High Resolution Counter.
     */
    public native long highResFrequency();

    private static native void registerNatives();

    static {
        registerNatives();
        instance = new Perf();
    }
}

package java.lang;

import  java.io.*;
import  java.util.*;

/**
 * The {@code Throwable} class is the superclass of all errors and
 * exceptions in the Java language. Only objects that are instances of this
 * class (or one of its subclasses) are thrown by the Java Virtual Machine or
 * can be thrown by the Java {@code throw} statement. Similarly, only
 * this class or one of its subclasses can be the argument type in a
 * {@code catch} clause.
 *
 * For the purposes of compile-time checking of exceptions, {@code
 * Throwable} and any subclass of {@code Throwable} that is not also a
 * subclass of either {@link RuntimeException} or {@link Error} are
 * regarded as checked exceptions.
 *
 * Instances of two subclasses, {@link java.lang.Error} and
 * {@link java.lang.Exception}, are conventionally used to indicate
 * that exceptional situations have occurred. Typically, these instances
 * are freshly created in the context of the exceptional situation so
 * as to include relevant information (such as stack trace data).
 *
 * A throwable contains a snapshot of the execution stack of its
 * thread at the time it was created. It can also contain a message
 * string that gives more information about the error. Over time, a
 * throwable can {@linkplain Throwable#addSuppressed suppress} other
 * throwables from being propagated.  Finally, the throwable can also
 * contain a <i>cause</i>: another throwable that caused this
 * throwable to be constructed.  The recording of this causal information
 * is referred to as the <i>chained exception</i> facility, as the
 * cause can, itself, have a cause, and so on, leading to a "chain" of
 * exceptions, each caused by another.
 *
 * One reason that a throwable may have a cause is that the class that
 * throws it is built atop a lower layered abstraction, and an operation on
 * the upper layer fails due to a failure in the lower layer.  It would be bad
 * design to let the throwable thrown by the lower layer propagate outward, as
 * it is generally unrelated to the abstraction provided by the upper layer.
 * Further, doing so would tie the API of the upper layer to the details of
 * its implementation, assuming the lower layer's exception was a checked
 * exception.  Throwing a "wrapped exception" (i.e., an exception containing a
 * cause) allows the upper layer to communicate the details of the failure to
 * its caller without incurring either of these shortcomings.  It preserves
 * the flexibility to change the implementation of the upper layer without
 * changing its API (in particular, the set of exceptions thrown by its
 * methods).
 *
 * A second reason that a throwable may have a cause is that the method
 * that throws it must conform to a general-purpose interface that does not
 * permit the method to throw the cause directly.  For example, suppose
 * a persistent collection conforms to the {@link java.util.Collection
 * Collection} interface, and that its persistence is implemented atop
 * {@code java.io}.  Suppose the internals of the {@code add} method
 * can throw an {@link java.io.IOException IOException}.  The implementation
 * can communicate the details of the {@code IOException} to its caller
 * while conforming to the {@code Collection} interface by wrapping the
 * {@code IOException} in an appropriate unchecked exception.  (The
 * specification for the persistent collection should indicate that it is
 * capable of throwing such exceptions.)
 *
 * A cause can be associated with a throwable in two ways: via a
 * constructor that takes the cause as an argument, or via the
 * {@link #initCause(Throwable)} method.  New throwable classes that
 * wish to allow causes to be associated with them should provide constructors
 * that take a cause and delegate (perhaps indirectly) to one of the
 * {@code Throwable} constructors that takes a cause.
 *
 * Because the {@code initCause} method is public, it allows a cause to be
 * associated with any throwable, even a "legacy throwable" whose
 * implementation predates the addition of the exception chaining mechanism to
 * {@code Throwable}.
 *
 * By convention, class {@code Throwable} and its subclasses have two
 * constructors, one that takes no arguments and one that takes a
 * {@code String} argument that can be used to produce a detail message.
 * Further, those subclasses that might likely have a cause associated with
 * them should have two more constructors, one that takes a
 * {@code Throwable} (the cause), and one that takes a
 * {@code String} (the detail message) and a {@code Throwable} (the cause).
 */
public class Throwable {
    /**
     * The JVM saves some indication of the stack backtrace in this slot.
     */
    private transient Object backtrace;

    /**
     * Specific details about the Throwable.
     */
    private String detailMessage;

    /*
     * To allow Throwable objects to be made immutable and safely
     * reused by the JVM, such as OutOfMemoryErrors, fields of
     * Throwable that are writable in response to user actions, cause,
     * stackTrace, and suppressedExceptions obey the following
     * protocol:
     *
     * 1) The fields are initialized to a non-null sentinel value
     * which indicates the value has logically not been set.
     *
     * 2) Writing a null to the field indicates further writes
     * are forbidden
     *
     * 3) The sentinel value may be replaced with another non-null
     * value.
     *
     * For example, implementations of the HotSpot JVM have
     * preallocated OutOfMemoryError objects to provide for better
     * diagnosability of that situation.  These objects are created
     * without calling the constructor for that class and the fields
     * in question are initialized to null.  To support this
     * capability, any new fields added to Throwable that require
     * being initialized to a non-null value require a coordinated JVM
     * change.
     */

    /**
     * The throwable that caused this throwable to get thrown, or null if this
     * throwable was not caused by another throwable, or if the causative
     * throwable is unknown.  If this field is equal to this throwable itself,
     * it indicates that the cause of this throwable has not yet been
     * initialized.
     */
    private Throwable cause = this;

    /**
     * The JVM code sets the depth of the backtrace for later retrieval
     */
    private transient int depth;

    // Setting this static field introduces an acceptable
    // initialization dependency on a few java.util classes.
    private static final List<Throwable> SUPPRESSED_SENTINEL = Collections.emptyList();

    /**
     * The list of suppressed exceptions, as returned by {@link
     * #getSuppressed()}.  The list is initialized to a zero-element
     * unmodifiable sentinel list.  When a serialized Throwable is
     * read in, if the {@code suppressedExceptions} field points to a
     * zero-element list, the field is reset to the sentinel value.
     */
    private List<Throwable> suppressedExceptions = SUPPRESSED_SENTINEL;

    /** Message for trying to suppress a null exception. */
    private static final String NULL_CAUSE_MESSAGE = "Cannot suppress a null exception.";

    /** Message for trying to suppress oneself. */
    private static final String SELF_SUPPRESSION_MESSAGE = "Self-suppression not permitted";

    /** Caption  for labeling causative exception stack traces */
    private static final String CAUSE_CAPTION = "Caused by: ";

    /** Caption for labeling suppressed exception stack traces */
    private static final String SUPPRESSED_CAPTION = "Suppressed: ";

    /**
     * Constructs a new throwable with {@code null} as its detail message.
     * The cause is not initialized, and may subsequently be initialized by a
     * call to {@link #initCause}.
     */
    public Throwable() {
    }

    /**
     * Constructs a new throwable with the specified detail message.  The
     * cause is not initialized, and may subsequently be initialized by
     * a call to {@link #initCause}.
     *
     * @param message   the detail message. The detail message is saved for
     *          later retrieval by the {@link #getMessage()} method.
     */
    public Throwable(String message) {
        detailMessage = message;
    }

    /**
     * Constructs a new throwable with the specified detail message and
     * cause.
     *
     * Note that the detail message associated with
     * {@code cause} is <i>not</i> automatically incorporated in
     * this throwable's detail message.
     *
     * @param message the detail message (which is saved for later retrieval
     *         by the {@link #getMessage()} method).
     * @param cause the cause (which is saved for later retrieval by the
     *         {@link #getCause()} method).  (A {@code null} value is
     *         permitted, and indicates that the cause is nonexistent or
     *         unknown.)
     */
    public Throwable(String message, Throwable cause) {
        detailMessage = message;
        this.cause = cause;
    }

    /**
     * Constructs a new throwable with the specified cause and a detail
     * message of {@code (cause==null ? null : cause.toString())} (which
     * typically contains the class and detail message of {@code cause}).
     * This constructor is useful for throwables that are little more than
     * wrappers for other throwables.
     *
     * @param cause the cause (which is saved for later retrieval by the
     *         {@link #getCause()} method).  (A {@code null} value is
     *         permitted, and indicates that the cause is nonexistent or
     *         unknown.)
     */
    public Throwable(Throwable cause) {
        detailMessage = (cause == null ? null : cause.toString());
        this.cause = cause;
    }

    /**
     * Constructs a new throwable with the specified detail message,
     * cause, {@linkplain #addSuppressed suppression} enabled or
     * disabled, and writable stack trace enabled or disabled.  If
     * suppression is disabled, {@link #getSuppressed} for this object
     * will return a zero-length array and calls to {@link
     * #addSuppressed} that would otherwise append an exception to the
     * suppressed list will have no effect.
     *
     * Note that the other constructors of {@code Throwable} treat
     * suppression as being enabled and the stack trace as being
     * writable.  Subclasses of {@code Throwable} should document any
     * conditions under which suppression is disabled and document
     * conditions under which the stack trace is not writable.
     * Disabling of suppression should only occur in exceptional
     * circumstances where special requirements exist, such as a
     * virtual machine reusing exception objects under low-memory
     * situations.  Circumstances where a given exception object is
     * repeatedly caught and rethrown, such as to implement control
     * flow between two sub-systems, is another situation where
     * immutable throwable objects would be appropriate.
     *
     * @param message the detail message.
     * @param cause the cause.  (A {@code null} value is permitted,
     * and indicates that the cause is nonexistent or unknown.)
     * @param enableSuppression whether or not suppression is enabled or disabled
     * @param writableStackTrace whether or not the stack trace should be
     *                           writable
     */
    protected Throwable(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        detailMessage = message;
        this.cause = cause;
        if (!enableSuppression)
            suppressedExceptions = null;
    }

    /**
     * Returns the detail message string of this throwable.
     *
     * @return the detail message string of this {@code Throwable} instance
     *          (which may be {@code null}).
     */
    public String getMessage() {
        return detailMessage;
    }

    /**
     * Creates a localized description of this throwable.
     * Subclasses may override this method in order to produce a
     * locale-specific message.  For subclasses that do not override this
     * method, the default implementation returns the same result as
     * {@code getMessage()}.
     *
     * @return The localized description of this throwable.
     */
    public String getLocalizedMessage() {
        return getMessage();
    }

    /**
     * Returns the cause of this throwable or {@code null} if the
     * cause is nonexistent or unknown.  (The cause is the throwable that
     * caused this throwable to get thrown.)
     *
     * This implementation returns the cause that was supplied via one of
     * the constructors requiring a {@code Throwable}, or that was set after
     * creation with the {@link #initCause(Throwable)} method.  While it is
     * typically unnecessary to override this method, a subclass can override
     * it to return a cause set by some other means.  This is appropriate for
     * a "legacy chained throwable" that predates the addition of chained
     * exceptions to {@code Throwable}.  Note that it is <i>not</i>
     * necessary to override any of the {@code PrintStackTrace} methods,
     * all of which invoke the {@code getCause} method to determine the
     * cause of a throwable.
     *
     * @return the cause of this throwable or {@code null} if the
     *          cause is nonexistent or unknown.
     */
    public synchronized Throwable getCause() {
        return (cause==this ? null : cause);
    }

    /**
     * Initializes the <i>cause</i> of this throwable to the specified value.
     * (The cause is the throwable that caused this throwable to get thrown.)
     *
     * This method can be called at most once.  It is generally called from
     * within the constructor, or immediately after creating the
     * throwable.  If this throwable was created
     * with {@link #Throwable(Throwable)} or
     * {@link #Throwable(String,Throwable)}, this method cannot be called
     * even once.
     *
     * An example of using this method on a legacy throwable type
     * without other support for setting the cause is:
     *
     * <pre>
     * try {
     *     lowLevelOp();
     * } catch (LowLevelException le) {
     *     throw (HighLevelException)
     *           new HighLevelException().initCause(le); // Legacy constructor
     * }
     * </pre>
     *
     * @param cause the cause (which is saved for later retrieval by the
     *         {@link #getCause()} method).  (A {@code null} value is
     *         permitted, and indicates that the cause is nonexistent or
     *         unknown.)
     * @return a reference to this {@code Throwable} instance.
     * @throws IllegalArgumentException if {@code cause} is this
     *         throwable.  (A throwable cannot be its own cause.)
     * @throws IllegalStateException if this throwable was
     *         created with {@link #Throwable(Throwable)} or
     *         {@link #Throwable(String,Throwable)}, or this method has already
     *         been called on this throwable.
     */
    public synchronized Throwable initCause(Throwable cause) {
        if (this.cause != this)
            throw new IllegalStateException("Can't overwrite cause with " + Objects.toString(cause, "a null"), this);
        if (cause == this)
            throw new IllegalArgumentException("Self-causation not permitted", this);
        this.cause = cause;
        return this;
    }

    /**
     * Returns a short description of this throwable.
     * The result is the concatenation of:
     * <ul>
     * <li>the {@linkplain Class#getName() name} of the class of this object
     * <li>": " (a colon and a space)
     * <li>the result of invoking this object's {@link #getLocalizedMessage}
     *      method
     * </ul>
     * If {@code getLocalizedMessage} returns {@code null}, then just
     * the class name is returned.
     *
     * @return a string representation of this throwable.
     */
    public String toString() {
        String s = getClass().getName();
        String message = getLocalizedMessage();
        return (message != null) ? (s + ": " + message) : s;
    }

    /**
     * Appends the specified exception to the exceptions that were
     * suppressed in order to deliver this exception. This method is
     * thread-safe and typically called (automatically and implicitly)
     * by the {@code try}-with-resources statement.
     *
     * The suppression behavior is enabled <em>unless</em> disabled
     * {@linkplain #Throwable(String, Throwable, boolean, boolean) via
     * a constructor}.  When suppression is disabled, this method does
     * nothing other than to validate its argument.
     *
     * Note that when one exception {@linkplain
     * #initCause(Throwable) causes} another exception, the first
     * exception is usually caught and then the second exception is
     * thrown in response.  In other words, there is a causal
     * connection between the two exceptions.
     *
     * In contrast, there are situations where two independent
     * exceptions can be thrown in sibling code blocks, in particular
     * in the {@code try} block of a {@code try}-with-resources
     * statement and the compiler-generated {@code finally} block
     * which closes the resource.
     *
     * In these situations, only one of the thrown exceptions can be
     * propagated.  In the {@code try}-with-resources statement, when
     * there are two such exceptions, the exception originating from
     * the {@code try} block is propagated and the exception from the
     * {@code finally} block is added to the list of exceptions
     * suppressed by the exception from the {@code try} block.  As an
     * exception unwinds the stack, it can accumulate multiple
     * suppressed exceptions.
     *
     * An exception may have suppressed exceptions while also being
     * caused by another exception.  Whether or not an exception has a
     * cause is semantically known at the time of its creation, unlike
     * whether or not an exception will suppress other exceptions
     * which is typically only determined after an exception is
     * thrown.
     *
     * Note that programmer written code is also able to take
     * advantage of calling this method in situations where there are
     * multiple sibling exceptions and only one can be propagated.
     *
     * @param exception the exception to be added to the list of
     *        suppressed exceptions
     * @throws IllegalArgumentException if {@code exception} is this
     *         throwable; a throwable cannot suppress itself.
     * @throws NullPointerException if {@code exception} is {@code null}
     */
    public final synchronized void addSuppressed(Throwable exception) {
        if (exception == this)
            throw new IllegalArgumentException(SELF_SUPPRESSION_MESSAGE, exception);

        if (exception == null)
            throw new NullPointerException(NULL_CAUSE_MESSAGE);

        if (suppressedExceptions == null) // Suppressed exceptions not recorded
            return;

        if (suppressedExceptions == SUPPRESSED_SENTINEL)
            suppressedExceptions = new ArrayList<>(1);

        suppressedExceptions.add(exception);
    }

    private static final Throwable[] EMPTY_THROWABLE_ARRAY = new Throwable[0];

    /**
     * Returns an array containing all of the exceptions that were
     * suppressed, typically by the {@code try}-with-resources
     * statement, in order to deliver this exception.
     *
     * If no exceptions were suppressed or {@linkplain
     * #Throwable(String, Throwable, boolean, boolean) suppression is
     * disabled}, an empty array is returned.  This method is
     * thread-safe.  Writes to the returned array do not affect future
     * calls to this method.
     *
     * @return an array containing all of the exceptions that were
     *         suppressed to deliver this exception.
     */
    public final synchronized Throwable[] getSuppressed() {
        if (suppressedExceptions == SUPPRESSED_SENTINEL || suppressedExceptions == null)
            return EMPTY_THROWABLE_ARRAY;
        else
            return suppressedExceptions.toArray(EMPTY_THROWABLE_ARRAY);
    }
}

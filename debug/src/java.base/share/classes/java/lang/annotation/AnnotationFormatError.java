package java.lang.annotation;

/**
 * Thrown when the annotation parser attempts to read an annotation
 * from a class file and determines that the annotation is malformed.
 * This error can be thrown by the {@linkplain
 * java.lang.reflect.AnnotatedElement API used to read annotations
 * reflectively}.
 */
public class AnnotationFormatError extends Error {
    /**
     * Constructs a new {@code AnnotationFormatError} with the specified
     * detail message.
     *
     * @param message   the detail message.
     */
    public AnnotationFormatError(String message) {
        super(message);
    }

    /**
     * Constructs a new {@code AnnotationFormatError} with the specified
     * detail message and cause.  Note that the detail message associated
     * with {@code cause} is <i>not</i> automatically incorporated in
     * this error's detail message.
     *
     * @param message the detail message
     * @param cause the cause (A {@code null} value is permitted, and
     *     indicates that the cause is nonexistent or unknown.)
     */
    public AnnotationFormatError(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Constructs a new {@code AnnotationFormatError} with the specified
     * cause and a detail message of
     * {@code (cause == null ? null : cause.toString())} (which
     * typically contains the class and detail message of {@code cause}).
     *
     * @param cause the cause (A {@code null} value is permitted, and
     *     indicates that the cause is nonexistent or unknown.)
     */
    public AnnotationFormatError(Throwable cause) {
        super(cause);
    }
}

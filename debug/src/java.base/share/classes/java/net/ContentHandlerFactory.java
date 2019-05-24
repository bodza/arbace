package java.net;

/**
 * This interface defines a factory for content handlers. An
 * implementation of this interface should map a MIME type into an
 * instance of {@code ContentHandler}.
 *
 * This interface is used by the {@code URLStreamHandler} class
 * to create a {@code ContentHandler} for a MIME type.
 */
public interface ContentHandlerFactory {
    /**
     * Creates a new {@code ContentHandler} to read an object from
     * a {@code URLStreamHandler}.
     *
     * @param mimetype   the MIME type for which a content handler is desired.
     *
     * @return a new {@code ContentHandler} to read an object from a
     *          {@code URLStreamHandler}.
     */
    ContentHandler createContentHandler(String mimetype);
}

package java.net;

/**
 * This interface defines a factory for {@code URL} stream
 * protocol handlers.
 *
 * A URL stream handler factory is used as specified in the
 * {@linkplain java.net.URL#URL(String,String,int,String) URL constructor}.
 */
public interface URLStreamHandlerFactory {
    /**
     * Creates a new {@code URLStreamHandler} instance with the specified
     * protocol.
     *
     * @param protocol   the protocol ("{@code ftp}",
     *                     "{@code http}", "{@code nntp}", etc.).
     * @return a {@code URLStreamHandler} for the specific protocol, or {@code
     *          null} if this factory cannot create a handler for the specific
     *          protocol
     */
    URLStreamHandler createURLStreamHandler(String protocol);
}

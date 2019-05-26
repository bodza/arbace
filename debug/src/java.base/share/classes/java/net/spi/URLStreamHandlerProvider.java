package java.net.spi;

import java.net.URLStreamHandlerFactory;

/**
 * URL stream handler service-provider class.
 *
 *A URL stream handler provider is a concrete subclass of this class that
 * has a zero-argument constructor. URL stream handler providers may be
 * installed in an instance of the Java platform by adding them to the
 * application class path.
 *
 * A URL stream handler provider identifies itself with a
 * provider-configuration file named java.net.spi.URLStreamHandlerProvider in
 * the resource directory META-INF/services. The file should contain a list of
 * fully-qualified concrete URL stream handler provider class names, one per line.
 *
 * URL stream handler providers are located at runtime, as specified in the
 * {@linkplain java.net.URL#URL(String,String,int,String) URL constructor}.
 */
public abstract class URLStreamHandlerProvider implements URLStreamHandlerFactory {
    private URLStreamHandlerProvider(Void ignore) { }

    /**
     * Initializes a new URL stream handler provider.
     */
    protected URLStreamHandlerProvider() {
        this(null);
    }
}

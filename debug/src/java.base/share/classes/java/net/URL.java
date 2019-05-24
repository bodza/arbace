package java.net;

import java.io.IOException;
import java.io.InputStream;
import java.net.spi.URLStreamHandlerProvider;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Class {@code URL} represents a Uniform Resource
 * Locator, a pointer to a "resource" on the World
 * Wide Web. A resource can be something as simple as a file or a
 * directory, or it can be a reference to a more complicated object,
 * such as a query to a database or to a search engine. More
 * information on the types of URLs and their formats can be found at:
 * <a href=
 * "http://web.archive.org/web/20051219043731/http://archive.ncsa.uiuc.edu/SDG/Software/Mosaic/Demo/url-primer.html">
 * <i>Types of URL</i></a>
 *
 * In general, a URL can be broken into several parts. Consider the
 * following example:
 * <blockquote><pre>
 *     http://www.example.com/docs/resource1.html
 * </pre></blockquote>
 *
 * The URL above indicates that the protocol to use is
 * {@code http} (HyperText Transfer Protocol) and that the
 * information resides on a host machine named
 * {@code www.example.com}. The information on that host
 * machine is named {@code /docs/resource1.html}. The exact
 * meaning of this name on the host machine is both protocol
 * dependent and host dependent. The information normally resides in
 * a file, but it could be generated on the fly. This component of
 * the URL is called the <i>path</i> component.
 *
 * A URL can optionally specify a "port", which is the
 * port number to which the TCP connection is made on the remote host
 * machine. If the port is not specified, the default port for
 * the protocol is used instead. For example, the default port for
 * {@code http} is {@code 80}. An alternative port could be
 * specified as:
 * <blockquote><pre>
 *     http://www.example.com:1080/docs/resource1.html
 * </pre></blockquote>
 *
 * The syntax of {@code URL} is defined by  <a
 * href="http://www.ietf.org/rfc/rfc2396.txt"><i>RFC&nbsp;2396: Uniform
 * Resource Identifiers (URI): Generic Syntax</i></a>, amended by <a
 * href="http://www.ietf.org/rfc/rfc2732.txt"><i>RFC&nbsp;2732: Format for
 * Literal IPv6 Addresses in URLs</i></a>. The Literal IPv6 address format
 * also supports scope_ids. The syntax and usage of scope_ids is described
 * <a href="Inet6Address.html#scoped">here</a>.
 *
 * A URL may have appended to it a "fragment", also known
 * as a "ref" or a "reference". The fragment is indicated by the sharp
 * sign character "#" followed by more characters. For example,
 * <blockquote><pre>
 *     http://java.sun.com/index.html#chapter1
 * </pre></blockquote>
 *
 * This fragment is not technically part of the URL. Rather, it
 * indicates that after the specified resource is retrieved, the
 * application is specifically interested in that part of the
 * document that has the tag {@code chapter1} attached to it. The
 * meaning of a tag is resource specific.
 *
 * An application can also specify a "relative URL",
 * which contains only enough information to reach the resource
 * relative to another URL. Relative URLs are frequently used within
 * HTML pages. For example, if the contents of the URL:
 * <blockquote><pre>
 *     http://java.sun.com/index.html
 * </pre></blockquote>
 * contained within it the relative URL:
 * <blockquote><pre>
 *     FAQ.html
 * </pre></blockquote>
 * it would be a shorthand for:
 * <blockquote><pre>
 *     http://java.sun.com/FAQ.html
 * </pre></blockquote>
 *
 * The relative URL need not specify all the components of a URL. If
 * the protocol, host name, or port number is missing, the value is
 * inherited from the fully specified URL. The file component must be
 * specified. The optional fragment is not inherited.
 *
 * The URL class does not itself encode or decode any URL components
 * according to the escaping mechanism defined in RFC2396. It is the
 * responsibility of the caller to encode any fields, which need to be
 * escaped prior to calling URL, and also to decode any escaped fields,
 * that are returned from URL. Furthermore, because URL has no knowledge
 * of URL escaping, it does not recognise equivalence between the encoded
 * or decoded form of the same URL. For example, the two URLs:<br>
 * <pre>http://foo.com/hello world/ and http://foo.com/hello%20world</pre>
 * would be considered not equal to each other.
 *
 * Note, the {@link java.net.URI} class does perform escaping of its
 * component fields in certain circumstances. The recommended way
 * to manage the encoding and decoding of URLs is to use {@link java.net.URI},
 * and to convert between these two classes using {@link #toURI()} and
 * {@link URI#toURL()}.
 *
 * The {@link URLEncoder} and {@link URLDecoder} classes can also be
 * used, but only for HTML form encoding, which is not the same
 * as the encoding scheme defined in RFC2396.
 */
public final class URL {
    /**
     * The protocol to use (ftp, http, nntp, ... etc.) .
     */
    private String protocol;

    /**
     * The host name to connect to.
     */
    private String host;

    /**
     * The protocol port to connect to.
     */
    private int port = -1;

    /**
     * The specified file name on that host. {@code file} is
     * defined as {@code path[?query]}
     */
    private String file;

    /**
     * The query part of this URL.
     */
    private transient String query;

    /**
     * The authority part of this URL.
     */
    private String authority;

    /**
     * The path part of this URL.
     */
    private transient String path;

    /**
     * The userinfo part of this URL.
     */
    private transient String userInfo;

    /**
     * # reference.
     */
    private String ref;

    /**
     * The host's IP address, used in equals and hashCode.
     * Computed on demand. An uninitialized or unknown hostAddress is null.
     */
    transient InetAddress hostAddress;

    /**
     * The URLStreamHandler for this URL.
     */
    /* oops! */public transient URLStreamHandler handler;

    /* Our hash code.
     */
    private int hashCode = -1;

    /**
     * Creates a {@code URL} object from the specified
     * {@code protocol}, {@code host}, {@code port}
     * number, and {@code file}.
     *
     * {@code host} can be expressed as a host name or a literal
     * IP address. If IPv6 literal address is used, it should be
     * enclosed in square brackets ({@code '['} and {@code ']'}), as
     * specified by <a
     * href="http://www.ietf.org/rfc/rfc2732.txt">RFC&nbsp;2732</a>;
     * However, the literal IPv6 address format defined in <a
     * href="http://www.ietf.org/rfc/rfc2373.txt"><i>RFC&nbsp;2373: IP
     * Version 6 Addressing Architecture</i></a> is also accepted.
     *
     * Specifying a {@code port} number of {@code -1}
     * indicates that the URL should use the default port for the
     * protocol.
     *
     * If this is the first URL object being created with the specified
     * protocol, a <i>stream protocol handler</i> object, an instance of
     * class {@code URLStreamHandler}, is created for that protocol:
     * <ol>
     * <li>If the application has previously set up an instance of
     *     {@code URLStreamHandlerFactory} as the stream handler factory,
     *     then the {@code createURLStreamHandler} method of that instance
     *     is called with the protocol string as an argument to create the
     *     stream protocol handler.
     * <li>If the previous step fails to find a protocol handler, the
     *     constructor reads the value of the system property:
     *     <blockquote>{@code
     *         java.protocol.handler.pkgs
     *     }</blockquote>
     *     If the value of that system property is not {@code null},
     *     it is interpreted as a list of packages separated by a vertical
     *     slash character '{@code |}'. The constructor tries to load
     *     the class named:
     *     <blockquote>{@code
     *         <package>.<protocol>.Handler
     *     }</blockquote>
     *     where {@code <package>} is replaced by the name of the package
     *     and {@code <protocol>} is replaced by the name of the protocol.
     *     If this class does not exist, or if the class exists but it is not
     *     a subclass of {@code URLStreamHandler}, then the next package
     *     in the list is tried.
     * <li>If the previous step fails to find a protocol handler, then the
     *     constructor tries to load a built-in protocol handler.
     *     If this class does not exist, or if the class exists but it is not a
     *     subclass of {@code URLStreamHandler}, then a
     *     {@code MalformedURLException} is thrown.
     * </ol>
     *
     * Protocol handlers for the following protocols are guaranteed
     * to exist on the search path :-
     * <blockquote><pre>
     *     http, https, file, and jar
     * </pre></blockquote>
     * Protocol handlers for additional protocols may also be  available.
     * Some protocol handlers, for example those used for loading platform
     * classes or classes on the class path, may not be overridden. The details
     * of such restrictions, and when those restrictions apply (during
     * initialization of the runtime for example), are implementation specific
     * and therefore not specified
     *
     * No validation of the inputs is performed by this constructor.
     *
     * @param protocol   the name of the protocol to use.
     * @param host       the name of the host.
     * @param port       the port number on the host.
     * @param file       the file on the host
     * @throws MalformedURLException  if an unknown protocol or the port
     *                  is a negative number other than -1
     */
    public URL(String protocol, String host, int port, String file) throws MalformedURLException
    {
        this(protocol, host, port, file, null);
    }

    /**
     * Creates a URL from the specified {@code protocol}
     * name, {@code host} name, and {@code file} name. The
     * default port for the specified protocol is used.
     *
     * This constructor is equivalent to the four-argument
     * constructor with the only difference of using the
     * default port for the specified protocol.
     *
     * No validation of the inputs is performed by this constructor.
     *
     * @param protocol   the name of the protocol to use.
     * @param host       the name of the host.
     * @param file       the file on the host.
     * @throws MalformedURLException  if an unknown protocol is specified.
     */
    public URL(String protocol, String host, String file) throws MalformedURLException {
        this(protocol, host, -1, file);
    }

    /**
     * Creates a {@code URL} object from the specified
     * {@code protocol}, {@code host}, {@code port}
     * number, {@code file}, and {@code handler}. Specifying
     * a {@code port} number of {@code -1} indicates that
     * the URL should use the default port for the protocol. Specifying
     * a {@code handler} of {@code null} indicates that the URL
     * should use a default stream handler for the protocol, as outlined
     * for:
     *     java.net.URL#URL(java.lang.String, java.lang.String, int,
     *                      java.lang.String)
     *
     * No validation of the inputs is performed by this constructor.
     *
     * @param protocol   the name of the protocol to use.
     * @param host       the name of the host.
     * @param port       the port number on the host.
     * @param file       the file on the host
     * @param handler    the stream handler for the URL.
     * @throws MalformedURLException  if an unknown protocol or the port
                        is a negative number other than -1
     */
    public URL(String protocol, String host, int port, String file, URLStreamHandler handler) throws MalformedURLException {
        protocol = toLowerCase(protocol);
        this.protocol = protocol;
        if (host != null) {
            /**
             * if host is a literal IPv6 address,
             * we will make it conform to RFC 2732
             */
            if (host.indexOf(':') >= 0 && !host.startsWith("[")) {
                host = "["+host+"]";
            }
            this.host = host;

            if (port < -1) {
                throw new MalformedURLException("Invalid port number :" + port);
            }
            this.port = port;
            authority = (port == -1) ? host : host + ":" + port;
        }

        int index = file.indexOf('#');
        this.ref = index < 0 ? null : file.substring(index + 1);
        file = index < 0 ? file : file.substring(0, index);
        int q = file.lastIndexOf('?');
        if (q != -1) {
            this.query = file.substring(q + 1);
            this.path = file.substring(0, q);
            this.file = path + "?" + query;
        } else {
            this.path = file;
            this.file = path;
        }

        // Note: we don't do validation of the URL here. Too risky to change
        // right now, but worth considering for future reference. -br
        if (handler == null && (handler = getURLStreamHandler(protocol)) == null) {
            throw new MalformedURLException("unknown protocol: " + protocol);
        }
        this.handler = handler;
    }

    /**
     * Creates a {@code URL} object from the {@code String}
     * representation.
     *
     * This constructor is equivalent to a call to the two-argument
     * constructor with a {@code null} first argument.
     *
     * @param spec   the {@code String} to parse as a URL.
     * @throws MalformedURLException  if no protocol is specified, or an
     *               unknown protocol is found, or {@code spec} is {@code null},
     *               or the parsed URL fails to comply with the specific syntax
     *               of the associated protocol.
     */
    public URL(String spec) throws MalformedURLException {
        this(null, spec);
    }

    /**
     * Creates a URL by parsing the given spec within a specified context.
     *
     * The new URL is created from the given context URL and the spec
     * argument as described in
     * RFC2396 &quot;Uniform Resource Identifiers : Generic * Syntax&quot; :
     * <blockquote><pre>
     *          &lt;scheme&gt;://&lt;authority&gt;&lt;path&gt;?&lt;query&gt;#&lt;fragment&gt;
     * </pre></blockquote>
     * The reference is parsed into the scheme, authority, path, query and
     * fragment parts. If the path component is empty and the scheme,
     * authority, and query components are undefined, then the new URL is a
     * reference to the current document. Otherwise, the fragment and query
     * parts present in the spec are used in the new URL.
     *
     * If the scheme component is defined in the given spec and does not match
     * the scheme of the context, then the new URL is created as an absolute
     * URL based on the spec alone. Otherwise the scheme component is inherited
     * from the context URL.
     *
     * If the authority component is present in the spec then the spec is
     * treated as absolute and the spec authority and path will replace the
     * context authority and path. If the authority component is absent in the
     * spec then the authority of the new URL will be inherited from the
     * context.
     *
     * If the spec's path component begins with a slash character
     * &quot;/&quot; then the
     * path is treated as absolute and the spec path replaces the context path.
     *
     * Otherwise, the path is treated as a relative path and is appended to the
     * context path, as described in RFC2396. Also, in this case,
     * the path is canonicalized through the removal of directory
     * changes made by occurrences of &quot;..&quot; and &quot;.&quot;.
     *
     * For a more detailed description of URL parsing, refer to RFC2396.
     *
     * @param context   the context in which to parse the specification.
     * @param spec      the {@code String} to parse as a URL.
     * @throws MalformedURLException  if no protocol is specified, or an
     *               unknown protocol is found, or {@code spec} is {@code null},
     *               or the parsed URL fails to comply with the specific syntax
     *               of the associated protocol.
     */
    public URL(URL context, String spec) throws MalformedURLException {
        this(context, spec, null);
    }

    /**
     * Creates a URL by parsing the given spec with the specified handler
     * within a specified context. If the handler is null, the parsing
     * occurs as with the two argument constructor.
     *
     * @param context   the context in which to parse the specification.
     * @param spec      the {@code String} to parse as a URL.
     * @param handler   the stream handler for the URL.
     * @throws MalformedURLException  if no protocol is specified, or an
     *               unknown protocol is found, or {@code spec} is {@code null},
     *               or the parsed URL fails to comply with the specific syntax
     *               of the associated protocol.
     */
    public URL(URL context, String spec, URLStreamHandler handler) throws MalformedURLException
    {
        String original = spec;
        int i, limit, c;
        int start = 0;
        String newProtocol = null;
        boolean aRef=false;
        boolean isRelative = false;

        try {
            limit = spec.length();
            while ((limit > 0) && (spec.charAt(limit - 1) <= ' ')) {
                limit--; // eliminate trailing whitespace
            }
            while ((start < limit) && (spec.charAt(start) <= ' ')) {
                start++; // eliminate leading whitespace
            }

            if (spec.regionMatches(true, start, "url:", 0, 4)) {
                start += 4;
            }
            if (start < spec.length() && spec.charAt(start) == '#') {
                /* we're assuming this is a ref relative to the context URL.
                 * This means protocols cannot start w/ '#', but we must parse
                 * ref URL's like: "hello:there" w/ a ':' in them.
                 */
                aRef=true;
            }
            for (i = start; !aRef && (i < limit) && ((c = spec.charAt(i)) != '/'); i++) {
                if (c == ':') {
                    String s = toLowerCase(spec.substring(start, i));
                    if (isValidProtocol(s)) {
                        newProtocol = s;
                        start = i + 1;
                    }
                    break;
                }
            }

            // Only use our context if the protocols match.
            protocol = newProtocol;
            if ((context != null) && ((newProtocol == null) || newProtocol.equalsIgnoreCase(context.protocol))) {
                // inherit the protocol handler from the context
                // if not specified to the constructor
                if (handler == null) {
                    handler = context.handler;
                }

                // If the context is a hierarchical URL scheme and the spec
                // contains a matching scheme then maintain backwards
                // compatibility and treat it as if the spec didn't contain
                // the scheme; see 5.2.3 of RFC2396
                if (context.path != null && context.path.startsWith("/"))
                    newProtocol = null;

                if (newProtocol == null) {
                    protocol = context.protocol;
                    authority = context.authority;
                    userInfo = context.userInfo;
                    host = context.host;
                    port = context.port;
                    file = context.file;
                    path = context.path;
                    isRelative = true;
                }
            }

            if (protocol == null) {
                throw new MalformedURLException("no protocol: "+original);
            }

            // Get the protocol handler if not specified or the protocol
            // of the context could not be used
            if (handler == null && (handler = getURLStreamHandler(protocol)) == null) {
                throw new MalformedURLException("unknown protocol: "+protocol);
            }

            this.handler = handler;

            i = spec.indexOf('#', start);
            if (i >= 0) {
                ref = spec.substring(i + 1, limit);
                limit = i;
            }

            /*
             * Handle special case inheritance of query and fragment
             * implied by RFC2396 section 5.2.2.
             */
            if (isRelative && start == limit) {
                query = context.query;
                if (ref == null) {
                    ref = context.ref;
                }
            }

            handler.parseURL(this, spec, start, limit);
        } catch (MalformedURLException e) {
            throw e;
        } catch (Exception e) {
            MalformedURLException exception = new MalformedURLException(e.getMessage());
            exception.initCause(e);
            throw exception;
        }
    }

    /**
     * Creates a URL from a URI, as if by invoking {@code uri.toURL()}.
     */
    static URL fromURI(URI uri) throws MalformedURLException {
        if (!uri.isAbsolute()) {
            throw new IllegalArgumentException("URI is not absolute");
        }
        String protocol = uri.getScheme();

        // In general we need to go via Handler.parseURL, but for the jrt
        // protocol we enforce that the Handler is not overrideable and can
        // optimize URI to URL conversion.
        //
        // Case-sensitive comparison for performance; malformed protocols will
        // be handled correctly by the slow path.
        if (protocol.equals("jrt") && !uri.isOpaque() && uri.getRawFragment() == null) {
            String query = uri.getRawQuery();
            String path = uri.getRawPath();
            String file = (query == null) ? path : path + "?" + query;

            // URL represent undefined host as empty string while URI use null
            String host = uri.getHost();
            if (host == null) {
                host = "";
            }

            int port = uri.getPort();

            return new URL("jrt", host, port, file, null);
        } else {
            return new URL((URL)null, uri.toString(), null);
        }
    }

    /*
     * Returns true if specified string is a valid protocol name.
     */
    private boolean isValidProtocol(String protocol) {
        int len = protocol.length();
        if (len < 1)
            return false;
        char c = protocol.charAt(0);
        if (!Character.isLetter(c))
            return false;
        for (int i = 1; i < len; i++) {
            c = protocol.charAt(i);
            if (!Character.isLetterOrDigit(c) && c != '.' && c != '+' && c != '-') {
                return false;
            }
        }
        return true;
    }

    /**
     * Sets the fields of the URL. This is not a public method so that
     * only URLStreamHandlers can modify URL fields. URLs are
     * otherwise constant.
     *
     * @param protocol the name of the protocol to use
     * @param host the name of the host
     * @param port the port number on the host
     * @param file the file on the host
     * @param ref the internal reference in the URL
     */
    void set(String protocol, String host, int port, String file, String ref) {
        synchronized (this) {
            this.protocol = protocol;
            this.host = host;
            authority = port == -1 ? host : host + ":" + port;
            this.port = port;
            this.file = file;
            this.ref = ref;
            /* This is very important. We must recompute this after the
             * URL has been changed. */
            hashCode = -1;
            hostAddress = null;
            int q = file.lastIndexOf('?');
            if (q != -1) {
                query = file.substring(q+1);
                path = file.substring(0, q);
            } else
                path = file;
        }
    }

    /**
     * Sets the specified 8 fields of the URL. This is not a public method so
     * that only URLStreamHandlers can modify URL fields. URLs are otherwise
     * constant.
     *
     * @param protocol the name of the protocol to use
     * @param host the name of the host
     * @param port the port number on the host
     * @param authority the authority part for the url
     * @param userInfo the username and password
     * @param path the file on the host
     * @param ref the internal reference in the URL
     * @param query the query part of this URL
     */
    void set(String protocol, String host, int port, String authority, String userInfo, String path, String query, String ref) {
        synchronized (this) {
            this.protocol = protocol;
            this.host = host;
            this.port = port;
            this.file = query == null ? path : path + "?" + query;
            this.userInfo = userInfo;
            this.path = path;
            this.ref = ref;
            /* This is very important. We must recompute this after the
             * URL has been changed. */
            hashCode = -1;
            hostAddress = null;
            this.query = query;
            this.authority = authority;
        }
    }

    /**
     * Gets the query part of this {@code URL}.
     *
     * @return the query part of this {@code URL},
     * or <code>null</code> if one does not exist
     */
    public String getQuery() {
        return query;
    }

    /**
     * Gets the path part of this {@code URL}.
     *
     * @return the path part of this {@code URL}, or an
     * empty string if one does not exist
     */
    public String getPath() {
        return path;
    }

    /**
     * Gets the userInfo part of this {@code URL}.
     *
     * @return the userInfo part of this {@code URL}, or
     * <code>null</code> if one does not exist
     */
    public String getUserInfo() {
        return userInfo;
    }

    /**
     * Gets the authority part of this {@code URL}.
     *
     * @return the authority part of this {@code URL}
     */
    public String getAuthority() {
        return authority;
    }

    /**
     * Gets the port number of this {@code URL}.
     *
     * @return the port number, or -1 if the port is not set
     */
    public int getPort() {
        return port;
    }

    /**
     * Gets the default port number of the protocol associated
     * with this {@code URL}. If the URL scheme or the URLStreamHandler
     * for the URL do not define a default port number,
     * then -1 is returned.
     *
     * @return the port number
     */
    public int getDefaultPort() {
        return handler.getDefaultPort();
    }

    /**
     * Gets the protocol name of this {@code URL}.
     *
     * @return the protocol of this {@code URL}.
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * Gets the host name of this {@code URL}, if applicable.
     * The format of the host conforms to RFC 2732, i.e. for a
     * literal IPv6 address, this method will return the IPv6 address
     * enclosed in square brackets ({@code '['} and {@code ']'}).
     *
     * @return the host name of this {@code URL}.
     */
    public String getHost() {
        return host;
    }

    /**
     * Gets the file name of this {@code URL}.
     * The returned file portion will be
     * the same as <code>getPath()</code>, plus the concatenation of
     * the value of <code>getQuery()</code>, if any. If there is
     * no query portion, this method and <code>getPath()</code> will
     * return identical results.
     *
     * @return the file name of this {@code URL},
     * or an empty string if one does not exist
     */
    public String getFile() {
        return file;
    }

    /**
     * Gets the anchor (also known as the "reference") of this
     * {@code URL}.
     *
     * @return the anchor (also known as the "reference") of this
     *          {@code URL}, or <code>null</code> if one does not exist
     */
    public String getRef() {
        return ref;
    }

    /**
     * Compares this URL for equality with another object.
     *
     * If the given object is not a URL then this method immediately returns
     * {@code false}.
     *
     * Two URL objects are equal if they have the same protocol, reference
     * equivalent hosts, have the same port number on the host, and the same
     * file and fragment of the file.
     *
     * Two hosts are considered equivalent if both host names can be resolved
     * into the same IP addresses; else if either host name can't be
     * resolved, the host names must be equal without regard to case; or both
     * host names equal to null.
     *
     * Since hosts comparison requires name resolution, this operation is a
     * blocking operation.
     *
     * Note: The defined behavior for {@code equals} is known to
     * be inconsistent with virtual hosting in HTTP.
     *
     * @param obj   the URL to compare against.
     * @return {@code true} if the objects are the same;
     *          {@code false} otherwise.
     */
    public boolean equals(Object obj) {
        if (!(obj instanceof URL))
            return false;
        URL u2 = (URL)obj;

        return handler.equals(this, u2);
    }

    /**
     * Creates an integer suitable for hash table indexing.
     *
     * The hash code is based upon all the URL components relevant for URL
     * comparison. As such, this operation is a blocking operation.
     *
     * @return a hash code for this {@code URL}.
     */
    public synchronized int hashCode() {
        if (hashCode != -1)
            return hashCode;

        hashCode = handler.hashCode(this);
        return hashCode;
    }

    /**
     * Compares two URLs, excluding the fragment component.
     *
     * Returns {@code true} if this {@code URL} and the
     * {@code other} argument are equal without taking the
     * fragment component into consideration.
     *
     * @param other   the {@code URL} to compare against.
     * @return {@code true} if they reference the same remote object;
     *          {@code false} otherwise.
     */
    public boolean sameFile(URL other) {
        return handler.sameFile(this, other);
    }

    /**
     * Constructs a string representation of this {@code URL}. The
     * string is created by calling the {@code toExternalForm}
     * method of the stream protocol handler for this object.
     *
     * @return a string representation of this object.
     */
    public String toString() {
        return toExternalForm();
    }

    /**
     * Constructs a string representation of this {@code URL}. The
     * string is created by calling the {@code toExternalForm}
     * method of the stream protocol handler for this object.
     *
     * @return a string representation of this object.
     */
    public String toExternalForm() {
        return handler.toExternalForm(this);
    }

    /**
     * Returns a {@link java.net.URI} equivalent to this URL.
     * This method functions in the same way as {@code new URI (this.toString())}.
     *
     * Note, any URL instance that complies with RFC 2396 can be converted
     * to a URI. However, some URLs that are not strictly in compliance
     * can not be converted to a URI.
     *
     * @throws URISyntaxException if this URL is not formatted strictly according to
     *            to RFC2396 and cannot be converted to a URI.
     *
     * @return a URI instance equivalent to this URL.
     */
    public URI toURI() throws URISyntaxException {
        return new URI (toString());
    }

    /**
     * Returns a {@link java.net.URLConnection URLConnection} instance that
     * represents a connection to the remote object referred to by the
     * {@code URL}.
     *
     * A new instance of {@linkplain java.net.URLConnection URLConnection} is
     * created every time when invoking the
     * {@linkplain java.net.URLStreamHandler#openConnection(URL)
     * URLStreamHandler.openConnection(URL)} method of the protocol handler for
     * this URL.
     *
     * It should be noted that a URLConnection instance does not establish
     * the actual network connection on creation. This will happen only when
     * calling {@linkplain java.net.URLConnection#connect() URLConnection.connect()}.
     *
     * If for the URL's protocol (such as HTTP or JAR), there
     * exists a public, specialized URLConnection subclass belonging
     * to one of the following packages or one of their subpackages:
     * java.lang, java.io, java.util, java.net, the connection
     * returned will be of that subclass. For example, for HTTP an
     * HttpURLConnection will be returned.
     *
     * @return a {@link java.net.URLConnection URLConnection} linking
     *             to the URL.
     * @throws IOException  if an I/O exception occurs.
     */
    public URLConnection openConnection() throws java.io.IOException {
        return handler.openConnection(this);
    }

    /**
     * Opens a connection to this {@code URL} and returns an
     * {@code InputStream} for reading from that connection. This
     * method is a shorthand for:
     * <blockquote><pre>
     *     openConnection().getInputStream()
     * </pre></blockquote>
     *
     * @return an input stream for reading from the URL connection.
     * @throws IOException  if an I/O exception occurs.
     */
    public final InputStream openStream() throws java.io.IOException {
        return openConnection().getInputStream();
    }

    /**
     * Gets the contents of this URL. This method is a shorthand for:
     * <blockquote><pre>
     *     openConnection().getContent()
     * </pre></blockquote>
     *
     * @return the contents of this URL.
     * @throws IOException  if an I/O exception occurs.
     */
    public final Object getContent() throws java.io.IOException {
        return openConnection().getContent();
    }

    /**
     * Gets the contents of this URL. This method is a shorthand for:
     * <blockquote><pre>
     *     openConnection().getContent(classes)
     * </pre></blockquote>
     *
     * @param classes an array of Java types
     * @return the content object of this URL that is the first match of
     *               the types specified in the classes array.
     *               null if none of the requested types are supported.
     * @throws IOException  if an I/O exception occurs.
     */
    public final Object getContent(Class<?>[] classes) throws java.io.IOException {
        return openConnection().getContent(classes);
    }

    /**
     * The URLStreamHandler factory.
     */
    private static volatile URLStreamHandlerFactory factory;

    /**
     * Sets an application's {@code URLStreamHandlerFactory}.
     * This method can be called at most once in a given Java Virtual
     * Machine.
     *
     *The {@code URLStreamHandlerFactory} instance is used to
     *construct a stream protocol handler from a protocol name.
     *
     * @param fac   the desired factory.
     * @throws Error  if the application has already set a factory.
     */
    public static void setURLStreamHandlerFactory(URLStreamHandlerFactory fac) {
        synchronized (streamHandlerLock) {
            if (factory != null) {
                throw new Error("factory already defined");
            }
            handlers.clear();

            // safe publication of URLStreamHandlerFactory with volatile write
            factory = fac;
        }
    }

    private static final URLStreamHandlerFactory defaultFactory = new DefaultFactory();

    private static class DefaultFactory implements URLStreamHandlerFactory {
        private static String PREFIX = "sun.net.www.protocol";

        public URLStreamHandler createURLStreamHandler(String protocol) {
            String name = PREFIX + "." + protocol + ".Handler";
            try {
                @SuppressWarnings("deprecation")
                Object o = Class.forName(name).newInstance();
                return (URLStreamHandler)o;
            } catch (ClassNotFoundException x) {
                // ignore
            } catch (Exception e) {
                // For compatibility, all Exceptions are ignored.
                // any number of exceptions can get thrown here
            }
            return null;
        }
    }

    /**
     * The property which specifies the package prefix list to be scanned
     * for protocol handlers.  The value of this property (if any) should
     * be a vertical bar delimited list of package names to search through
     * for a protocol handler to load.  The policy of this class is that
     * all protocol handlers will be in a class called <protocolname>.Handler,
     * and each package in the list is examined in turn for a matching
     * handler.  If none are found (or the property is not specified), the
     * default package prefix, sun.net.www.protocol, is used.  The search
     * proceeds from the first package in the list to the last and stops
     * when a match is found.
     */
    private static URLStreamHandler lookupViaProperty(String protocol) {
        String packagePrefixList = null; // "java.protocol.handler.pkgs"
        if (packagePrefixList == null) {
            // not set
            return null;
        }

        String[] packagePrefixes = packagePrefixList.split("\\|");
        URLStreamHandler handler = null;
        for (int i = 0; handler == null && i<packagePrefixes.length; i++) {
            String packagePrefix = packagePrefixes[i].trim();
            try {
                String clsName = packagePrefix + "." + protocol + ".Handler";
                Class<?> cls = null;
                try {
                    cls = Class.forName(clsName);
                } catch (ClassNotFoundException e) {
                    ClassLoader cl = ClassLoader.getSystemClassLoader();
                    if (cl != null) {
                        cls = cl.loadClass(clsName);
                    }
                }
                if (cls != null) {
                    @SuppressWarnings("deprecation")
                    Object tmp = cls.newInstance();
                    handler = (URLStreamHandler)tmp;
                }
            } catch (Exception e) {
                // any number of exceptions can get thrown here
            }
        }
        return handler;
    }

    /**
     * Returns the protocol in lower case. Special cases known protocols
     * to avoid loading locale classes during startup.
     */
    static String toLowerCase(String protocol) {
        if (protocol.equals("jrt") || protocol.equals("file") || protocol.equals("jar")) {
            return protocol;
        } else {
            return protocol.toLowerCase();
        }
    }

    /**
     * Non-overrideable protocols: "jrt" and "file"
     *
     * Character-based comparison for performance reasons; also ensures
     * case-insensitive comparison in a locale-independent fashion.
     */
    static boolean isOverrideable(String protocol) {
        if (protocol.length() == 3) {
            if ((Character.toLowerCase(protocol.charAt(0)) == 'j') &&
                    (Character.toLowerCase(protocol.charAt(1)) == 'r') &&
                    (Character.toLowerCase(protocol.charAt(2)) == 't')) {
                return false;
            }
        } else if (protocol.length() == 4) {
            if ((Character.toLowerCase(protocol.charAt(0)) == 'f') &&
                    (Character.toLowerCase(protocol.charAt(1)) == 'i') &&
                    (Character.toLowerCase(protocol.charAt(2)) == 'l') &&
                    (Character.toLowerCase(protocol.charAt(3)) == 'e')) {
                return false;
            }
        }
        return true;
    }

    /**
     * A table of protocol handlers.
     */
    static Hashtable<String,URLStreamHandler> handlers = new Hashtable<>();
    private static final Object streamHandlerLock = new Object();

    /**
     * Returns the Stream Handler.
     * @param protocol the protocol to use
     */
    static URLStreamHandler getURLStreamHandler(String protocol) {
        URLStreamHandler handler = handlers.get(protocol);

        if (handler != null) {
            return handler;
        }

        URLStreamHandlerFactory fac;
        boolean checkedWithFactory = false;

        if (isOverrideable(protocol) && jdk.internal.misc.VM.isBooted()) {
            // Use the factory (if any). Volatile read makes
            // URLStreamHandlerFactory appear fully initialized to current thread.
            fac = factory;
            if (fac != null) {
                handler = fac.createURLStreamHandler(protocol);
                checkedWithFactory = true;
            }

            if (handler == null) {
                handler = lookupViaProperty(protocol);
            }
        }

        if (handler == null) {
            // Try the built-in protocol handler
            handler = defaultFactory.createURLStreamHandler(protocol);
        }

        synchronized (streamHandlerLock) {
            URLStreamHandler handler2 = null;

            // Check again with hashtable just in case another
            // thread created a handler since we last checked
            handler2 = handlers.get(protocol);

            if (handler2 != null) {
                return handler2;
            }

            // Check with factory if another thread set a
            // factory since our last check
            if (!checkedWithFactory && (fac = factory) != null) {
                handler2 = fac.createURLStreamHandler(protocol);
            }

            if (handler2 != null) {
                // The handler from the factory must be given more
                // importance. Discard the default handler that
                // this thread created.
                handler = handler2;
            }

            // Insert this handler into the hashtable
            if (handler != null) {
                handlers.put(protocol, handler);
            }
        }
        return handler;
    }

    private void resetState() {
        this.protocol = null;
        this.host = null;
        this.port = -1;
        this.file = null;
        this.authority = null;
        this.ref = null;
        this.hashCode = -1;
        this.handler = null;
        this.query = null;
        this.path = null;
        this.userInfo = null;
    }
}

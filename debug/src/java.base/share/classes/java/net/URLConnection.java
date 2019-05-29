package java.net;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Objects;
import java.util.StringTokenizer;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

/**
 * The abstract class {@code URLConnection} is the superclass
 * of all classes that represent a communications link between the
 * application and a URL. Instances of this class can be used both to
 * read from and to write to the resource referenced by the URL.
 *
 * In general, creating a connection to a URL is a multistep process:
 * <ol>
 * <li>The connection object is created by invoking the
 *     {@link URL#openConnection() openConnection} method on a URL.
 * <li>The setup parameters and general request properties are manipulated.
 * <li>The actual connection to the remote object is made, using the
 *    {@link #connect() connect} method.
 * <li>The remote object becomes available. The header fields and the contents
 *     of the remote object can be accessed.
 * </ol>
 *
 * The setup parameters are modified using the following methods:
 * <ul>
 *   <li>{@code setAllowUserInteraction}
 *   <li>{@code setDoInput}
 *   <li>{@code setDoOutput}
 *   <li>{@code setIfModifiedSince}
 *   <li>{@code setUseCaches}
 * </ul>
 *
 * and the general request properties are modified using the method:
 * <ul>
 *   <li>{@code setRequestProperty}
 * </ul>
 *
 * Default values for the {@code AllowUserInteraction} and
 * {@code UseCaches} parameters can be set using the methods
 * {@code setDefaultAllowUserInteraction} and
 * {@code setDefaultUseCaches}.
 *
 * Each of the above {@code set} methods has a corresponding
 * {@code get} method to retrieve the value of the parameter or
 * general request property. The specific parameters and general
 * request properties that are applicable are protocol specific.
 *
 * The following methods are used to access the header fields and
 * the contents after the connection is made to the remote object:
 * <ul>
 *   <li>{@code getContent}
 *   <li>{@code getHeaderField}
 *   <li>{@code getInputStream}
 *   <li>{@code getOutputStream}
 * </ul>
 *
 * Certain header fields are accessed frequently. The methods:
 * <ul>
 *   <li>{@code getContentEncoding}
 *   <li>{@code getContentLength}
 *   <li>{@code getContentType}
 *   <li>{@code getDate}
 *   <li>{@code getExpiration}
 *   <li>{@code getLastModified}
 * </ul>
 *
 * provide convenient access to these fields. The
 * {@code getContentType} method is used by the
 * {@code getContent} method to determine the type of the remote
 * object; subclasses may find it convenient to override the
 * {@code getContentType} method.
 *
 * In the common case, all of the pre-connection parameters and
 * general request properties can be ignored: the pre-connection
 * parameters and request properties default to sensible values. For
 * most clients of this interface, there are only two interesting
 * methods: {@code getInputStream} and {@code getContent},
 * which are mirrored in the {@code URL} class by convenience methods.
 *
 * More information on the request properties and header fields of
 * an {@code http} connection can be found at:
 * <blockquote><pre>
 * <a href="http://www.ietf.org/rfc/rfc2616.txt">http://www.ietf.org/rfc/rfc2616.txt</a>
 * </pre></blockquote>
 *
 * Invoking the {@code close()} methods on the {@code InputStream} or {@code OutputStream} of an
 * {@code URLConnection} after a request may free network resources associated with this
 * instance, unless particular protocol specifications specify different behaviours
 * for it.
 */
public abstract class URLConnection {
    /**
     * The URL represents the remote object on the World Wide Web to
     * which this connection is opened.
     *
     * The value of this field can be accessed by the
     * {@code getURL} method.
     *
     * The default value of this variable is the value of the URL
     * argument in the {@code URLConnection} constructor.
     */
    protected URL url;

    /**
     * This variable is set by the {@code setDoInput} method. Its
     * value is returned by the {@code getDoInput} method.
     *
     * A URL connection can be used for input and/or output. Setting the
     * {@code doInput} flag to {@code true} indicates that
     * the application intends to read data from the URL connection.
     *
     * The default value of this field is {@code true}.
     */
    protected boolean doInput = true;

    /**
     * This variable is set by the {@code setDoOutput} method. Its
     * value is returned by the {@code getDoOutput} method.
     *
     * A URL connection can be used for input and/or output. Setting the
     * {@code doOutput} flag to {@code true} indicates
     * that the application intends to write data to the URL connection.
     *
     * The default value of this field is {@code false}.
     */
    protected boolean doOutput = false;

    private static boolean defaultAllowUserInteraction = false;

    /**
     * If {@code true}, this {@code URL} is being examined in
     * a context in which it makes sense to allow user interactions such
     * as popping up an authentication dialog. If {@code false},
     * then no user interaction is allowed.
     *
     * The value of this field can be set by the
     * {@code setAllowUserInteraction} method.
     * Its value is returned by the
     * {@code getAllowUserInteraction} method.
     * Its default value is the value of the argument in the last invocation
     * of the {@code setDefaultAllowUserInteraction} method.
     */
    protected boolean allowUserInteraction = defaultAllowUserInteraction;

    private static volatile boolean defaultUseCaches = true;

    /**
     * If {@code true}, the protocol is allowed to use caching
     * whenever it can. If {@code false}, the protocol must always
     * try to get a fresh copy of the object.
     *
     * This field is set by the {@code setUseCaches} method. Its
     * value is returned by the {@code getUseCaches} method.
     *
     * Its default value is the value given in the last invocation of the
     * {@code setDefaultUseCaches} method.
     *
     * The default setting may be overridden per protocol with
     * {@link #setDefaultUseCaches(String,boolean)}.
     */
    protected boolean useCaches;

    private static final Map<String, Boolean> defaultCaching = Collections.synchronizedMap(new HashMap<>());

    /**
     * Some protocols support skipping the fetching of the object unless
     * the object has been modified more recently than a certain time.
     *
     * A nonzero value gives a time as the number of milliseconds since
     * January 1, 1970, GMT. The object is fetched only if it has been
     * modified more recently than that time.
     *
     * This variable is set by the {@code setIfModifiedSince}
     * method. Its value is returned by the
     * {@code getIfModifiedSince} method.
     *
     * The default value of this field is {@code 0}, indicating
     * that the fetching must always occur.
     */
    protected long ifModifiedSince = 0;

    /**
     * If {@code false}, this connection object has not created a
     * communications link to the specified URL. If {@code true},
     * the communications link has been established.
     */
    protected boolean connected = false;

    private int connectTimeout;
    private int readTimeout;

    /**
     * Opens a communications link to the resource referenced by this
     * URL, if such a connection has not already been established.
     *
     * If the {@code connect} method is called when the connection
     * has already been opened (indicated by the {@code connected}
     * field having the value {@code true}), the call is ignored.
     *
     * URLConnection objects go through two phases: first they are
     * created, then they are connected.  After being created, and
     * before being connected, various options can be specified
     * (e.g., doInput and UseCaches).  After connecting, it is an
     * error to try to set them.  Operations that depend on being
     * connected, like getContentLength, will implicitly perform the
     * connection, if necessary.
     *
     * @throws SocketTimeoutException if the timeout expires before
     *               the connection can be established
     * @throws IOException  if an I/O error occurs while opening the
     *               connection.
     */
    public abstract void connect() throws IOException;

    /**
     * Sets a specified timeout value, in milliseconds, to be used
     * when opening a communications link to the resource referenced
     * by this URLConnection.  If the timeout expires before the
     * connection can be established, a
     * java.net.SocketTimeoutException is raised. A timeout of zero is
     * interpreted as an infinite timeout.
     *
     * Some non-standard implementation of this method may ignore
     * the specified timeout. To see the connect timeout set, please
     * call getConnectTimeout().
     *
     * @param timeout an {@code int} that specifies the connect
     *               timeout value in milliseconds
     * @throws IllegalArgumentException if the timeout parameter is negative
     */
    public void setConnectTimeout(int timeout) {
        if (timeout < 0) {
            throw new IllegalArgumentException("timeout can not be negative");
        }
        connectTimeout = timeout;
    }

    /**
     * Returns setting for connect timeout.
     *
     * 0 return implies that the option is disabled
     * (i.e., timeout of infinity).
     *
     * @return an {@code int} that indicates the connect timeout
     *         value in milliseconds
     */
    public int getConnectTimeout() {
        return connectTimeout;
    }

    /**
     * Sets the read timeout to a specified timeout, in
     * milliseconds. A non-zero value specifies the timeout when
     * reading from Input stream when a connection is established to a
     * resource. If the timeout expires before there is data available
     * for read, a java.net.SocketTimeoutException is raised. A
     * timeout of zero is interpreted as an infinite timeout.
     *
     *Some non-standard implementation of this method ignores the
     * specified timeout. To see the read timeout set, please call
     * getReadTimeout().
     *
     * @param timeout an {@code int} that specifies the timeout
     * value to be used in milliseconds
     * @throws IllegalArgumentException if the timeout parameter is negative
     */
    public void setReadTimeout(int timeout) {
        if (timeout < 0) {
            throw new IllegalArgumentException("timeout can not be negative");
        }
        readTimeout = timeout;
    }

    /**
     * Returns setting for read timeout. 0 return implies that the
     * option is disabled (i.e., timeout of infinity).
     *
     * @return an {@code int} that indicates the read timeout
     *         value in milliseconds
     */
    public int getReadTimeout() {
        return readTimeout;
    }

    /**
     * Constructs a URL connection to the specified URL. A connection to
     * the object referenced by the URL is not created.
     *
     * @param url   the specified URL.
     */
    protected URLConnection(URL url) {
        this.url = url;
        if (url == null) {
            this.useCaches = defaultUseCaches;
        } else {
            this.useCaches = getDefaultUseCaches(url.getProtocol());
        }
    }

    /**
     * Returns the value of this {@code URLConnection}'s {@code URL}
     * field.
     *
     * @return the value of this {@code URLConnection}'s {@code URL}
     *          field.
     */
    public URL getURL() {
        return url;
    }

    /**
     * Returns the value of the {@code content-length} header field.
     *
     * <b>Note</b>: {@link #getContentLengthLong() getContentLengthLong()}
     * should be preferred over this method, since it returns a {@code long}
     * instead and is therefore more portable.
     *
     * @return the content length of the resource that this connection's URL
     *          references, {@code -1} if the content length is not known,
     *          or if the content length is greater than Integer.MAX_VALUE.
     */
    public int getContentLength() {
        long l = getContentLengthLong();
        if (l > Integer.MAX_VALUE)
            return -1;
        return (int) l;
    }

    /**
     * Returns the value of the {@code content-length} header field as a long.
     *
     * @return the content length of the resource that this connection's URL
     *          references, or {@code -1} if the content length is
     *          not known.
     */
    public long getContentLengthLong() {
        return getHeaderFieldLong("content-length", -1);
    }

    /**
     * Returns the value of the {@code content-type} header field.
     *
     * @return the content type of the resource that the URL references,
     *          or {@code null} if not known.
     */
    public String getContentType() {
        return getHeaderField("content-type");
    }

    /**
     * Returns the value of the {@code content-encoding} header field.
     *
     * @return the content encoding of the resource that the URL references,
     *          or {@code null} if not known.
     */
    public String getContentEncoding() {
        return getHeaderField("content-encoding");
    }

    /**
     * Returns the value of the {@code expires} header field.
     *
     * @return the expiration date of the resource that this URL references,
     *          or 0 if not known. The value is the number of milliseconds since
     *          January 1, 1970 GMT.
     */
    public long getExpiration() {
        return /* oops! getHeaderFieldDate("expires", 0) */0;
    }

    /**
     * Returns the value of the {@code date} header field.
     *
     * @return the sending date of the resource that the URL references,
     *          or {@code 0} if not known. The value returned is the
     *          number of milliseconds since January 1, 1970 GMT.
     */
    public long getDate() {
        return /* oops! getHeaderFieldDate("date", 0) */0;
    }

    /**
     * Returns the value of the {@code last-modified} header field.
     * The result is the number of milliseconds since January 1, 1970 GMT.
     *
     * @return the date the resource referenced by this
     *          {@code URLConnection} was last modified, or 0 if not known.
     */
    public long getLastModified() {
        return /* oops! getHeaderFieldDate("last-modified", 0) */0;
    }

    /**
     * Returns the value of the named header field.
     *
     * If called on a connection that sets the same header multiple times
     * with possibly different values, only the last value is returned.
     *
     * @param name   the name of a header field.
     * @return the value of the named header field, or {@code null}
     *          if there is no such field in the header.
     */
    public String getHeaderField(String name) {
        return null;
    }

    /**
     * Returns an unmodifiable Map of the header fields.
     * The Map keys are Strings that represent the
     * response-header field names. Each Map value is an
     * unmodifiable List of Strings that represents
     * the corresponding field values.
     *
     * @return a Map of header fields
     */
    public Map<String, List<String>> getHeaderFields() {
        return Collections.emptyMap();
    }

    /**
     * Returns the value of the named field parsed as a number.
     *
     * This form of {@code getHeaderField} exists because some
     * connection types (e.g., {@code http-ng}) have pre-parsed
     * headers. Classes for that connection type can override this method
     * and short-circuit the parsing.
     *
     * @param name      the name of the header field.
     * @param Default   the default value.
     * @return the value of the named field, parsed as an integer. The
     *          {@code Default} value is returned if the field is
     *          missing or malformed.
     */
    public int getHeaderFieldInt(String name, int Default) {
        String value = getHeaderField(name);
        try {
            return Integer.parseInt(value);
        } catch (Exception e) { }
        return Default;
    }

    /**
     * Returns the value of the named field parsed as a number.
     *
     * This form of {@code getHeaderField} exists because some
     * connection types (e.g., {@code http-ng}) have pre-parsed
     * headers. Classes for that connection type can override this method
     * and short-circuit the parsing.
     *
     * @param name      the name of the header field.
     * @param Default   the default value.
     * @return the value of the named field, parsed as a long. The
     *          {@code Default} value is returned if the field is
     *          missing or malformed.
     */
    public long getHeaderFieldLong(String name, long Default) {
        String value = getHeaderField(name);
        try {
            return Long.parseLong(value);
        } catch (Exception e) { }
        return Default;
    }

    /**
     * Returns the key for the {@code n}<sup>th</sup> header field.
     * It returns {@code null} if there are fewer than {@code n+1} fields.
     *
     * @param n   an index, where {@code n>=0}
     * @return the key for the {@code n}<sup>th</sup> header field,
     *          or {@code null} if there are fewer than {@code n+1}
     *          fields.
     */
    public String getHeaderFieldKey(int n) {
        return null;
    }

    /**
     * Returns the value for the {@code n}<sup>th</sup> header field.
     * It returns {@code null} if there are fewer than
     * {@code n+1}fields.
     *
     * This method can be used in conjunction with the
     * {@link #getHeaderFieldKey(int) getHeaderFieldKey} method to iterate through all
     * the headers in the message.
     *
     * @param n   an index, where {@code n>=0}
     * @return the value of the {@code n}<sup>th</sup> header field
     *          or {@code null} if there are fewer than {@code n+1} fields
     */
    public String getHeaderField(int n) {
        return null;
    }

    /**
     * Retrieves the contents of this URL connection.
     *
     * This method first determines the content type of the object by
     * calling the {@code getContentType} method. If this is
     * the first time that the application has seen that specific content
     * type, a content handler for that content type is created.
     *
     * This is done as follows:
     * <ol>
     * <li>If the application has set up a content handler factory instance
     *     using the {@code setContentHandlerFactory} method, the
     *     {@code createContentHandler} method of that instance is called
     *     with the content type as an argument; the result is a content
     *     handler for that content type.
     * <li>Failing that, this method tries to load a content handler
     *     class as defined by {@link java.net.ContentHandler ContentHandler}.
     *     If the class does not exist, or is not a subclass of {@code
     *     ContentHandler}, then an {@code UnknownServiceException} is thrown.
     * </ol>
     *
     * @return the object fetched. The {@code instanceof} operator
     *               should be used to determine the specific kind of object
     *               returned.
     * @throws IOException              if an I/O error occurs while
     *               getting the content.
     * @throws UnknownServiceException  if the protocol does not support
     *               the content type.
     */
    public Object getContent() throws IOException {
        // Must call getInputStream before GetHeaderField gets called
        // so that FileNotFoundException has a chance to be thrown up
        // from here without being caught.
        getInputStream();
        return getContentHandler().getContent(this);
    }

    /**
     * Retrieves the contents of this URL connection.
     *
     * @param classes the {@code Class} array
     * indicating the requested types
     * @return the object fetched that is the first match of the type
     *               specified in the classes array. null if none of
     *               the requested types are supported.
     *               The {@code instanceof} operator should be used to
     *               determine the specific kind of object returned.
     * @throws IOException              if an I/O error occurs while
     *               getting the content.
     * @throws UnknownServiceException  if the protocol does not support
     *               the content type.
     */
    public Object getContent(Class<?>[] classes) throws IOException {
        // Must call getInputStream before GetHeaderField gets called
        // so that FileNotFoundException has a chance to be thrown up
        // from here without being caught.
        getInputStream();
        return getContentHandler().getContent(this, classes);
    }

    /**
     * Returns an input stream that reads from this open connection.
     *
     * A SocketTimeoutException can be thrown when reading from the
     * returned input stream if the read timeout expires before data
     * is available for read.
     *
     * @return an input stream that reads from this open connection.
     * @throws IOException              if an I/O error occurs while
     *               creating the input stream.
     * @throws UnknownServiceException  if the protocol does not support
     *               input.
     */
    public InputStream getInputStream() throws IOException {
        throw new UnknownServiceException("protocol doesn't support input");
    }

    /**
     * Returns an output stream that writes to this connection.
     *
     * @return an output stream that writes to this connection.
     * @throws IOException              if an I/O error occurs while
     *               creating the output stream.
     * @throws UnknownServiceException  if the protocol does not support
     *               output.
     */
    public OutputStream getOutputStream() throws IOException {
        throw new UnknownServiceException("protocol doesn't support output");
    }

    /**
     * Returns a {@code String} representation of this URL connection.
     *
     * @return a string representation of this {@code URLConnection}.
     */
    public String toString() {
        return String.str(this.getClass().getName(), ":", url);
    }

    /**
     * Sets the value of the {@code doInput} field for this
     * {@code URLConnection} to the specified value.
     *
     * A URL connection can be used for input and/or output.  Set the doInput
     * flag to true if you intend to use the URL connection for input,
     * false if not.  The default is true.
     *
     * @param doinput   the new value.
     * @throws IllegalStateException if already connected
     */
    public void setDoInput(boolean doinput) {
        checkConnected();
        doInput = doinput;
    }

    /**
     * Returns the value of this {@code URLConnection}'s
     * {@code doInput} flag.
     *
     * @return the value of this {@code URLConnection}'s
     *          {@code doInput} flag.
     */
    public boolean getDoInput() {
        return doInput;
    }

    /**
     * Sets the value of the {@code doOutput} field for this
     * {@code URLConnection} to the specified value.
     *
     * A URL connection can be used for input and/or output.  Set the doOutput
     * flag to true if you intend to use the URL connection for output,
     * false if not.  The default is false.
     *
     * @param dooutput   the new value.
     * @throws IllegalStateException if already connected
     */
    public void setDoOutput(boolean dooutput) {
        checkConnected();
        doOutput = dooutput;
    }

    /**
     * Returns the value of this {@code URLConnection}'s
     * {@code doOutput} flag.
     *
     * @return the value of this {@code URLConnection}'s
     *          {@code doOutput} flag.
     */
    public boolean getDoOutput() {
        return doOutput;
    }

    /**
     * Set the value of the {@code allowUserInteraction} field of
     * this {@code URLConnection}.
     *
     * @param allowuserinteraction   the new value.
     * @throws IllegalStateException if already connected
     */
    public void setAllowUserInteraction(boolean allowuserinteraction) {
        checkConnected();
        allowUserInteraction = allowuserinteraction;
    }

    /**
     * Returns the value of the {@code allowUserInteraction} field for
     * this object.
     *
     * @return the value of the {@code allowUserInteraction} field for
     *          this object.
     */
    public boolean getAllowUserInteraction() {
        return allowUserInteraction;
    }

    /**
     * Sets the default value of the
     * {@code allowUserInteraction} field for all future
     * {@code URLConnection} objects to the specified value.
     *
     * @param defaultallowuserinteraction   the new value.
     */
    public static void setDefaultAllowUserInteraction(boolean defaultallowuserinteraction) {
        defaultAllowUserInteraction = defaultallowuserinteraction;
    }

    /**
     * Returns the default value of the {@code allowUserInteraction}
     * field.
     *
     * This default is "sticky", being a part of the static state of all
     * URLConnections.  This flag applies to the next, and all following
     * URLConnections that are created.
     *
     * @return the default value of the {@code allowUserInteraction}
     *          field.
     */
    public static boolean getDefaultAllowUserInteraction() {
        return defaultAllowUserInteraction;
    }

    /**
     * Sets the value of the {@code useCaches} field of this
     * {@code URLConnection} to the specified value.
     *
     * Some protocols do caching of documents.  Occasionally, it is important
     * to be able to "tunnel through" and ignore the caches (e.g., the
     * "reload" button in a browser).  If the UseCaches flag on a connection
     * is true, the connection is allowed to use whatever caches it can.
     *  If false, caches are to be ignored.
     *  The default value comes from defaultUseCaches, which defaults to
     * true. A default value can also be set per-protocol using
     * {@link #setDefaultUseCaches(String,boolean)}.
     *
     * @param usecaches a {@code boolean} indicating whether
     * or not to allow caching
     * @throws IllegalStateException if already connected
     */
    public void setUseCaches(boolean usecaches) {
        checkConnected();
        useCaches = usecaches;
    }

    /**
     * Returns the value of this {@code URLConnection}'s
     * {@code useCaches} field.
     *
     * @return the value of this {@code URLConnection}'s
     *          {@code useCaches} field.
     */
    public boolean getUseCaches() {
        return useCaches;
    }

    /**
     * Sets the value of the {@code ifModifiedSince} field of
     * this {@code URLConnection} to the specified value.
     *
     * @param ifmodifiedsince   the new value.
     * @throws IllegalStateException if already connected
     */
    public void setIfModifiedSince(long ifmodifiedsince) {
        checkConnected();
        ifModifiedSince = ifmodifiedsince;
    }

    /**
     * Returns the value of this object's {@code ifModifiedSince} field.
     *
     * @return the value of this object's {@code ifModifiedSince} field.
     */
    public long getIfModifiedSince() {
        return ifModifiedSince;
    }

    /**
     * Returns the default value of a {@code URLConnection}'s
     * {@code useCaches} flag.
     *
     * This default is "sticky", being a part of the static state of all
     * URLConnections.  This flag applies to the next, and all following
     * URLConnections that are created. This default value can be over-ridden
     * per protocol using {@link #setDefaultUseCaches(String,boolean)}
     *
     * @return the default value of a {@code URLConnection}'s
     *          {@code useCaches} flag.
     */
    public boolean getDefaultUseCaches() {
        return defaultUseCaches;
    }

    /**
     * Sets the default value of the {@code useCaches} field to the
     * specified value. This default value can be over-ridden
     * per protocol using {@link #setDefaultUseCaches(String,boolean)}
     *
     * @param defaultusecaches   the new value.
     */
    public void setDefaultUseCaches(boolean defaultusecaches) {
        defaultUseCaches = defaultusecaches;
    }

    /**
     * Sets the default value of the {@code useCaches} field for the named
     * protocol to the given value. This value overrides any default setting
     * set by {@link #setDefaultUseCaches(boolean)} for the given protocol.
     * Successive calls to this method change the setting and affect the
     * default value for all future connections of that protocol. The protocol
     * name is case insensitive.
     *
     * @param protocol the protocol to set the default for
     * @param defaultVal whether caching is enabled by default for the given protocol
     */
    public static void setDefaultUseCaches(String protocol, boolean defaultVal) {
        protocol = protocol.toLowerCase();
        defaultCaching.put(protocol, defaultVal);
    }

    /**
     * Returns the default value of the {@code useCaches} flag for the given protocol. If
     * {@link #setDefaultUseCaches(String,boolean)} was called for the given protocol,
     * then that value is returned. Otherwise, if {@link #setDefaultUseCaches(boolean)}
     * was called, then that value is returned. If neither method was called,
     * the return value is {@code true}. The protocol name is case insensitive.
     *
     * @param protocol the protocol whose defaultUseCaches setting is required
     * @return the default value of the {@code useCaches} flag for the given protocol.
     */
    public static boolean getDefaultUseCaches(String protocol) {
        Boolean protoDefault = defaultCaching.get(protocol.toLowerCase());
        if (protoDefault != null) {
            return protoDefault.booleanValue();
        } else {
            return defaultUseCaches;
        }
    }

    /**
     * The ContentHandler factory.
     */
    private static volatile ContentHandlerFactory factory;

    /**
     * Sets the {@code ContentHandlerFactory} of an
     * application. It can be called at most once by an application.
     *
     * The {@code ContentHandlerFactory} instance is used to
     * construct a content handler from a content type.
     *
     * @param fac   the desired factory.
     * @throws Error  if the factory has already been defined.
     */
    public static synchronized void setContentHandlerFactory(ContentHandlerFactory fac) {
        if (factory != null) {
            throw new Error("factory already defined");
        }
        factory = fac;
    }

    private static final Hashtable<String, ContentHandler> handlers = new Hashtable<>();

    /**
     * Gets the Content Handler appropriate for this connection.
     */
    private ContentHandler getContentHandler() throws UnknownServiceException {
        String contentType = stripOffParameters(getContentType());
        if (contentType == null) {
            throw new UnknownServiceException("no content-type");
        }

        ContentHandler handler = handlers.get(contentType);
        if (handler != null)
            return handler;

        if (factory != null) {
            handler = factory.createContentHandler(contentType);
            if (handler != null)
                return handler;
        }

        try {
            handler = lookupContentHandlerClassFor(contentType);
        } catch (Exception e) {
            handler = UnknownContentHandler.INSTANCE;
        }

        // assert handler != null;

        ContentHandler h = handlers.putIfAbsent(contentType, handler);
        return Objects.requireNonNullElse(h, handler);
    }

    /*
     * Media types are in the format: type/subtype*(; parameter).
     * For looking up the content handler, we should ignore those
     * parameters.
     */
    private String stripOffParameters(String contentType) {
        if (contentType == null)
            return null;
        int index = contentType.indexOf(';');

        if (index > 0)
            return contentType.substring(0, index);
        else
            return contentType;
    }

    private static final String contentClassPrefix = "sun.net.www.content";
    private static final String contentPathProp = "java.content.handler.pkgs";

    /**
     * Looks for a content handler in a user-definable set of places.
     * By default it looks in {@value #contentClassPrefix}, but users can define
     * a vertical-bar delimited set of class prefixes to search through in
     * addition by defining the {@value #contentPathProp} property.
     * The class name must be of the form:
     * <pre>
     *     {package-prefix}.{major}.{minor}
     * e.g.
     *     YoyoDyne.experimental.text.plain
     * </pre>
     */
    private ContentHandler lookupContentHandlerClassFor(String contentType) {
        String contentHandlerClassName = typeToPackageName(contentType);

        String contentHandlerPkgPrefixes = getContentHandlerPkgPrefixes();

        StringTokenizer packagePrefixIter = new StringTokenizer(contentHandlerPkgPrefixes, "|");

        while (packagePrefixIter.hasMoreTokens()) {
            String packagePrefix = packagePrefixIter.nextToken().trim();

            try {
                String clsName = String.str(packagePrefix, ".", contentHandlerClassName);
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
                    // @SuppressWarnings("deprecation")
                    Object tmp = cls.newInstance();
                    return (ContentHandler) tmp;
                }
            } catch (Exception ignored) { }
        }

        return UnknownContentHandler.INSTANCE;
    }

    /**
     * Utility function to map a MIME content type into an equivalent
     * pair of class name components.  For example: "text/html" would
     * be returned as "text.html"
     */
    private String typeToPackageName(String contentType) {
        // make sure we canonicalize the class name: all lower case
        contentType = contentType.toLowerCase();
        int len = contentType.length();
        char nm[] = new char[len];
        contentType.getChars(0, len, nm, 0);
        for (int i = 0; i < len; i++) {
            char c = nm[i];
            if (c == '/') {
                nm[i] = '.';
            } else if (!('A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' || '0' <= c && c <= '9')) {
                nm[i] = '_';
            }
        }
        return new String(nm);
    }

    /**
     * Returns a vertical bar separated list of package prefixes for potential
     * content handlers.  Tries to get the java.content.handler.pkgs property
     * to use as a set of package prefixes to search.  Whether or not
     * that property has been defined, the {@value #contentClassPrefix}
     * is always the last one on the returned package list.
     */
    private String getContentHandlerPkgPrefixes() {
        return contentClassPrefix;
    }

    /**
     * Check for FlashPix image data in InputStream is.  Return true if
     * the stream has FlashPix data, false otherwise.  Before calling this
     * method, the stream should have already been checked to be sure it
     * contains Microsoft Structured Storage data.
     */
    private static boolean checkfpx(InputStream is) throws IOException {
        /* Test for FlashPix image data in Microsoft Structured Storage format.
         * In general, should do this with calls to an SS implementation.
         * Lacking that, need to dig via offsets to get to the FlashPix
         * ClassID.  Details:
         *
         * Offset to Fpx ClsID from beginning of stream should be:
         *
         * FpxClsidOffset = rootEntryOffset + clsidOffset
         *
         * where: clsidOffset = 0x50.
         *        rootEntryOffset = headerSize + sectorSize*sectDirStart
         *                          + 128*rootEntryDirectory
         *
         *        where:  headerSize = 0x200 (always)
         *                sectorSize = 2 raised to power of uSectorShift,
         *                             which is found in the header at
         *                             offset 0x1E.
         *                sectDirStart = found in the header at offset 0x30.
         *                rootEntryDirectory = in general, should search for
         *                                     directory labelled as root.
         *                                     We will assume value of 0 (i.e.,
         *                                     rootEntry is in first directory)
         */

        // Mark the stream so we can reset it. 0x100 is enough for the first
        // few reads, but the mark will have to be reset and set again once
        // the offset to the root directory entry is computed. That offset
        // can be very large and isn't know until the stream has been read from
        is.mark(0x100);

        // Get the byte ordering located at 0x1E. 0xFE is Intel,
        // 0xFF is other
        long toSkip = (long)0x1C;
        long posn;

        if ((posn = skipForward(is, toSkip)) < toSkip) {
          is.reset();
          return false;
        }

        int c[] = new int[16];
        if (readBytes(c, 2, is) < 0) {
            is.reset();
            return false;
        }

        int byteOrder = c[0];

        posn+=2;
        int uSectorShift;
        if (readBytes(c, 2, is) < 0) {
            is.reset();
            return false;
        }

        if (byteOrder == 0xFE) {
            uSectorShift = c[0];
            uSectorShift += c[1] << 8;
        }
        else {
            uSectorShift = c[0] << 8;
            uSectorShift += c[1];
        }

        posn += 2;
        toSkip = (long)0x30 - posn;
        long skipped = 0;
        if ((skipped = skipForward(is, toSkip)) < toSkip) {
          is.reset();
          return false;
        }
        posn += skipped;

        if (readBytes(c, 4, is) < 0) {
            is.reset();
            return false;
        }

        int sectDirStart;
        if (byteOrder == 0xFE) {
            sectDirStart = c[0];
            sectDirStart += c[1] << 8;
            sectDirStart += c[2] << 16;
            sectDirStart += c[3] << 24;
        } else {
            sectDirStart =  c[0] << 24;
            sectDirStart += c[1] << 16;
            sectDirStart += c[2] << 8;
            sectDirStart += c[3];
        }
        posn += 4;
        is.reset(); // Reset back to the beginning

        toSkip = 0x200L + (long)(1<<uSectorShift)*sectDirStart + 0x50L;

        // Sanity check!
        if (toSkip < 0) {
            return false;
        }

        /*
         * How far can we skip? Is there any performance problem here?
         * This skip can be fairly long, at least 0x4c650 in at least
         * one case. Have to assume that the skip will fit in an int.
         * Leave room to read whole root dir
         */
        is.mark((int)toSkip+0x30);

        if ((skipForward(is, toSkip)) < toSkip) {
            is.reset();
            return false;
        }

        /* should be at beginning of ClassID, which is as follows
         * (in Intel byte order):
         *    00 67 61 56 54 C1 CE 11 85 53 00 AA 00 A1 F9 5B
         *
         * This is stored from Windows as long,short,short,char[8]
         * so for byte order changes, the order only changes for
         * the first 8 bytes in the ClassID.
         *
         * Test against this, ignoring second byte (Intel) since
         * this could change depending on part of Fpx file we have.
         */

        if (readBytes(c, 16, is) < 0) {
            is.reset();
            return false;
        }

        // intel byte order
        if (byteOrder == 0xFE &&
            c[0] == 0x00 && c[2] == 0x61 && c[3] == 0x56 &&
            c[4] == 0x54 && c[5] == 0xC1 && c[6] == 0xCE &&
            c[7] == 0x11 && c[8] == 0x85 && c[9] == 0x53 &&
            c[10]== 0x00 && c[11]== 0xAA && c[12]== 0x00 &&
            c[13]== 0xA1 && c[14]== 0xF9 && c[15]== 0x5B) {
            is.reset();
            return true;
        }

        // non-intel byte order
        else if (c[3] == 0x00 && c[1] == 0x61 && c[0] == 0x56 &&
            c[5] == 0x54 && c[4] == 0xC1 && c[7] == 0xCE &&
            c[6] == 0x11 && c[8] == 0x85 && c[9] == 0x53 &&
            c[10]== 0x00 && c[11]== 0xAA && c[12]== 0x00 &&
            c[13]== 0xA1 && c[14]== 0xF9 && c[15]== 0x5B) {
            is.reset();
            return true;
        }
        is.reset();
        return false;
    }

    /**
     * Tries to read the specified number of bytes from the stream
     * Returns -1, If EOF is reached before len bytes are read, returns 0
     * otherwise
     */
    private static int readBytes(int c[], int len, InputStream is) throws IOException {
        byte buf[] = new byte[len];
        if (is.read(buf, 0, len) < len) {
            return -1;
        }

        // fill the passed in int array
        for (int i = 0; i < len; i++) {
             c[i] = buf[i] & 0xff;
        }
        return 0;
    }

    /**
     * Skips through the specified number of bytes from the stream
     * until either EOF is reached, or the specified
     * number of bytes have been skipped
     */
    private static long skipForward(InputStream is, long toSkip) throws IOException {
        long eachSkip = 0;
        long skipped = 0;

        while (skipped != toSkip) {
            eachSkip = is.skip(toSkip - skipped);

            // check if EOF is reached
            if (eachSkip <= 0) {
                if (is.read() == -1) {
                    return skipped;
                } else {
                    skipped++;
                }
            }
            skipped += eachSkip;
        }
        return skipped;
    }

    private void checkConnected() {
        if (connected)
            throw new IllegalStateException("Already connected");
    }
}

class UnknownContentHandler extends ContentHandler {
    static final ContentHandler INSTANCE = new UnknownContentHandler();

    public Object getContent(URLConnection uc) throws IOException {
        return uc.getInputStream();
    }
}

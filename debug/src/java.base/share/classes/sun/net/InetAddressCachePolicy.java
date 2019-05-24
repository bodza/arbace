package sun.net;

public final class InetAddressCachePolicy {
    public static final int FOREVER = -1;
    public static final int NEVER = 0;

    /* default value for positive lookups */
    public static final int DEFAULT_POSITIVE = 30;

    /* The Java-level namelookup cache policy for successful lookups:
     *
     * -1: caching forever
     * any positive value: the number of seconds to cache an address for
     *
     * default value is forever (FOREVER), as we let the platform do the
     * caching. For security reasons, this caching is made forever when
     * a security manager is set.
     */
    private static volatile int cachePolicy = FOREVER;

    /* The Java-level namelookup cache policy for negative lookups:
     *
     * -1: caching forever
     * any positive value: the number of seconds to cache an address for
     *
     * default value is 0. It can be set to some other value for
     * performance reasons.
     */
    private static volatile int negativeCachePolicy = NEVER;

    /* Whether or not the cache policy for successful lookups was set
     * using a property (cmd line).
     */
    private static boolean propertySet;

    /* Whether or not the cache policy for negative lookups was set
     * using a property (cmd line).
     */
    private static boolean propertyNegativeSet;

    static {
        Integer ttl = null; // "networkaddress.cache.ttl" or "sun.net.inetaddr.ttl"
        if (ttl != null) {
            cachePolicy = (ttl < 0) ? FOREVER : ttl;
            propertySet = true;
        } else {
            cachePolicy = DEFAULT_POSITIVE;
        }

        ttl = null; // "networkaddress.cache.negative.ttl" or "sun.net.inetaddr.negative.ttl"
        if (ttl != null) {
            negativeCachePolicy = (ttl < 0) ? FOREVER : ttl;
            propertyNegativeSet = true;
        }
    }

    public static int get() {
        return cachePolicy;
    }

    public static int getNegative() {
        return negativeCachePolicy;
    }

    /**
     * Sets the cache policy for successful lookups if the user has not
     * already specified a cache policy for it using a command-property.
     *
     * @param newPolicy the value in seconds for how long the lookup should be cached
     */
    public static synchronized void setIfNotSet(int newPolicy) {
        /*
         * When setting the new value we may want to signal that the cache
         * should be flushed, though this doesn't seem strictly necessary.
         */
        if (!propertySet) {
            checkValue(newPolicy, cachePolicy);
            cachePolicy = newPolicy;
        }
    }

    /**
     * Sets the cache policy for negative lookups if the user has not
     * already specified a cache policy for it using a command-property.
     *
     * @param newPolicy the value in seconds for how long the lookup should be cached
     */
    public static void setNegativeIfNotSet(int newPolicy) {
        /*
         * When setting the new value we may want to signal that the
         * cache should be flushed, though this doesn't seem strictly
         * necessary.
         */
        if (!propertyNegativeSet) {
            // Negative caching does not seem to have any security
            // implications.
            // checkValue(newPolicy, negativeCachePolicy);
            // but we should normalize negative policy
            negativeCachePolicy = (newPolicy < 0) ? FOREVER : newPolicy;
        }
    }

    private static void checkValue(int newPolicy, int oldPolicy) {
        /*
         * If malicious code gets a hold of this method, prevent
         * setting the cache policy to something laxer or some
         * invalid negative value.
         */
        if (newPolicy == FOREVER)
            return;

        if ((oldPolicy == FOREVER) || (newPolicy < oldPolicy) || (newPolicy < FOREVER)) {
            throw new SecurityException("can't make InetAddress cache more lax");
        }
    }
}

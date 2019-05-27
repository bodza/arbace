package java.net;

/**
 * This class defines a factory for creating DatagramSocketImpls. It defaults
 * to creating plain DatagramSocketImpls, but may create other DatagramSocketImpls
 * by setting the impl.prefix system property.
 */
class DefaultDatagramSocketImplFactory {
    static Class<?> prefixImplClass = null;

    static {
        String prefix = null; // "impl.prefix"
        try {
            if (prefix != null)
                prefixImplClass = Class.forName(String.str("java.net.", prefix, "DatagramSocketImpl"));
        } catch (Exception e) {
            System.err.println(String.str("Can't find class: java.net.", prefix, "DatagramSocketImpl: check impl.prefix property"));
            // prefixImplClass = null;
        }
    }

    /**
     * Creates a new <code>DatagramSocketImpl</code> instance.
     *
     * @param isMulticast     true if this impl if for a MutlicastSocket
     * @return a new instance of a <code>DatagramSocketImpl</code>.
     */
    static DatagramSocketImpl createDatagramSocketImpl(boolean isMulticast /*unused on unix*/) throws SocketException {
        if (prefixImplClass != null) {
            try {
                // @SuppressWarnings("deprecation")
                DatagramSocketImpl result = (DatagramSocketImpl)prefixImplClass.newInstance();
                return result;
            } catch (Exception e) {
                throw new SocketException("can't instantiate DatagramSocketImpl");
            }
        } else {
            return new java.net.PlainDatagramSocketImpl();
        }
    }
}

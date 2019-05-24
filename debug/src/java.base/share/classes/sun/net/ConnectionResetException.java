package sun.net;

import java.net.SocketException;

/**
 * Thrown to indicate a connection reset.
 */
public class ConnectionResetException extends SocketException {
    public ConnectionResetException(String msg) {
        super(msg);
    }

    public ConnectionResetException() {
    }
}

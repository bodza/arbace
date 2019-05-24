package sun.nio.cs;

/*
 * FastPath byte[]->char[] decoder, REPLACE on malformed or
 * unmappable input.
 */

public interface ArrayDecoder {
    int decode(byte[] src, int off, int len, char[] dst);

    default boolean isASCIICompatible() {
        return false;
    }
}

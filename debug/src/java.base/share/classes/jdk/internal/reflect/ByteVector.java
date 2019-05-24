package jdk.internal.reflect;

/** A growable array of bytes. */

interface ByteVector {
    public int  getLength();
    public byte get(int index);
    public void put(int index, byte value);
    public void add(byte value);
    public void trim();
    public byte[] getData();
}

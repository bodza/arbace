package jdk.internal.org.objectweb.asm.commons;

import java.util.Collections;
import java.util.Map;

/**
 * A {@link Remapper} using a {@link Map} to define its mapping.
 */
public class SimpleRemapper extends Remapper {
    private final Map<String, String> mapping;

    public SimpleRemapper(Map<String, String> mapping) {
        this.mapping = mapping;
    }

    public SimpleRemapper(String oldName, String newName) {
        this.mapping = Collections.singletonMap(oldName, newName);
    }

    @Override
    public String mapMethodName(String owner, String name, String desc) {
        String s = map(owner + '.' + name + desc);
        return s == null ? name : s;
    }

    @Override
    public String mapInvokeDynamicMethodName(String name, String desc) {
        String s = map('.' + name + desc);
        return s == null ? name : s;
    }

    @Override
    public String mapFieldName(String owner, String name, String desc) {
        String s = map(owner + '.' + name);
        return s == null ? name : s;
    }

    @Override
    public String map(String key) {
        return mapping.get(key);
    }
}

package jdk.vm.ci.meta;

import java.util.IllegalFormatException;
import java.util.UnknownFormatConversionException;

/**
 * Represents a reference to a Java field, either resolved or unresolved fields.
 */
public interface JavaField {
    /**
     * Returns the name of this field.
     */
    String getName();
}

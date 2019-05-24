package jdk.vm.ci.meta;

import java.util.IllegalFormatException;
import java.util.UnknownFormatConversionException;

/**
 * Represents a reference to a Java method, either resolved or unresolved.
 */
public interface JavaMethod {
    /**
     * Returns the name of this method.
     */
    String getName();
}

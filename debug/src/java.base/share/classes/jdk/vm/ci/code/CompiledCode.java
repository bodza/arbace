package jdk.vm.ci.code;

/**
 * Marker type for an object containing the output of a compiler in a form suitable for installing
 * into a managed code heap. Since the details of a code heap are specific to each runtime, this
 * interface does not specify any methods.
 */
public interface CompiledCode {
}

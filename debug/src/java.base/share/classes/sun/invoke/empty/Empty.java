package sun.invoke.empty;

/**
 * An empty class in an empty package.
 * Used as a proxy for unprivileged code, since making access checks
 * against it will only succeed against public methods in public types.
 *
 * This class also stands (internally to sun.invoke) for the type of a
 * value that cannot be produced, because the expression of this type
 * always returns abnormally.  (Cf. Nothing in the closures proposal.)
 */
public class Empty {
    private Empty() { throw new InternalError(); }
}

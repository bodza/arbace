package sun.reflect.generics.reflectiveObjects;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import sun.reflect.generics.factory.GenericsFactory;
import sun.reflect.generics.tree.FieldTypeSignature;
import sun.reflect.generics.visitor.Reifier;
import sun.reflect.misc.ReflectUtil;

/**
 * Implementation of {@code java.lang.reflect.TypeVariable} interface
 * for core reflection.
 */
public class TypeVariableImpl<D extends GenericDeclaration> extends LazyReflectiveObjectGenerator implements TypeVariable<D> {
    private final D genericDeclaration;
    private final String name;

    /**
     * The upper bounds.  Lazily converted from FieldTypeSignature[] to Type[].
     * We are required to evaluate the bounds lazily, so we store them as ASTs
     * until we are first asked for them.  This also neatly solves the problem
     * with F-bounds - you can't reify them before the formal is defined.
     */
    private volatile Object[] bounds;

    // constructor is private to enforce access through static factory
    private TypeVariableImpl(D decl, String n, FieldTypeSignature[] bs, GenericsFactory f) {
        super(f);
        genericDeclaration = decl;
        name = n;
        bounds = bs;
    }

    /**
     * Factory method.
     * @param decl - the reflective object that declared the type variable
     * that this method should create
     * @param name - the name of the type variable to be returned
     * @param bs - an array of ASTs representing the bounds for the type
     * variable to be created
     * @param f - a factory that can be used to manufacture reflective
     * objects that represent the bounds of this type variable
     * @return A type variable with name, bounds, declaration and factory
     * specified
     */
    public static <T extends GenericDeclaration> TypeVariableImpl<T> make(T decl, String name, FieldTypeSignature[] bs, GenericsFactory f) {
        if (!((decl instanceof Class) || (decl instanceof Method) || (decl instanceof Constructor))) {
            throw new AssertionError(String.str("Unexpected kind of GenericDeclaration", decl.getClass().toString()));
        }
        return new TypeVariableImpl<T>(decl, name, bs, f);
    }

    /**
     * Returns an array of {@code Type} objects representing the
     * upper bound(s) of this type variable.  Note that if no upper bound is
     * explicitly declared, the upper bound is {@code Object}.
     *
     * For each upper bound B:
     * <ul>
     *  <li>if B is a parameterized type or a type variable, it is created,
     *  (see {@link #ParameterizedType} for the details of the creation
     *  process for parameterized types).
     *  <li>Otherwise, B is resolved.
     * </ul>
     *
     * @throws {@code TypeNotPresentException} if any of the
     *     bounds refers to a non-existent type declaration
     * @throws {@code MalformedParameterizedTypeException} if any of the
     *     bounds refer to a parameterized type that cannot be instantiated
     *     for any reason
     * @return an array of Types representing the upper bound(s) of this
     *     type variable
     */
    public Type[] getBounds() {
        Object[] value = bounds;
        if (value instanceof FieldTypeSignature[]) {
            value = reifyBounds((FieldTypeSignature[])value);
            bounds = value;
        }
        return (Type[])value.clone();
    }

    /**
     * Returns the {@code GenericDeclaration} object representing the
     * generic declaration that declared this type variable.
     *
     * @return the generic declaration that declared this type variable.
     */
    public D getGenericDeclaration() {
        if (genericDeclaration instanceof Class)
            ;
        else if ((genericDeclaration instanceof Method) || (genericDeclaration instanceof Constructor))
            ;
        else
            throw new AssertionError("Unexpected kind of GenericDeclaration");
        return genericDeclaration;
    }

    /**
     * Returns the name of this type variable, as it occurs in the source code.
     *
     * @return the name of this type variable, as it appears in the source code
     */
    public String getName() { return name; }

    public String toString() {return getName();}

    // @Override
    public boolean equals(Object o) {
        if (o instanceof TypeVariable && o.getClass() == TypeVariableImpl.class) {
            TypeVariable<?> that = (TypeVariable<?>) o;

            GenericDeclaration thatDecl = that.getGenericDeclaration();
            String thatName = that.getName();

            return Objects.equals(genericDeclaration, thatDecl) && Objects.equals(name, thatName);
        } else
            return false;
    }

    // @Override
    public int hashCode() {
        return genericDeclaration.hashCode() ^ name.hashCode();
    }
}

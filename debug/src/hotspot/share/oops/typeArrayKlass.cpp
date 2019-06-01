#include "precompiled.hpp"

#include "classfile/moduleEntry.hpp"
#include "classfile/packageEntry.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "memory/metadataFactory.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "memory/universe.hpp"
#include "oops/arrayKlass.inline.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/klass.inline.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayKlass.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "utilities/macros.hpp"

bool TypeArrayKlass::compute_is_subtype_of(Klass* k) {
  if (!k->is_typeArray_klass()) {
    return ArrayKlass::compute_is_subtype_of(k);
  }

  TypeArrayKlass* tak = TypeArrayKlass::cast(k);
  if (dimension() != tak->dimension()) return false;

  return element_type() == tak->element_type();
}

TypeArrayKlass* TypeArrayKlass::create_klass(BasicType type, const char* name_str, TRAPS) {
  Symbol* sym = NULL;
  if (name_str != NULL) {
    sym = SymbolTable::new_permanent_symbol(name_str, CHECK_NULL);
  }

  ClassLoaderData* null_loader_data = ClassLoaderData::the_null_class_loader_data();

  TypeArrayKlass* ak = TypeArrayKlass::allocate(null_loader_data, type, sym, CHECK_NULL);

  // Add all classes to our internal class loader list here,
  // including classes in the bootstrap (NULL) class loader.
  // GC walks these as strong roots.
  null_loader_data->add_class(ak);

  // Call complete_create_array_klass after all instance variables have been initialized.
  complete_create_array_klass(ak, ak->super(), ModuleEntryTable::javabase_moduleEntry(), CHECK_NULL);

  return ak;
}

TypeArrayKlass* TypeArrayKlass::allocate(ClassLoaderData* loader_data, BasicType type, Symbol* name, TRAPS) {

  int size = ArrayKlass::static_size(TypeArrayKlass::header_size());

  return new (loader_data, size, THREAD) TypeArrayKlass(type, name);
}

TypeArrayKlass::TypeArrayKlass(BasicType type, Symbol* name) : ArrayKlass(name, ID) {
  set_layout_helper(array_layout_helper(type));

  set_max_length(arrayOopDesc::max_array_length(type));

  set_class_loader_data(ClassLoaderData::the_null_class_loader_data());
}

typeArrayOop TypeArrayKlass::allocate_common(int length, bool do_zero, TRAPS) {
  if (length >= 0) {
    if (length <= max_length()) {
      size_t size = typeArrayOopDesc::object_size(layout_helper(), length);
      return (typeArrayOop)Universe::heap()->array_allocate(this, (int)size, length,
                                                            do_zero, CHECK_NULL);
    } else {
      report_java_out_of_memory("Requested array size exceeds VM limit");
      THROW_OOP_0(Universe::out_of_memory_error_array_size());
    }
  } else {
    THROW_MSG_0(vmSymbols::java_lang_NegativeArraySizeException(), err_msg("%d", length));
  }
}

oop TypeArrayKlass::multi_allocate(int rank, jint* last_size, TRAPS) {
  // For typeArrays this is only called for the last dimension
  int length = *last_size;
  return allocate(length, THREAD);
}

void TypeArrayKlass::copy_array(arrayOop s, int src_pos, arrayOop d, int dst_pos, int length, TRAPS) {

  // Check destination type.
  if (!d->is_typeArray()) {
    ResourceMark rm(THREAD);
    stringStream ss;
    if (d->is_objArray()) {
      ss.print("arraycopy: type mismatch: can not copy %s[] into object array[]", type2name_tab[ArrayKlass::cast(s->klass())->element_type()]);
    } else {
      ss.print("arraycopy: destination type %s is not an array", d->klass()->external_name());
    }
    THROW_MSG(vmSymbols::java_lang_ArrayStoreException(), ss.as_string());
  }
  if (element_type() != TypeArrayKlass::cast(d->klass())->element_type()) {
    ResourceMark rm(THREAD);
    stringStream ss;
    ss.print("arraycopy: type mismatch: can not copy %s[] into %s[]",
             type2name_tab[ArrayKlass::cast(s->klass())->element_type()],
             type2name_tab[ArrayKlass::cast(d->klass())->element_type()]);
    THROW_MSG(vmSymbols::java_lang_ArrayStoreException(), ss.as_string());
  }

  // Check if all offsets and lengths are non negative.
  if (src_pos < 0 || dst_pos < 0 || length < 0) {
    // Pass specific exception reason.
    ResourceMark rm(THREAD);
    stringStream ss;
    if (src_pos < 0) {
      ss.print("arraycopy: source index %d out of bounds for %s[%d]", src_pos, type2name_tab[ArrayKlass::cast(s->klass())->element_type()], s->length());
    } else if (dst_pos < 0) {
      ss.print("arraycopy: destination index %d out of bounds for %s[%d]", dst_pos, type2name_tab[ArrayKlass::cast(d->klass())->element_type()], d->length());
    } else {
      ss.print("arraycopy: length %d is negative", length);
    }
    THROW_MSG(vmSymbols::java_lang_ArrayIndexOutOfBoundsException(), ss.as_string());
  }
  // Check if the ranges are valid
  if ((((unsigned int) length + (unsigned int) src_pos) > (unsigned int) s->length()) ||
      (((unsigned int) length + (unsigned int) dst_pos) > (unsigned int) d->length())) {
    // Pass specific exception reason.
    ResourceMark rm(THREAD);
    stringStream ss;
    if (((unsigned int) length + (unsigned int) src_pos) > (unsigned int) s->length()) {
      ss.print("arraycopy: last source index %u out of bounds for %s[%d]",
               (unsigned int) length + (unsigned int) src_pos,
               type2name_tab[ArrayKlass::cast(s->klass())->element_type()], s->length());
    } else {
      ss.print("arraycopy: last destination index %u out of bounds for %s[%d]",
               (unsigned int) length + (unsigned int) dst_pos,
               type2name_tab[ArrayKlass::cast(d->klass())->element_type()], d->length());
    }
    THROW_MSG(vmSymbols::java_lang_ArrayIndexOutOfBoundsException(), ss.as_string());
  }
  // Check zero copy
  if (length == 0)
    return;

  // This is an attempt to make the copy_array fast.
  int l2es = log2_element_size();
  size_t src_offset = arrayOopDesc::base_offset_in_bytes(element_type()) + ((size_t)src_pos << l2es);
  size_t dst_offset = arrayOopDesc::base_offset_in_bytes(element_type()) + ((size_t)dst_pos << l2es);
  ArrayAccess<ARRAYCOPY_ATOMIC>::arraycopy<void>(s, src_offset, d, dst_offset, (size_t)length << l2es);
}

// create a klass of array holding typeArrays
Klass* TypeArrayKlass::array_klass_impl(bool or_null, int n, TRAPS) {
  int dim = dimension();
    if (dim == n)
      return this;

  // lock-free read needs acquire semantics
  if (higher_dimension_acquire() == NULL) {
    if (or_null)  return NULL;

    ResourceMark rm;
    JavaThread *jt = (JavaThread *)THREAD;
    {
      MutexLocker mc(Compile_lock, THREAD);   // for vtables
      // Atomic create higher dimension and link into list
      MutexLocker mu(MultiArray_lock, THREAD);

      if (higher_dimension() == NULL) {
        Klass* oak = ObjArrayKlass::allocate_objArray_klass(class_loader_data(), dim + 1, this, CHECK_NULL);
        ObjArrayKlass* h_ak = ObjArrayKlass::cast(oak);
        h_ak->set_lower_dimension(this);
        // use 'release' to pair with lock-free load
        release_set_higher_dimension(h_ak);
      }
    }
  } else {
    CHECK_UNHANDLED_OOPS_ONLY(Thread::current()->clear_unhandled_oops());
  }
  ObjArrayKlass* h_ak = ObjArrayKlass::cast(higher_dimension());
  if (or_null) {
    return h_ak->array_klass_or_null(n);
  }
  return h_ak->array_klass(n, THREAD);
}

Klass* TypeArrayKlass::array_klass_impl(bool or_null, TRAPS) {
  return array_klass_impl(or_null, dimension() +  1, THREAD);
}

int TypeArrayKlass::oop_size(oop obj) const {
  typeArrayOop t = typeArrayOop(obj);
  return t->object_size();
}

void TypeArrayKlass::initialize(TRAPS) {
  // Nothing to do. Having this function is handy since objArrayKlasses can be
  // initialized by calling initialize on their bottom_klass, see ObjArrayKlass::initialize
}

const char* TypeArrayKlass::external_name(BasicType type) {
  switch (type) {
    case T_BOOLEAN: return "[Z";
    case T_CHAR:    return "[C";
    case T_FLOAT:   return "[F";
    case T_DOUBLE:  return "[D";
    case T_BYTE:    return "[B";
    case T_SHORT:   return "[S";
    case T_INT:     return "[I";
    case T_LONG:    return "[J";
    default: ShouldNotReachHere();
  }
  return NULL;
}

// Printing

void TypeArrayKlass::print_on(outputStream* st) const { }

void TypeArrayKlass::print_value_on(outputStream* st) const {
  st->print("{type array ");
  BasicType bt = element_type();
  if (bt == T_BOOLEAN) {
    st->print("bool");
  } else {
    st->print("%s", type2name_tab[bt]);
  }
  st->print("}");
}

const char* TypeArrayKlass::internal_name() const {
  return Klass::external_name();
}

// A TypeArrayKlass is an array of a primitive type, its defining module is java.base
ModuleEntry* TypeArrayKlass::module() const {
  return ModuleEntryTable::javabase_moduleEntry();
}

PackageEntry* TypeArrayKlass::package() const {
  return NULL;
}

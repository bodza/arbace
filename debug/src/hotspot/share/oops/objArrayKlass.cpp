#include "precompiled.hpp"

#include "classfile/moduleEntry.hpp"
#include "classfile/packageEntry.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "memory/iterator.inline.hpp"
#include "memory/metadataFactory.hpp"
#include "memory/metaspaceClosure.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/arrayKlass.inline.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/klass.inline.hpp"
#include "oops/objArrayKlass.inline.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "utilities/macros.hpp"

ObjArrayKlass* ObjArrayKlass::allocate(ClassLoaderData* loader_data, int n, Klass* k, Symbol* name, TRAPS) {
  int size = ArrayKlass::static_size(ObjArrayKlass::header_size());

  return new (loader_data, size, THREAD) ObjArrayKlass(n, k, name);
}

Klass* ObjArrayKlass::allocate_objArray_klass(ClassLoaderData* loader_data, int n, Klass* element_klass, TRAPS) {
  // Eagerly allocate the direct array supertype.
  Klass* super_klass = NULL;
  if (!Universe::is_bootstrapping() || SystemDictionary::Object_klass_loaded()) {
    Klass* element_super = element_klass->super();
    if (element_super != NULL) {
      // The element type has a direct super.  E.g., String[] has direct super of Object[].
      super_klass = element_super->array_klass_or_null();
      bool supers_exist = super_klass != NULL;
      // Also, see if the element has secondary supertypes.
      // We need an array type for each.
      Array<Klass*>* element_supers = element_klass->secondary_supers();
      for ( int i = element_supers->length()-1; i >= 0; i-- ) {
        Klass* elem_super = element_supers->at(i);
        if (elem_super->array_klass_or_null() == NULL) {
          supers_exist = false;
          break;
        }
      }
      if (!supers_exist) {
        // Oops.  Not allocated yet.  Back out, allocate it, and retry.
        Klass* ek = NULL;
        {
          MutexUnlocker mu(MultiArray_lock);
          MutexUnlocker mc(Compile_lock);   // for vtables
          super_klass = element_super->array_klass(CHECK_0);
          for ( int i = element_supers->length()-1; i >= 0; i-- ) {
            Klass* elem_super = element_supers->at(i);
            elem_super->array_klass(CHECK_0);
          }
          // Now retry from the beginning
          ek = element_klass->array_klass(n, CHECK_0);
        }
        return ek;
      }
    } else {
      // The element type is already Object.  Object[] has direct super of Object.
      super_klass = SystemDictionary::Object_klass();
    }
  }

  // Create type name for klass.
  Symbol* name = NULL;
  if (!element_klass->is_instance_klass() || (name = InstanceKlass::cast(element_klass)->array_name()) == NULL) {
    ResourceMark rm(THREAD);
    char *name_str = element_klass->name()->as_C_string();
    int len = element_klass->name()->utf8_length();
    char *new_str = NEW_RESOURCE_ARRAY(char, len + 4);
    int idx = 0;
    new_str[idx++] = '[';
    if (element_klass->is_instance_klass()) { // it could be an array or simple type
      new_str[idx++] = 'L';
    }
    memcpy(&new_str[idx], name_str, len * sizeof(char));
    idx += len;
    if (element_klass->is_instance_klass()) {
      new_str[idx++] = ';';
    }
    new_str[idx++] = '\0';
    name = SymbolTable::new_permanent_symbol(new_str, CHECK_0);
    if (element_klass->is_instance_klass()) {
      InstanceKlass* ik = InstanceKlass::cast(element_klass);
      ik->set_array_name(name);
    }
  }

  // Initialize instance variables
  ObjArrayKlass* oak = ObjArrayKlass::allocate(loader_data, n, element_klass, name, CHECK_0);

  // Add all classes to our internal class loader list here,
  // including classes in the bootstrap (NULL) class loader.
  // GC walks these as strong roots.
  loader_data->add_class(oak);

  ModuleEntry* module = oak->module();

  // Call complete_create_array_klass after all instance variables has been initialized.
  ArrayKlass::complete_create_array_klass(oak, super_klass, module, CHECK_0);

  return oak;
}

ObjArrayKlass::ObjArrayKlass(int n, Klass* element_klass, Symbol* name) : ArrayKlass(name, ID) {
  this->set_dimension(n);
  this->set_element_klass(element_klass);
  // decrement refcount because object arrays are not explicitly freed.  The
  // InstanceKlass array_name() keeps the name counted while the klass is
  // loaded.
  name->decrement_refcount();

  Klass* bk;
  if (element_klass->is_objArray_klass()) {
    bk = ObjArrayKlass::cast(element_klass)->bottom_klass();
  } else {
    bk = element_klass;
  }
  this->set_bottom_klass(bk);
  this->set_class_loader_data(bk->class_loader_data());

  this->set_layout_helper(array_layout_helper(T_OBJECT));
}

int ObjArrayKlass::oop_size(oop obj) const {
  return objArrayOop(obj)->object_size();
}

objArrayOop ObjArrayKlass::allocate(int length, TRAPS) {
  if (length >= 0) {
    if (length <= arrayOopDesc::max_array_length(T_OBJECT)) {
      int size = objArrayOopDesc::object_size(length);
      return (objArrayOop)Universe::heap()->array_allocate(this, size, length,
                                                           /* do_zero */ true, THREAD);
    } else {
      report_java_out_of_memory("Requested array size exceeds VM limit");
      THROW_OOP_0(Universe::out_of_memory_error_array_size());
    }
  } else {
    THROW_MSG_0(vmSymbols::java_lang_NegativeArraySizeException(), err_msg("%d", length));
  }
}

static int multi_alloc_counter = 0;

oop ObjArrayKlass::multi_allocate(int rank, jint* sizes, TRAPS) {
  int length = *sizes;
  // Call to lower_dimension uses this pointer, so most be called before a
  // possible GC
  Klass* ld_klass = lower_dimension();
  // If length < 0 allocate will throw an exception.
  objArrayOop array = allocate(length, CHECK_NULL);
  objArrayHandle h_array (THREAD, array);
  if (rank > 1) {
    if (length != 0) {
      for (int index = 0; index < length; index++) {
        ArrayKlass* ak = ArrayKlass::cast(ld_klass);
        oop sub_array = ak->multi_allocate(rank-1, &sizes[1], CHECK_NULL);
        h_array->obj_at_put(index, sub_array);
      }
    } else {
      // Since this array dimension has zero length, nothing will be
      // allocated, however the lower dimension values must be checked
      // for illegal values.
      for (int i = 0; i < rank - 1; ++i) {
        sizes += 1;
        if (*sizes < 0) {
          THROW_MSG_0(vmSymbols::java_lang_NegativeArraySizeException(), err_msg("%d", *sizes));
        }
      }
    }
  }
  return h_array();
}

// Either oop or narrowOop depending on UseCompressedOops.
void ObjArrayKlass::do_copy(arrayOop s, size_t src_offset, arrayOop d, size_t dst_offset, int length, TRAPS) {
  if (oopDesc::equals(s, d)) {
    ArrayAccess<>::oop_arraycopy(s, src_offset, d, dst_offset, length);
  } else {
    // We have to make sure all elements conform to the destination array
    Klass* bound = ObjArrayKlass::cast(d->klass())->element_klass();
    Klass* stype = ObjArrayKlass::cast(s->klass())->element_klass();
    if (stype == bound || stype->is_subtype_of(bound)) {
      // elements are guaranteed to be subtypes, so no check necessary
      ArrayAccess<ARRAYCOPY_DISJOINT>::oop_arraycopy(s, src_offset, d, dst_offset, length);
    } else {
      // slow case: need individual subtype checks
      // note: don't use obj_at_put below because it includes a redundant store check
      if (!ArrayAccess<ARRAYCOPY_DISJOINT | ARRAYCOPY_CHECKCAST>::oop_arraycopy(s, src_offset, d, dst_offset, length)) {
        ResourceMark rm(THREAD);
        stringStream ss;
        if (!bound->is_subtype_of(stype)) {
          ss.print("arraycopy: type mismatch: can not copy %s[] into %s[]", stype->external_name(), bound->external_name());
        } else {
          // oop_arraycopy should return the index in the source array that
          // contains the problematic oop.
          ss.print("arraycopy: element type mismatch: can not cast one of the elements of %s[] to the type of the destination array, %s", stype->external_name(), bound->external_name());
        }
        THROW_MSG(vmSymbols::java_lang_ArrayStoreException(), ss.as_string());
      }
    }
  }
}

void ObjArrayKlass::copy_array(arrayOop s, int src_pos, arrayOop d, int dst_pos, int length, TRAPS) {
  if (!d->is_objArray()) {
    ResourceMark rm(THREAD);
    stringStream ss;
    if (d->is_typeArray()) {
      ss.print("arraycopy: type mismatch: can not copy object array[] into %s[]", type2name_tab[ArrayKlass::cast(d->klass())->element_type()]);
    } else {
      ss.print("arraycopy: destination type %s is not an array", d->klass()->external_name());
    }
    THROW_MSG(vmSymbols::java_lang_ArrayStoreException(), ss.as_string());
  }

  // Check is all offsets and lengths are non negative
  if (src_pos < 0 || dst_pos < 0 || length < 0) {
    // Pass specific exception reason.
    ResourceMark rm(THREAD);
    stringStream ss;
    if (src_pos < 0) {
      ss.print("arraycopy: source index %d out of bounds for object array[%d]", src_pos, s->length());
    } else if (dst_pos < 0) {
      ss.print("arraycopy: destination index %d out of bounds for object array[%d]", dst_pos, d->length());
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
      ss.print("arraycopy: last source index %u out of bounds for object array[%d]", (unsigned int) length + (unsigned int) src_pos, s->length());
    } else {
      ss.print("arraycopy: last destination index %u out of bounds for object array[%d]", (unsigned int) length + (unsigned int) dst_pos, d->length());
    }
    THROW_MSG(vmSymbols::java_lang_ArrayIndexOutOfBoundsException(), ss.as_string());
  }

  // Special case. Boundary cases must be checked first
  // This allows the following call: copy_array(s, s.length(), d.length(), 0).
  // This is correct, since the position is supposed to be an 'in between point', i.e., s.length(),
  // points to the right of the last element.
  if (length==0) {
    return;
  }
  if (UseCompressedOops) {
    size_t src_offset = (size_t) objArrayOopDesc::obj_at_offset<narrowOop>(src_pos);
    size_t dst_offset = (size_t) objArrayOopDesc::obj_at_offset<narrowOop>(dst_pos);
    do_copy(s, src_offset, d, dst_offset, length, CHECK);
  } else {
    size_t src_offset = (size_t) objArrayOopDesc::obj_at_offset<oop>(src_pos);
    size_t dst_offset = (size_t) objArrayOopDesc::obj_at_offset<oop>(dst_pos);
    do_copy(s, src_offset, d, dst_offset, length, CHECK);
  }
}

Klass* ObjArrayKlass::array_klass_impl(bool or_null, int n, TRAPS) {
  int dim = dimension();
  if (dim == n) return this;

  // lock-free read needs acquire semantics
  if (higher_dimension_acquire() == NULL) {
    if (or_null)  return NULL;

    ResourceMark rm;
    JavaThread *jt = (JavaThread *)THREAD;
    {
      MutexLocker mc(Compile_lock, THREAD);   // for vtables
      // Ensure atomic creation of higher dimensions
      MutexLocker mu(MultiArray_lock, THREAD);

      // Check if another thread beat us
      if (higher_dimension() == NULL) {
        // Create multi-dim klass object and link them together
        Klass* k = ObjArrayKlass::allocate_objArray_klass(class_loader_data(), dim + 1, this, CHECK_NULL);
        ObjArrayKlass* ak = ObjArrayKlass::cast(k);
        ak->set_lower_dimension(this);
        // use 'release' to pair with lock-free load
        release_set_higher_dimension(ak);
      }
    }
  } else {
    CHECK_UNHANDLED_OOPS_ONLY(Thread::current()->clear_unhandled_oops());
  }

  ObjArrayKlass *ak = ObjArrayKlass::cast(higher_dimension());
  if (or_null) {
    return ak->array_klass_or_null(n);
  }
  return ak->array_klass(n, THREAD);
}

Klass* ObjArrayKlass::array_klass_impl(bool or_null, TRAPS) {
  return array_klass_impl(or_null, dimension() +  1, THREAD);
}

bool ObjArrayKlass::can_be_primary_super_slow() const {
  if (!bottom_klass()->can_be_primary_super())
    // array of interfaces
    return false;
  else
    return Klass::can_be_primary_super_slow();
}

GrowableArray<Klass*>* ObjArrayKlass::compute_secondary_supers(int num_extra_slots, Array<Klass*>* transitive_interfaces) {
  // interfaces = { cloneable_klass, serializable_klass, elemSuper[], ... };
  Array<Klass*>* elem_supers = element_klass()->secondary_supers();
  int num_elem_supers = elem_supers == NULL ? 0 : elem_supers->length();
  int num_secondaries = num_extra_slots + 2 + num_elem_supers;
  if (num_secondaries == 2) {
    // Must share this for correct bootstrapping!
    set_secondary_supers(Universe::the_array_interfaces_array());
    return NULL;
  } else {
    GrowableArray<Klass*>* secondaries = new GrowableArray<Klass*>(num_elem_supers+2);
    secondaries->push(SystemDictionary::Cloneable_klass());
    secondaries->push(SystemDictionary::Serializable_klass());
    for (int i = 0; i < num_elem_supers; i++) {
      Klass* elem_super = (Klass*) elem_supers->at(i);
      Klass* array_super = elem_super->array_klass_or_null();
      secondaries->push(array_super);
    }
    return secondaries;
  }
}

bool ObjArrayKlass::compute_is_subtype_of(Klass* k) {
  if (!k->is_objArray_klass())
    return ArrayKlass::compute_is_subtype_of(k);

  ObjArrayKlass* oak = ObjArrayKlass::cast(k);
  return element_klass()->is_subtype_of(oak->element_klass());
}

void ObjArrayKlass::initialize(TRAPS) {
  bottom_klass()->initialize(THREAD);  // dispatches to either InstanceKlass or TypeArrayKlass
}

void ObjArrayKlass::metaspace_pointers_do(MetaspaceClosure* it) {
  ArrayKlass::metaspace_pointers_do(it);
  it->push(&_element_klass);
  it->push(&_bottom_klass);
}

// JVM support

jint ObjArrayKlass::compute_modifier_flags(TRAPS) const {
  // The modifier for an objectArray is the same as its element
  if (element_klass() == NULL) {
    return JVM_ACC_ABSTRACT | JVM_ACC_FINAL | JVM_ACC_PUBLIC;
  }
  // Return the flags of the bottom element type.
  jint element_flags = bottom_klass()->compute_modifier_flags(CHECK_0);

  return (element_flags & (JVM_ACC_PUBLIC | JVM_ACC_PRIVATE | JVM_ACC_PROTECTED))
                        | (JVM_ACC_ABSTRACT | JVM_ACC_FINAL);
}

ModuleEntry* ObjArrayKlass::module() const {
  // The array is defined in the module of its bottom class
  return bottom_klass()->module();
}

PackageEntry* ObjArrayKlass::package() const {
  return bottom_klass()->package();
}

// Printing

void ObjArrayKlass::print_on(outputStream* st) const { }

void ObjArrayKlass::print_value_on(outputStream* st) const {
  element_klass()->print_value_on(st);
  st->print("[]");
}

void ObjArrayKlass::oop_print_value_on(oop obj, outputStream* st) {
  st->print("a ");
  element_klass()->print_value_on(st);
  int len = objArrayOop(obj)->length();
  st->print("[%d] ", len);
  obj->print_address_on(st);
}

const char* ObjArrayKlass::internal_name() const {
  return external_name();
}

// Verification

void ObjArrayKlass::verify_on(outputStream* st) {
  ArrayKlass::verify_on(st);
  guarantee(element_klass()->is_klass(), "should be klass");
  guarantee(bottom_klass()->is_klass(), "should be klass");
  Klass* bk = bottom_klass();
  guarantee(bk->is_instance_klass() || bk->is_typeArray_klass(),  "invalid bottom klass");
}

void ObjArrayKlass::oop_verify_on(oop obj, outputStream* st) {
  ArrayKlass::oop_verify_on(obj, st);
  guarantee(obj->is_objArray(), "must be objArray");
  objArrayOop oa = objArrayOop(obj);
  for (int index = 0; index < oa->length(); index++) {
    guarantee(oopDesc::is_oop_or_null(oa->obj_at(index)), "should be oop");
  }
}

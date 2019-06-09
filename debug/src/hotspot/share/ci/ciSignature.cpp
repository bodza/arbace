#include "precompiled.hpp"

#include "ci/ciSignature.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/signature.hpp"

// ciSignature
//
// This class represents the signature of a method.

ciSignature::ciSignature(ciKlass* accessing_klass, const constantPoolHandle& cpool, ciSymbol* symbol) {
  EXCEPTION_CONTEXT;
  _accessing_klass = accessing_klass;
  _symbol = symbol;

  ciEnv* env = ciEnv::current();
  Arena* arena = env->arena();
  _types = new (arena) GrowableArray<ciType*>(arena, 8, 0, NULL);

  int size = 0;
  int count = 0;
  ResourceMark rm(THREAD);
  Symbol* sh = symbol->get_symbol();
  SignatureStream ss(sh);
  for ( ; ; ss.next()) {
    // Process one element of the signature
    ciType* type;
    if (!ss.is_object()) {
      type = ciType::make(ss.type());
    } else {
      Symbol* name = ss.as_symbol(THREAD);
      if (HAS_PENDING_EXCEPTION) {
        type = ss.is_array() ? (ciType*)ciEnv::unloaded_ciobjarrayklass()
          : (ciType*)ciEnv::unloaded_ciinstance_klass();
        env->record_out_of_memory_failure();
        CLEAR_PENDING_EXCEPTION;
      } else {
        ciSymbol* klass_name = env->get_symbol(name);
        type = env->get_klass_by_name_impl(_accessing_klass, cpool, klass_name, false);
      }
    }
    _types->append(type);
    if (ss.at_return_type()) {
      // Done processing the return type; do not add it into the count.
      break;
    }
    size += type->size();
    count++;
  }
  _size = size;
  _count = count;
}

// What is the return type of this signature?
ciType* ciSignature::return_type() const {
  return _types->at(_count);
}

// What is the type of the index'th element of this signature?
ciType* ciSignature::type_at(int index) const {
  // The first _klasses element holds the return klass.
  return _types->at(index);
}

// Compare this signature to another one.  Signatures with different
// accessing classes but with signature-types resolved to the same
// types are defined to be equal.
bool ciSignature::equals(ciSignature* that) {
  // Compare signature
  if (!this->as_symbol()->equals(that->as_symbol()))  return false;
  // Compare all types of the arguments
  for (int i = 0; i < _count; i++) {
    if (this->type_at(i) != that->type_at(i))         return false;
  }
  // Compare the return type
  if (this->return_type() != that->return_type())     return false;
  return true;
}

void ciSignature::print_signature() {
  _symbol->print_symbol();
}

void ciSignature::print() {
  tty->print("<ciSignature symbol=");
  print_signature();
 tty->print(" accessing_klass=");
  _accessing_klass->print();
  tty->print(" address=" INTPTR_FORMAT ">", p2i((address)this));
}

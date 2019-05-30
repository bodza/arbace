#ifndef SHARE_VM_RUNTIME_FIELDTYPE_HPP
#define SHARE_VM_RUNTIME_FIELDTYPE_HPP

#include "memory/allocation.hpp"
#include "oops/symbol.hpp"

// Note: FieldType should be based on the SignatureIterator (or vice versa).
//       In any case, this structure should be re-thought at some point.

// A FieldType is used to determine the type of a field from a signature string.

// Information returned by get_array_info, which is scoped to decrement
// reference count if a Symbol is created in the case of T_OBJECT
class FieldArrayInfo : public StackObj {
  friend class FieldType;  // field type can set these fields.
  int       _dimension;
  Symbol*   _object_key;
 public:
  int       dimension()    { return _dimension; }
  Symbol*   object_key()   { return _object_key; }
  // basic constructor
  FieldArrayInfo() : _dimension(0), _object_key(NULL) {}
  // destructor decrements object key's refcount if created
  ~FieldArrayInfo() { if (_object_key != NULL) _object_key->decrement_refcount(); }
};

class FieldType: public AllStatic {
 private:
  static bool is_valid_array_signature(Symbol* signature);
 public:

  // Return basic type
  static BasicType basic_type(Symbol* signature);

  // Testing
  static bool is_array(Symbol* signature) { return signature->utf8_length() > 1 && signature->byte_at(0) == '[' && is_valid_array_signature(signature); }

  static bool is_obj(Symbol* signature) {
     int sig_length = signature->utf8_length();
     // Must start with 'L' and end with ';'
     return (sig_length >= 2 &&
             (signature->byte_at(0) == 'L') &&
             (signature->byte_at(sig_length - 1) == ';'));
  }

  // Parse field and extract array information. Works for T_ARRAY only.
  static BasicType get_array_info(Symbol* signature, FieldArrayInfo& ai, TRAPS);
};

#endif

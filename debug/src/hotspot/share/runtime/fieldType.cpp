#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayKlass.hpp"
#include "runtime/fieldType.hpp"
#include "runtime/signature.hpp"

BasicType FieldType::basic_type(Symbol* signature) {
  return char2type(signature->byte_at(0));
}

// Check if it is a valid array signature
bool FieldType::is_valid_array_signature(Symbol* sig) {
  // The first character is already checked
  int i = 1;
  int len = sig->utf8_length();
  // First skip all '['s
  while (i < len - 1 && sig->byte_at(i) == '[') i++;

  // Check type
  switch (sig->byte_at(i)) {
    case 'B': // T_BYTE
    case 'C': // T_CHAR
    case 'D': // T_DOUBLE
    case 'F': // T_FLOAT
    case 'I': // T_INT
    case 'J': // T_LONG
    case 'S': // T_SHORT
    case 'Z': // T_BOOLEAN
      // If it is an array, the type is the last character
      return (i + 1 == len);
    case 'L':
      // If it is an object, the last character must be a ';'
      return sig->byte_at(len - 1) == ';';
  }

  return false;
}

BasicType FieldType::get_array_info(Symbol* signature, FieldArrayInfo& fd, TRAPS) {
  int index = 1;
  int dim   = 1;
  while (signature->byte_at(index) == '[') {
    index++;
    dim++;
  }
  ResourceMark rm;
  char* element = signature->as_C_string() + index;
  BasicType element_type = char2type(element[0]);
  if (element_type == T_OBJECT) {
    int len = (int)strlen(element);
    element[len - 1] = '\0';        // chop off semicolon
    fd._object_key = SymbolTable::new_symbol(element + 1, CHECK_(T_BYTE));
  }
  // Pass dimension back to caller
  fd._dimension = dim;
  return element_type;
}

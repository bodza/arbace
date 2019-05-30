#ifndef HOTSPOT_SHARE_OOPS_REFLECTIONACCESSORIMPLKLASSHELPER_HPP
#define HOTSPOT_SHARE_OOPS_REFLECTIONACCESSORIMPLKLASSHELPER_HPP

#include "memory/allocation.hpp"

class InstanceKlass;

// Helper for classes derived from jdk/internal/reflect/{Method|Constructor}AccessorImpl:
// offers convenience functions to extract the names of target class/method/signature
// from the constant pool of these classes.
class ReflectionAccessorImplKlassHelper: public AllStatic {

  // Returns true if k is of type jdk/internal/reflect/GeneratedMethodAccessorXXX.
  static bool is_generated_method_accessor(const InstanceKlass* k);

  // Returns true if k is of type jdk/internal/reflect/GeneratedConstructorAccessorXXX.
  static bool is_generated_constructor_accessor(const InstanceKlass* k);

  // Returns true if k is of type jdk/internal/reflect/GeneratedSerializationConstructorAccessorXXX.
  static bool is_generated_method_serialization_constructor_accessor(const InstanceKlass* k);

  // Assuming k is of type jdk/internal/reflect/Generated{SerializationConstructor|Constructor|Method}AccessorXXX,
  // the name of the target class as resource-area allocated string.
  static const char* get_target_class_name(const InstanceKlass* k);

  // Assuming k is of type jdk/internal/reflect/Generated{SerializationConstructor|Constructor|Method}AccessorXXX,
  // the name of the target method as resource-area allocated string.
  static const char* get_target_method_name(const InstanceKlass* k);

  // Assuming k is of type jdk/internal/reflect/Generated{SerializationConstructor|Constructor|Method}AccessorXXX,
  // the signature of the target method as resource-area allocated string.
  static const char* get_target_method_signature(const InstanceKlass* k);

public:

  // Returns true if k is of type jdk/internal/reflect/Generated{SerializationConstructor|Constructor|Method}AccessorXXX
  // and it is safe to call print_invocation_target(k)
  static bool is_generated_accessor(const Klass* k);

  // Assuming k is of type jdk/internal/reflect/Generated{SerializationConstructor|Constructor|Method}AccessorXXX,
  // print out target class, method, signature in one line.
  static void print_invocation_target(outputStream* out, Klass* k);
};

#endif

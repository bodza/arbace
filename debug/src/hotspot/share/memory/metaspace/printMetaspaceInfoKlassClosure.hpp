#ifndef SHARE_MEMORY_METASPACE_PRINTMETASPACEINFOKLASSCLOSURE_HPP_
#define SHARE_MEMORY_METASPACE_PRINTMETASPACEINFOKLASSCLOSURE_HPP_

#include "memory/iterator.hpp"
#include "utilities/globalDefinitions.hpp"

class outputStream;
class InstanceKlass;

namespace metaspace {

// Helper class for MetaspaceUtils::print_report()
class PrintMetaspaceInfoKlassClosure : public KlassClosure {
private:
  outputStream* const _out;
  const bool          _do_print;

  bool print_reflection_invocation_target(outputStream* out, InstanceKlass* magic_accessor_impl_class);

public:

  uintx _num_classes;
  uintx _num_instance_classes;
  uintx _num_array_classes;

  PrintMetaspaceInfoKlassClosure(outputStream* out, bool do_print);
  void do_klass(Klass* k);

}; // end: PrintKlassInfoClosure
}

#endif

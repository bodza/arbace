#include "precompiled.hpp"

#include "memory/metaspace/printMetaspaceInfoKlassClosure.hpp"
#include "memory/resourceArea.hpp"
#include "oops/reflectionAccessorImplKlassHelper.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ostream.hpp"

namespace metaspace {
PrintMetaspaceInfoKlassClosure::PrintMetaspaceInfoKlassClosure(outputStream* out, bool do_print)
: _out(out), _do_print(do_print)
, _num_classes(0), _num_instance_classes(0), _num_array_classes(0) {
}

void PrintMetaspaceInfoKlassClosure::do_klass(Klass* k) {
  _num_classes ++;
  if (k->is_instance_klass()) {
    _num_instance_classes ++;
  } else if (k->is_array_klass()) {
    _num_array_classes ++;
  }
  if (_do_print) {
    _out->cr_indent();
    _out->print(UINTX_FORMAT_W(4) ": ", _num_classes);
    ResourceMark rm;
    _out->print("%s", k->external_name());

    // Special treatment for generated core reflection accessor classes: print invocation target.
    if (ReflectionAccessorImplKlassHelper::is_generated_accessor(k)) {
      _out->print(" (invokes: ");
      ReflectionAccessorImplKlassHelper::print_invocation_target(_out, k);
      _out->print(")");
    }
  }
}
}

#include "precompiled.hpp"

#include "classfile/classLoaderData.inline.hpp"
#include "classfile/javaClasses.hpp"
#include "memory/metaspace/printCLDMetaspaceInfoClosure.hpp"
#include "memory/metaspace/printMetaspaceInfoKlassClosure.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/safepoint.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ostream.hpp"

namespace metaspace {
PrintCLDMetaspaceInfoClosure::PrintCLDMetaspaceInfoClosure(outputStream* out, size_t scale, bool do_print, bool do_print_classes, bool break_down_by_chunktype)
: _out(out), _scale(scale), _do_print(do_print), _do_print_classes(do_print_classes)
, _break_down_by_chunktype(break_down_by_chunktype)
, _num_loaders(0), _num_loaders_unloading(0), _num_loaders_without_metaspace(0)
{
  memset(_num_loaders_by_spacetype, 0, sizeof(_num_loaders_by_spacetype));
}

static const char* classes_plural(uintx num) {
  return num == 1 ? "" : "es";
}

void PrintCLDMetaspaceInfoClosure::do_cld(ClassLoaderData* cld) {
  if (cld->is_unloading()) {
    _num_loaders_unloading ++;
    return;
  }

  ClassLoaderMetaspace* msp = cld->metaspace_or_null();
  if (msp == NULL) {
    _num_loaders_without_metaspace ++;
    return;
  }

  // Collect statistics for this class loader metaspace
  ClassLoaderMetaspaceStatistics this_cld_stat;
  msp->add_to_statistics(&this_cld_stat);

  // And add it to the running totals
  _stats_total.add(this_cld_stat);
  _num_loaders ++;
  _stats_by_spacetype[msp->space_type()].add(this_cld_stat);
  _num_loaders_by_spacetype[msp->space_type()] ++;

  // Optionally, print.
  if (_do_print) {
    _out->print(UINTX_FORMAT_W(4) ": ", _num_loaders);

    // Print "CLD for [<loader name>,] instance of <loader class name>"
    // or    "CLD for <anonymous class>, loaded by [<loader name>,] instance of <loader class name>"

    ResourceMark rm;
    const char* name = NULL;
    const char* class_name = NULL;

    // Note: this should also work if unloading:
    Klass* k = cld->class_loader_klass();
    if (k != NULL) {
      class_name = k->external_name();
      Symbol* s = cld->name();
      if (s != NULL) {
        name = s->as_C_string();
      }
    } else {
      name = "<bootstrap>";
    }

    // Print
    _out->print("CLD " PTR_FORMAT, p2i(cld));
    if (cld->is_unloading()) {
      _out->print(" (unloading)");
    }
    _out->print(":");
    if (cld->is_anonymous()) {
      _out->print(" <anonymous class>, loaded by");
    }
    if (name != NULL) {
      _out->print(" \"%s\"", name);
    }
    if (class_name != NULL) {
      _out->print(" instance of %s", class_name);
    }

    if (_do_print_classes) {
      streamIndentor sti(_out, 6);
      _out->cr_indent();
      _out->print("Loaded classes: ");
      PrintMetaspaceInfoKlassClosure pkic(_out, true);
      cld->classes_do(&pkic);
      _out->cr_indent();
      _out->print("-total-: ");
      _out->print(UINTX_FORMAT " class%s", pkic._num_classes, classes_plural(pkic._num_classes));
      if (pkic._num_instance_classes > 0 || pkic._num_array_classes > 0) {
        _out->print(" (");
        if (pkic._num_instance_classes > 0) {
          _out->print(UINTX_FORMAT " instance class%s", pkic._num_instance_classes,
              classes_plural(pkic._num_instance_classes));
        }
        if (pkic._num_array_classes > 0) {
          if (pkic._num_instance_classes > 0) {
            _out->print(", ");
          }
          _out->print(UINTX_FORMAT " array class%s", pkic._num_array_classes,
              classes_plural(pkic._num_array_classes));
        }
        _out->print(").");
      }
    }

    _out->cr();

    _out->cr();

    // Print statistics
    this_cld_stat.print_on(_out, _scale, _break_down_by_chunktype);
    _out->cr();
  }
}
}

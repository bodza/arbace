#ifndef SHARE_MEMORY_METASPACE_PRINTCLDMETASPACEINFOCLOSURE_HPP
#define SHARE_MEMORY_METASPACE_PRINTCLDMETASPACEINFOCLOSURE_HPP

#include "memory/iterator.hpp"
#include "memory/metaspace.hpp"
#include "memory/metaspace/metaspaceStatistics.hpp"
#include "utilities/globalDefinitions.hpp"

class outputStream;

namespace metaspace {

class PrintCLDMetaspaceInfoClosure : public CLDClosure {
private:
  outputStream* const _out;
  const size_t        _scale;
  const bool          _do_print;
  const bool          _do_print_classes;
  const bool          _break_down_by_chunktype;

public:

  uintx                           _num_loaders;
  uintx                           _num_loaders_without_metaspace;
  uintx                           _num_loaders_unloading;
  ClassLoaderMetaspaceStatistics  _stats_total;

  uintx                           _num_loaders_by_spacetype [Metaspace::MetaspaceTypeCount];
  ClassLoaderMetaspaceStatistics  _stats_by_spacetype [Metaspace::MetaspaceTypeCount];

  PrintCLDMetaspaceInfoClosure(outputStream* out, size_t scale, bool do_print,
      bool do_print_classes, bool break_down_by_chunktype);
  void do_cld(ClassLoaderData* cld);
};
}

#endif

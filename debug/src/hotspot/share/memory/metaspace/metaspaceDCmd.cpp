#include "precompiled.hpp"
#include "memory/metaspace.hpp"
#include "memory/metaspace/metaspaceDCmd.hpp"
#include "memory/resourceArea.hpp"
#include "services/diagnosticCommand.hpp"
#include "services/nmtCommon.hpp"

namespace metaspace {

MetaspaceDCmd::MetaspaceDCmd(outputStream* output, bool heap)
  : DCmdWithParser(output, heap)
  , _basic("basic", "Prints a basic summary (does not need a safepoint).", "BOOLEAN", false, "false")
  , _show_loaders("show-loaders", "Shows usage by class loader.", "BOOLEAN", false, "false")
  , _show_classes("show-classes", "If show-loaders is set, shows loaded classes for each loader.", "BOOLEAN", false, "false")
  , _by_chunktype("by-chunktype", "Break down numbers by chunk type.", "BOOLEAN", false, "false")
  , _by_spacetype("by-spacetype", "Break down numbers by loader type.", "BOOLEAN", false, "false")
  , _show_vslist("vslist", "Shows details about the underlying virtual space.", "BOOLEAN", false, "false")
  , _show_vsmap("vsmap", "Shows chunk composition of the underlying virtual spaces", "BOOLEAN", false, "false")
  , _scale("scale", "Memory usage in which to scale. Valid values are: 1, KB, MB or GB (fixed scale) "
           "or \"dynamic\" for a dynamically choosen scale.",
     "STRING", false, "dynamic")
{
  _dcmdparser.add_dcmd_option(&_basic);
  _dcmdparser.add_dcmd_option(&_show_loaders);
  _dcmdparser.add_dcmd_option(&_show_classes);
  _dcmdparser.add_dcmd_option(&_by_chunktype);
  _dcmdparser.add_dcmd_option(&_by_spacetype);
  _dcmdparser.add_dcmd_option(&_show_vslist);
  _dcmdparser.add_dcmd_option(&_show_vsmap);
  _dcmdparser.add_dcmd_option(&_scale);
}

int MetaspaceDCmd::num_arguments() {
  ResourceMark rm;
  MetaspaceDCmd* dcmd = new MetaspaceDCmd(NULL, false);
  if (dcmd != NULL) {
    DCmdMark mark(dcmd);
    return dcmd->_dcmdparser.num_arguments();
  } else {
    return 0;
  }
}

void MetaspaceDCmd::execute(DCmdSource source, TRAPS) {
  // Parse scale value.
  const char* scale_value = _scale.value();
  size_t scale = 0;
  if (scale_value != NULL) {
    if (strcasecmp("dynamic", scale_value) == 0) {
      scale = 0;
    } else {
      scale = 0;
      if (scale == 0) {
        output()->print_cr("Invalid scale: \"%s\". Will use dynamic scaling.", scale_value);
      }
    }
  }
  if (_basic.value() == true) {
    if (_show_loaders.value() || _by_chunktype.value() || _by_spacetype.value() || _show_vslist.value() || _show_vsmap.value()) {
      // Basic mode. Just print essentials. Does not need to be at a safepoint.
      output()->print_cr("In basic mode, additional arguments are ignored.");
    }
    MetaspaceUtils::print_basic_report(output(), scale);
  } else {
    // Full mode. Requires safepoint.
    int flags = 0;
    if (_show_loaders.value())         flags |= MetaspaceUtils::rf_show_loaders;
    if (_show_classes.value())         flags |= MetaspaceUtils::rf_show_classes;
    if (_by_chunktype.value())         flags |= MetaspaceUtils::rf_break_down_by_chunktype;
    if (_by_spacetype.value())         flags |= MetaspaceUtils::rf_break_down_by_spacetype;
    if (_show_vslist.value())          flags |= MetaspaceUtils::rf_show_vslist;
    if (_show_vsmap.value())           flags |= MetaspaceUtils::rf_show_vsmap;
    VM_PrintMetadata op(output(), scale, flags);
    VMThread::execute(&op);
  }
}
}

#include "precompiled.hpp"
#include "gc/shared/gcConfig.hpp"
#include "runtime/globals_extension.hpp"
#include "runtime/java.hpp"
#include "runtime/os.hpp"
#include "utilities/macros.hpp"
#include "gc/epsilon/epsilonArguments.hpp"
#include "gc/g1/g1Arguments.hpp"

struct SupportedGC {
  bool&               _flag;
  CollectedHeap::Name _name;
  GCArguments&        _arguments;
  const char*         _hs_err_name;

  SupportedGC(bool& flag, CollectedHeap::Name name, GCArguments& arguments, const char* hs_err_name) :
      _flag(flag), _name(name), _arguments(arguments), _hs_err_name(hs_err_name) {}
};

 static EpsilonArguments  epsilonArguments;
      static G1Arguments       g1Arguments;

// Table of supported GCs, for translating between command
// line flag, CollectedHeap::Name and GCArguments instance.
static const SupportedGC SupportedGCs[] = {
   SupportedGC(UseEpsilonGC,       CollectedHeap::Epsilon,  epsilonArguments,  "epsilon gc"),
        SupportedGC(UseG1GC,            CollectedHeap::G1,       g1Arguments,       "g1 gc"),
};

#define FOR_EACH_SUPPORTED_GC(var) \
  for (const SupportedGC* var = &SupportedGCs[0]; var < &SupportedGCs[ARRAY_SIZE(SupportedGCs)]; var++)

#define FAIL_IF_SELECTED(option, enabled) \
  if (option == enabled && FLAG_IS_CMDLINE(option)) { \
    vm_exit_during_initialization(enabled ? \
                                  "Option -XX:+" #option " not supported" : \
                                  "Option -XX:-" #option " not supported"); \
  }

GCArguments* GCConfig::_arguments = NULL;
bool GCConfig::_gc_selected_ergonomically = false;

void GCConfig::fail_if_unsupported_gc_is_selected() {
       FAIL_IF_SELECTED(UseConcMarkSweepGC, true);
  FAIL_IF_SELECTED(UseParallelGC,      true);
  FAIL_IF_SELECTED(UseParallelOldGC,   true);
    FAIL_IF_SELECTED(UseSerialGC,        true);
    FAIL_IF_SELECTED(UseParallelOldGC,   false);
         FAIL_IF_SELECTED(UseZGC,             true);
}

void GCConfig::select_gc_ergonomically() {
  if (os::is_server_class_machine()) {
    FLAG_SET_ERGO_IF_DEFAULT(bool, UseG1GC, true);
  } else {
  }
}

bool GCConfig::is_no_gc_selected() {
  FOR_EACH_SUPPORTED_GC(gc) {
    if (gc->_flag) {
      return false;
    }
  }

  return true;
}

bool GCConfig::is_exactly_one_gc_selected() {
  CollectedHeap::Name selected = CollectedHeap::None;

  FOR_EACH_SUPPORTED_GC(gc) {
    if (gc->_flag) {
      if (gc->_name == selected || selected == CollectedHeap::None) {
        // Selected
        selected = gc->_name;
      } else {
        // More than one selected
        return false;
      }
    }
  }

  return selected != CollectedHeap::None;
}

GCArguments* GCConfig::select_gc() {
  // Fail immediately if an unsupported GC is selected
  fail_if_unsupported_gc_is_selected();

  if (is_no_gc_selected()) {
    // Try select GC ergonomically
    select_gc_ergonomically();

    if (is_no_gc_selected()) {
      // Failed to select GC ergonomically
      vm_exit_during_initialization("Garbage collector not selected "
                                    "(default collector explicitly disabled)", NULL);
    }

    // Succeeded to select GC ergonomically
    _gc_selected_ergonomically = true;
  }

  if (!is_exactly_one_gc_selected()) {
    // More than one GC selected
    vm_exit_during_initialization("Multiple garbage collectors selected", NULL);
  }

  // Exactly one GC selected
  FOR_EACH_SUPPORTED_GC(gc) {
    if (gc->_flag) {
      return &gc->_arguments;
    }
  }

  fatal("Should have found the selected GC");

  return NULL;
}

void GCConfig::initialize() {
  assert(_arguments == NULL, "Already initialized");
  _arguments = select_gc();
}

bool GCConfig::is_gc_supported(CollectedHeap::Name name) {
  FOR_EACH_SUPPORTED_GC(gc) {
    if (gc->_name == name) {
      // Supported
      return true;
    }
  }

  // Not supported
  return false;
}

bool GCConfig::is_gc_selected(CollectedHeap::Name name) {
  FOR_EACH_SUPPORTED_GC(gc) {
    if (gc->_name == name && gc->_flag) {
      // Selected
      return true;
    }
  }

  // Not selected
  return false;
}

bool GCConfig::is_gc_selected_ergonomically() {
  return _gc_selected_ergonomically;
}

const char* GCConfig::hs_err_name() {
  if (is_exactly_one_gc_selected()) {
    // Exacly one GC selected
    FOR_EACH_SUPPORTED_GC(gc) {
      if (gc->_flag) {
        return gc->_hs_err_name;
      }
    }
  }

  // Zero or more than one GC selected
  return "unknown gc";
}

const char* GCConfig::hs_err_name(CollectedHeap::Name name) {
  FOR_EACH_SUPPORTED_GC(gc) {
    if (gc->_name == name) {
      return gc->_hs_err_name;
    }
  }
  return "unknown gc";
}

GCArguments* GCConfig::arguments() {
  assert(_arguments != NULL, "Not initialized");
  return _arguments;
}

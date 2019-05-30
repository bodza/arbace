#ifndef SHARE_GC_SHARED_GCCONFIG_HPP
#define SHARE_GC_SHARED_GCCONFIG_HPP

#include "gc/shared/collectedHeap.hpp"
#include "memory/allocation.hpp"

class GCArguments;

class GCConfig : public AllStatic {
private:
  static GCArguments* _arguments;
  static bool         _gc_selected_ergonomically;

  static void fail_if_unsupported_gc_is_selected();
  static bool is_no_gc_selected();
  static bool is_exactly_one_gc_selected();

  static void select_gc_ergonomically();
  static GCArguments* select_gc();

public:
  static void initialize();

  static bool is_gc_supported(CollectedHeap::Name name);
  static bool is_gc_selected(CollectedHeap::Name name);
  static bool is_gc_selected_ergonomically();

  static const char* hs_err_name();
  static const char* hs_err_name(CollectedHeap::Name name);

  static GCArguments* arguments();
};

#endif

#ifndef SHARE_VM_MEMORY_METASPACETRACER_HPP
#define SHARE_VM_MEMORY_METASPACETRACER_HPP

#include "memory/allocation.hpp"
#include "memory/metaspace.hpp"
#include "memory/metaspaceGCThresholdUpdater.hpp"

class ClassLoaderData;

class MetaspaceTracer : public CHeapObj<mtTracing> {
  template <typename E>
  void send_allocation_failure_event(ClassLoaderData *cld,
                                     size_t word_size,
                                     MetaspaceObj::Type objtype,
                                     Metaspace::MetadataType mdtype) const;
 public:
  void report_gc_threshold(size_t old_val,
                           size_t new_val,
                           MetaspaceGCThresholdUpdater::Type updater) const;
  void report_metaspace_allocation_failure(ClassLoaderData *cld,
                                           size_t word_size,
                                           MetaspaceObj::Type objtype,
                                           Metaspace::MetadataType mdtype) const;
  void report_metadata_oom(ClassLoaderData *cld,
                           size_t word_size,
                           MetaspaceObj::Type objtype,
                           Metaspace::MetadataType mdtype) const;
};

#endif

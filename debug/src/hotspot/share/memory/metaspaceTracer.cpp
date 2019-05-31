#include "precompiled.hpp"
#include "classfile/classLoaderData.hpp"
// #include "jfr/jfrEvents.hpp"
#include "memory/metaspaceTracer.hpp"
#include "oops/oop.inline.hpp"

void MetaspaceTracer::report_gc_threshold(size_t old_val, size_t new_val, MetaspaceGCThresholdUpdater::Type updater) const {
  EventMetaspaceGCThreshold event;
  if (event.should_commit()) {
    event.set_oldValue(old_val);
    event.set_newValue(new_val);
    event.set_updater((u1)updater);
    event.commit();
  }
}

void MetaspaceTracer::report_metaspace_allocation_failure(ClassLoaderData *cld, size_t word_size, MetaspaceObj::Type objtype, Metaspace::MetadataType mdtype) const {
  send_allocation_failure_event<EventMetaspaceAllocationFailure>(cld, word_size, objtype, mdtype);
}

void MetaspaceTracer::report_metadata_oom(ClassLoaderData *cld, size_t word_size, MetaspaceObj::Type objtype, Metaspace::MetadataType mdtype) const {
  send_allocation_failure_event<EventMetaspaceOOM>(cld, word_size, objtype, mdtype);
}

template <typename E>
void MetaspaceTracer::send_allocation_failure_event(ClassLoaderData *cld, size_t word_size, MetaspaceObj::Type objtype, Metaspace::MetadataType mdtype) const {
  E event;
  if (event.should_commit()) {
    event.set_classLoader(cld);
    if (cld->is_anonymous()) {
      event.set_anonymousClassLoader(true);
    } else {
      event.set_anonymousClassLoader(false);
    }
    event.set_size(word_size * BytesPerWord);
    event.set_metadataType((u1) mdtype);
    event.set_metaspaceObjectType((u1) objtype);
    event.commit();
  }
}

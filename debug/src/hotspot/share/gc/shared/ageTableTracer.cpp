#include "precompiled.hpp"

#include "gc/shared/ageTableTracer.hpp"
#include "gc/shared/gcId.hpp"

void AgeTableTracer::send_tenuring_distribution_event(uint age, size_t size) {
  EventTenuringDistribution e;
  if (e.should_commit()) {
    e.set_gcId(GCId::current());
    e.set_age(age);
    e.set_size(size);
    e.commit();
  }
}

bool AgeTableTracer::is_tenuring_distribution_event_enabled() {
  return EventTenuringDistribution::is_enabled();
}

#include "precompiled.hpp"

#include "memory/metaspaceCounters.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/globals.hpp"
#include "utilities/exceptions.hpp"

size_t MetaspaceCounters::used()         { return MetaspaceUtils::used_bytes(); }
size_t MetaspaceCounters::capacity()     { return MetaspaceUtils::committed_bytes(); }
size_t MetaspaceCounters::max_capacity() { return MetaspaceUtils::reserved_bytes(); }

void MetaspaceCounters::initialize_performance_counters() { }
void MetaspaceCounters::update_performance_counters() { }

size_t CompressedClassSpaceCounters::used()         { return MetaspaceUtils::used_bytes(Metaspace::ClassType); }
size_t CompressedClassSpaceCounters::capacity()     { return MetaspaceUtils::committed_bytes(Metaspace::ClassType); }
size_t CompressedClassSpaceCounters::max_capacity() { return MetaspaceUtils::reserved_bytes(Metaspace::ClassType); }

void CompressedClassSpaceCounters::update_performance_counters() { }
void CompressedClassSpaceCounters::initialize_performance_counters() { }

#include "precompiled.hpp"

#include "classfile/systemDictionary.hpp"
#include "memory/allocation.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "services/classLoadingService.hpp"
#include "services/memoryService.hpp"
#include "utilities/dtrace.hpp"
#include "utilities/macros.hpp"
#include "utilities/defaultStream.hpp"
#include "logging/log.hpp"
#include "logging/logConfiguration.hpp"

#define DTRACE_CLASSLOAD_PROBE(type, clss, shared)

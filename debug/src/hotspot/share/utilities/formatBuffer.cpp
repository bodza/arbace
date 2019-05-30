#include "precompiled.hpp"
#include "jvm.h"
#include "memory/allocation.hpp"
#include "utilities/formatBuffer.hpp"

#include <stdarg.h>

FormatBufferResource::FormatBufferResource(const char * format, ...)
  : FormatBufferBase((char*)resource_allocate_bytes(FormatBufferBase::BufferSize)) {
  va_list argp;
  va_start(argp, format);
  jio_vsnprintf(_buf, FormatBufferBase::BufferSize, format, argp);
  va_end(argp);
}

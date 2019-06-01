#include "precompiled.hpp"

#include "jvm.h"
#include "logging/logDecorators.hpp"
#include "logging/logDecorations.hpp"
#include "logging/logFileStreamOutput.hpp"
#include "logging/logMessageBuffer.hpp"
#include "memory/allocation.inline.hpp"

static bool initialized;
static union {
  char stdoutmem[sizeof(LogStdoutOutput)];
  jlong dummy;
} aligned_stdoutmem;
static union {
  char stderrmem[sizeof(LogStderrOutput)];
  jlong dummy;
} aligned_stderrmem;

LogStdoutOutput &StdoutLog = reinterpret_cast<LogStdoutOutput&>(aligned_stdoutmem.stdoutmem);
LogStderrOutput &StderrLog = reinterpret_cast<LogStderrOutput&>(aligned_stderrmem.stderrmem);

LogFileStreamInitializer::LogFileStreamInitializer() {
  if (!initialized) {
    ::new (&StdoutLog) LogStdoutOutput();
    ::new (&StderrLog) LogStderrOutput();
    initialized = true;
  }
}

int LogFileStreamOutput::write_decorations(const LogDecorations& decorations) {
  int total_written = 0;

  for (uint i = 0; i < LogDecorators::Count; i++) {
    LogDecorators::Decorator decorator = static_cast<LogDecorators::Decorator>(i);
    if (!_decorators.is_decorator(decorator)) {
      continue;
    }

    int written = jio_fprintf(_stream, "[%-*s]", _decorator_padding[decorator], decorations.decoration(decorator));
    if (written <= 0) {
      return -1;
    } else if (static_cast<size_t>(written - 2) > _decorator_padding[decorator]) {
      _decorator_padding[decorator] = written - 2;
    }
    total_written += written;
  }
  return total_written;
}

int LogFileStreamOutput::write(const LogDecorations& decorations, const char* msg) {
  const bool use_decorations = !_decorators.is_empty();

  int written = 0;
  os::flockfile(_stream);
  if (use_decorations) {
    written += write_decorations(decorations);
    written += jio_fprintf(_stream, " ");
  }
  written += jio_fprintf(_stream, "%s\n", msg);
  fflush(_stream);
  os::funlockfile(_stream);

  return written;
}

int LogFileStreamOutput::write(LogMessageBuffer::Iterator msg_iterator) {
  const bool use_decorations = !_decorators.is_empty();

  int written = 0;
  os::flockfile(_stream);
  for (; !msg_iterator.is_at_end(); msg_iterator++) {
    if (use_decorations) {
      written += write_decorations(msg_iterator.decorations());
      written += jio_fprintf(_stream, " ");
    }
    written += jio_fprintf(_stream, "%s\n", msg_iterator.message());
  }
  fflush(_stream);
  os::funlockfile(_stream);

  return written;
}

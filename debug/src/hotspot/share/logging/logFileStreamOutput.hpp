#ifndef SHARE_VM_LOGGING_LOGFILESTREAMOUTPUT_HPP
#define SHARE_VM_LOGGING_LOGFILESTREAMOUTPUT_HPP

#include "logging/logDecorators.hpp"
#include "logging/logOutput.hpp"
#include "utilities/globalDefinitions.hpp"

class LogDecorations;

class LogFileStreamInitializer {
 public:
  LogFileStreamInitializer();
};

// Ensure the default log streams have been initialized (stdout, stderr) using the static initializer below
static LogFileStreamInitializer log_stream_initializer;

// Base class for all FileStream-based log outputs.
class LogFileStreamOutput : public LogOutput {
 protected:
  FILE*               _stream;
  size_t              _decorator_padding[LogDecorators::Count];

  LogFileStreamOutput(FILE *stream) : _stream(stream) {
    for (size_t i = 0; i < LogDecorators::Count; i++) {
      _decorator_padding[i] = 0;
    }
  }

  int write_decorations(const LogDecorations& decorations);

 public:
  virtual int write(const LogDecorations& decorations, const char* msg);
  virtual int write(LogMessageBuffer::Iterator msg_iterator);
};

class LogStdoutOutput : public LogFileStreamOutput {
  friend class LogFileStreamInitializer;
 private:
  LogStdoutOutput() : LogFileStreamOutput(stdout) {
    set_config_string("all=warning");
  }
  virtual bool initialize(const char* options, outputStream* errstream) {
    return false;
  }
 public:
  virtual const char* name() const {
    return "stdout";
  }
};

class LogStderrOutput : public LogFileStreamOutput {
  friend class LogFileStreamInitializer;
 private:
  LogStderrOutput() : LogFileStreamOutput(stderr) {
    set_config_string("all=off");
  }
  virtual bool initialize(const char* options, outputStream* errstream) {
    return false;
  }
 public:
  virtual const char* name() const {
    return "stderr";
  }
};

extern LogStderrOutput &StderrLog;
extern LogStdoutOutput &StdoutLog;

#endif

#ifndef SHARE_VM_LOGGING_LOGFILEOUTPUT_HPP
#define SHARE_VM_LOGGING_LOGFILEOUTPUT_HPP

#include "logging/logFileStreamOutput.hpp"
#include "runtime/semaphore.hpp"
#include "utilities/globalDefinitions.hpp"

class LogDecorations;

// The log file output, with support for file rotation based on a target size.
class LogFileOutput : public LogFileStreamOutput {
 private:
  static const char* const FileOpenMode;
  static const char* const FileCountOptionKey;
  static const char* const FileSizeOptionKey;
  static const char* const PidFilenamePlaceholder;
  static const char* const TimestampFilenamePlaceholder;
  static const char* const TimestampFormat;
  static const size_t DefaultFileCount = 5;
  static const size_t DefaultFileSize = 20 * M;
  static const size_t StartTimeBufferSize = 20;
  static const size_t PidBufferSize = 21;
  static const uint   MaxRotationFileCount = 1000;
  static char         _pid_str[PidBufferSize];
  static char         _vm_start_time_str[StartTimeBufferSize];

  const char* _name;
  char* _file_name;
  char* _archive_name;

  uint  _current_file;
  uint  _file_count;
  uint  _file_count_max_digits;

  size_t  _archive_name_len;
  size_t  _rotate_size;
  size_t  _current_size;

  // Semaphore used for synchronizing file rotations and writes
  Semaphore _rotation_semaphore;

  void archive();
  void rotate();
  bool parse_options(const char* options, outputStream* errstream);
  char *make_file_name(const char* file_name, const char* pid_string, const char* timestamp_string);

  bool should_rotate() {
    return _file_count > 0 && _rotate_size > 0 && _current_size >= _rotate_size;
  }

  void increment_file_count() {
    _current_file++;
    if (_current_file == _file_count) {
      _current_file = 0;
    }
  }

 public:
  LogFileOutput(const char *name);
  virtual ~LogFileOutput();
  virtual bool initialize(const char* options, outputStream* errstream);
  virtual int write(const LogDecorations& decorations, const char* msg);
  virtual int write(LogMessageBuffer::Iterator msg_iterator);
  virtual void force_rotate();
  virtual void describe(outputStream* out);

  virtual const char* name() const {
    return _name;
  }

  static const char* const Prefix;
  static void set_file_name_parameters(jlong start_time);
};

#endif

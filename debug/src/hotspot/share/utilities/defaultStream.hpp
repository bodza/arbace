#ifndef SHARE_VM_UTILITIES_DEFAULTSTREAM_HPP
#define SHARE_VM_UTILITIES_DEFAULTSTREAM_HPP

#include "utilities/xmlstream.hpp"

class defaultStream : public xmlTextStream {
  friend void ostream_abort();
 public:
  enum { NO_WRITER = -1 };
 private:
  static int   _output_fd;
  static int   _error_fd;
  static FILE* _output_stream;
  static FILE* _error_stream;

  fileStream* open_file(const char* log_name);
 public:
  // must defer time stamp due to the fact that os::init() hasn't
  // yet been called and os::elapsed_counter() may not be valid
  defaultStream() {
    _writer = -1;
    _last_writer = -1;
  }
  ~defaultStream() { }

  static inline FILE* output_stream() { return DisplayVMOutputToStderr ? _error_stream : _output_stream; }
  static inline FILE* error_stream()  { return DisplayVMOutputToStdout ? _output_stream : _error_stream; }
  static inline int output_fd()       { return DisplayVMOutputToStderr ? _error_fd : _output_fd; }
  static inline int error_fd()        { return DisplayVMOutputToStdout ? _output_fd : _error_fd; }

  virtual void write(const char* s, size_t len);

  void flush() {
    xmlTextStream::flush();
    fflush(output_stream());
  }

  // advisory lock/unlock of _writer field:
 private:
  intx _writer;    // thread_id with current rights to output
  intx _last_writer;
 public:
  intx hold(intx writer_id);
  void release(intx holder);
  intx writer() { return _writer; }

  static defaultStream* instance;  // sole instance
};

#endif

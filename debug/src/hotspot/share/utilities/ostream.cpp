#include "precompiled.hpp"

#include "jvm.h"
#include "memory/allocation.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/arguments.hpp"
#include "runtime/os.inline.hpp"
#include "runtime/vm_version.hpp"
#include "utilities/defaultStream.hpp"
#include "utilities/macros.hpp"
#include "utilities/ostream.hpp"
#include "utilities/vmError.hpp"
#include "utilities/xmlstream.hpp"

// Declarations of jvm methods
extern "C" void jio_print(const char* s, size_t len);
extern "C" int jio_printf(const char *fmt, ...);

outputStream::outputStream(int width) {
  _width       = width;
  _position    = 0;
  _newlines    = 0;
  _precount    = 0;
  _indentation = 0;
  _scratch     = NULL;
  _scratch_len = 0;
}

outputStream::outputStream(int width, bool has_time_stamps) {
  _width       = width;
  _position    = 0;
  _newlines    = 0;
  _precount    = 0;
  _indentation = 0;
  _scratch     = NULL;
  _scratch_len = 0;
  if (has_time_stamps)  _stamp.update();
}

void outputStream::update_position(const char* s, size_t len) {
  for (size_t i = 0; i < len; i++) {
    char ch = s[i];
    if (ch == '\n') {
      _newlines += 1;
      _precount += _position + 1;
      _position = 0;
    } else if (ch == '\t') {
      int tw = 8 - (_position & 7);
      _position += tw;
      _precount -= tw - 1;  // invariant:  _precount + _position == total count
    } else {
      _position += 1;
    }
  }
}

// Execute a vsprintf, using the given buffer if necessary.
// Return a pointer to the formatted string.
const char* outputStream::do_vsnprintf(char* buffer, size_t buflen, const char* format, va_list ap, bool add_cr, size_t& result_len) {
  const char* result;
  if (add_cr)  buflen--;
  if (!strchr(format, '%')) {
    // constant format string
    result = format;
    result_len = strlen(result);
    if (add_cr && result_len >= buflen)  result_len = buflen - 1;  // truncate
  } else if (format[0] == '%' && format[1] == 's' && format[2] == '\0') {
    // trivial copy-through format string
    result = va_arg(ap, const char*);
    result_len = strlen(result);
    if (add_cr && result_len >= buflen)  result_len = buflen - 1;  // truncate
  } else {
    int written = os::vsnprintf(buffer, buflen, format, ap);
    result = buffer;
    if ((size_t)written < buflen) {
      result_len = written;
    } else {
      result_len = buflen - 1;
    }
  }
  if (add_cr) {
    if (result != buffer) {
      memcpy(buffer, result, result_len);
      result = buffer;
    }
    buffer[result_len++] = '\n';
    buffer[result_len] = 0;
  }
  return result;
}

void outputStream::do_vsnprintf_and_write_with_automatic_buffer(const char* format, va_list ap, bool add_cr) {
  char buffer[O_BUFLEN];
  size_t len;
  const char* str = do_vsnprintf(buffer, sizeof(buffer), format, ap, add_cr, len);
  write(str, len);
}

void outputStream::do_vsnprintf_and_write_with_scratch_buffer(const char* format, va_list ap, bool add_cr) {
  size_t len;
  const char* str = do_vsnprintf(_scratch, _scratch_len, format, ap, add_cr, len);
  write(str, len);
}

void outputStream::do_vsnprintf_and_write(const char* format, va_list ap, bool add_cr) {
  if (_scratch) {
    do_vsnprintf_and_write_with_scratch_buffer(format, ap, add_cr);
  } else {
    do_vsnprintf_and_write_with_automatic_buffer(format, ap, add_cr);
  }
}

void outputStream::print(const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  do_vsnprintf_and_write(format, ap, false);
  va_end(ap);
}

void outputStream::print_cr(const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  do_vsnprintf_and_write(format, ap, true);
  va_end(ap);
}

void outputStream::vprint(const char *format, va_list argptr) {
  do_vsnprintf_and_write(format, argptr, false);
}

void outputStream::vprint_cr(const char* format, va_list argptr) {
  do_vsnprintf_and_write(format, argptr, true);
}

void outputStream::fill_to(int col) {
  int need_fill = col - position();
  sp(need_fill);
}

void outputStream::move_to(int col, int slop, int min_space) {
  if (position() >= col + slop)
    cr();
  int need_fill = col - position();
  if (need_fill < min_space)
    need_fill = min_space;
  sp(need_fill);
}

void outputStream::put(char ch) {
  char buf[] = { ch, '\0' };
  write(buf, 1);
}

#define SP_USE_TABS false

void outputStream::sp(int count) {
  if (count < 0)  return;
  if (SP_USE_TABS && count >= 8) {
    int target = position() + count;
    while (count >= 8) {
      this->write("\t", 1);
      count -= 8;
    }
    count = target - position();
  }
  while (count > 0) {
    int nw = (count > 8) ? 8 : count;
    this->write("        ", nw);
    count -= nw;
  }
}

void outputStream::cr() {
  this->write("\n", 1);
}

void outputStream::cr_indent() {
  cr(); indent();
}

void outputStream::stamp() {
  if (!_stamp.is_updated()) {
    _stamp.update(); // start at 0 on first call to stamp()
  }

  // outputStream::stamp() may get called by ostream_abort(), use snprintf
  // to avoid allocating large stack buffer in print().
  char buf[40];
  jio_snprintf(buf, sizeof(buf), "%.3f", _stamp.seconds());
  print_raw(buf);
}

void outputStream::stamp(bool guard, const char* prefix, const char* suffix) {
  if (!guard) {
    return;
  }
  print_raw(prefix);
  stamp();
  print_raw(suffix);
}

void outputStream::date_stamp(bool guard, const char* prefix, const char* suffix) {
  if (!guard) {
    return;
  }
  print_raw(prefix);
  static const char error_time[] = "yyyy-mm-ddThh:mm:ss.mmm+zzzz";
  static const int buffer_length = 32;
  char buffer[buffer_length];
  const char* iso8601_result = os::iso8601_time(buffer, buffer_length);
  if (iso8601_result != NULL) {
    print_raw(buffer);
  } else {
    print_raw(error_time);
  }
  print_raw(suffix);
  return;
}

outputStream& outputStream::indent() {
  while (_position < _indentation) sp();
  return *this;
}

void outputStream::print_jlong(jlong value) {
  print(JLONG_FORMAT, value);
}

void outputStream::print_julong(julong value) {
  print(JULONG_FORMAT, value);
}

/**
 * This prints out hex data in a 'windbg' or 'xxd' form, where each line is:
 *   <hex-address>: 8 * <hex-halfword> <ascii translation (optional)>
 * example:
 * 0000000: 7f44 4f46 0102 0102 0000 0000 0000 0000  .DOF............
 * 0000010: 0000 0000 0000 0040 0000 0020 0000 0005  .......@... ....
 * 0000020: 0000 0000 0000 0040 0000 0000 0000 015d  .......@.......]
 * ...
 *
 * indent is applied to each line.  Ends with a CR.
 */
void outputStream::print_data(void* data, size_t len, bool with_ascii) {
  size_t limit = (len + 16) / 16 * 16;
  for (size_t i = 0; i < limit; ++i) {
    if (i % 16 == 0) {
      indent().print(INTPTR_FORMAT_W(07) ":", i);
    }
    if (i % 2 == 0) {
      print(" ");
    }
    if (i < len) {
      print("%02x", ((unsigned char*)data)[i]);
    } else {
      print("  ");
    }
    if ((i + 1) % 16 == 0) {
      if (with_ascii) {
        print("  ");
        for (size_t j = 0; j < 16; ++j) {
          size_t idx = i + j - 15;
          if (idx < len) {
            char c = ((char*)data)[idx];
            print("%c", c >= 32 && c <= 126 ? c : '.');
          }
        }
      }
      cr();
    }
  }
}

stringStream::stringStream(size_t initial_size) : outputStream() {
  buffer_length = initial_size;
  buffer        = NEW_RESOURCE_ARRAY(char, buffer_length);
  buffer_pos    = 0;
  buffer_fixed  = false;
}

// useful for output to fixed chunks of memory, such as performance counters
stringStream::stringStream(char* fixed_buffer, size_t fixed_buffer_size) : outputStream() {
  buffer_length = fixed_buffer_size;
  buffer        = fixed_buffer;
  buffer_pos    = 0;
  buffer_fixed  = true;
}

void stringStream::write(const char* s, size_t len) {
  size_t write_len = len;               // number of non-null bytes to write
  size_t end = buffer_pos + len + 1;    // position after write and final '\0'
  if (end > buffer_length) {
    if (buffer_fixed) {
      // if buffer cannot resize, silently truncate
      end = buffer_length;
      write_len = end - buffer_pos - 1; // leave room for the final '\0'
    } else {
      // For small overruns, double the buffer.  For larger ones,
      // increase to the requested size.
      if (end < buffer_length * 2) {
        end = buffer_length * 2;
      }
      char* oldbuf = buffer;
      buffer = NEW_RESOURCE_ARRAY(char, end);
      if (buffer_pos > 0) {
        memcpy(buffer, oldbuf, buffer_pos);
      }
      buffer_length = end;
    }
  }
  // invariant: buffer is always null-terminated
  guarantee(buffer_pos + write_len + 1 <= buffer_length, "stringStream oob");
  if (write_len > 0) {
    buffer[buffer_pos + write_len] = 0;
    memcpy(buffer + buffer_pos, s, write_len);
    buffer_pos += write_len;
  }

  // Note that the following does not depend on write_len.
  // This means that position and count get updated
  // even when overflow occurs.
  update_position(s, len);
}

char* stringStream::as_string() {
  char* copy = NEW_RESOURCE_ARRAY(char, buffer_pos + 1);
  strncpy(copy, buffer, buffer_pos);
  copy[buffer_pos] = 0;  // terminating null
  return copy;
}

stringStream::~stringStream() { }

xmlStream*   xtty;
outputStream* tty;
extern Mutex* tty_lock;

#define EXTRACHARLEN   32
#define CURRENTAPPX    ".current"
// convert YYYY-MM-DD HH:MM:SS to YYYY-MM-DD_HH-MM-SS
char* get_datetime_string(char *buf, size_t len) {
  os::local_time_string(buf, len);
  int i = (int)strlen(buf);
  while (--i >= 0) {
    if (buf[i] == ' ') buf[i] = '_';
    else if (buf[i] == ':') buf[i] = '-';
  }
  return buf;
}

static const char* make_log_name_internal(const char* log_name, const char* force_directory, int pid, const char* tms) {
  const char* basename = log_name;
  char file_sep = os::file_separator()[0];
  const char* cp;
  char  pid_text[32];

  for (cp = log_name; *cp != '\0'; cp++) {
    if (*cp == '/' || *cp == file_sep) {
      basename = cp + 1;
    }
  }
  const char* nametail = log_name;
  // Compute buffer length
  size_t buffer_length;
  if (force_directory != NULL) {
    buffer_length = strlen(force_directory) + strlen(os::file_separator()) + strlen(basename) + 1;
  } else {
    buffer_length = strlen(log_name) + 1;
  }

  const char* pts = strstr(basename, "%p");
  int pid_pos = (pts == NULL) ? -1 : (pts - nametail);

  if (pid_pos >= 0) {
    jio_snprintf(pid_text, sizeof(pid_text), "pid%u", pid);
    buffer_length += strlen(pid_text);
  }

  pts = strstr(basename, "%t");
  int tms_pos = (pts == NULL) ? -1 : (pts - nametail);
  if (tms_pos >= 0) {
    buffer_length += strlen(tms);
  }

  // File name is too long.
  if (buffer_length > JVM_MAXPATHLEN) {
    return NULL;
  }

  // Create big enough buffer.
  char *buf = NEW_C_HEAP_ARRAY(char, buffer_length, mtInternal);

  strcpy(buf, "");
  if (force_directory != NULL) {
    strcat(buf, force_directory);
    strcat(buf, os::file_separator());
    nametail = basename;       // completely skip directory prefix
  }

  // who is first, %p or %t?
  int first = -1, second = -1;
  const char *p1st = NULL;
  const char *p2nd = NULL;

  if (pid_pos >= 0 && tms_pos >= 0) {
    // contains both %p and %t
    if (pid_pos < tms_pos) {
      // case foo%pbar%tmonkey.log
      first  = pid_pos;
      p1st   = pid_text;
      second = tms_pos;
      p2nd   = tms;
    } else {
      // case foo%tbar%pmonkey.log
      first  = tms_pos;
      p1st   = tms;
      second = pid_pos;
      p2nd   = pid_text;
    }
  } else if (pid_pos >= 0) {
    // contains %p only
    first  = pid_pos;
    p1st   = pid_text;
  } else if (tms_pos >= 0) {
    // contains %t only
    first  = tms_pos;
    p1st   = tms;
  }

  int buf_pos = (int)strlen(buf);
  const char* tail = nametail;

  if (first >= 0) {
    tail = nametail + first + 2;
    strncpy(&buf[buf_pos], nametail, first);
    strcpy(&buf[buf_pos + first], p1st);
    buf_pos = (int)strlen(buf);
    if (second >= 0) {
      strncpy(&buf[buf_pos], tail, second - first - 2);
      strcpy(&buf[buf_pos + second - first - 2], p2nd);
      tail = nametail + second + 2;
    }
  }
  strcat(buf, tail);      // append rest of name, or all of name
  return buf;
}

static const char* make_log_name(const char* log_name, const char* force_directory) {
  char timestr[32];
  get_datetime_string(timestr, sizeof(timestr));
  return make_log_name_internal(log_name, force_directory, os::current_process_id(), timestr);
}

fileStream::fileStream(const char* file_name) {
  _file = fopen(file_name, "w");
  if (_file != NULL) {
    _need_close = true;
  } else {
    warning("Cannot open file %s due to %s\n", file_name, os::strerror(errno));
    _need_close = false;
  }
}

fileStream::fileStream(const char* file_name, const char* opentype) {
  _file = fopen(file_name, opentype);
  if (_file != NULL) {
    _need_close = true;
  } else {
    warning("Cannot open file %s due to %s\n", file_name, os::strerror(errno));
    _need_close = false;
  }
}

void fileStream::write(const char* s, size_t len) {
  if (_file != NULL) {
    // Make an unused local variable to avoid warning from gcc 4.x compiler.
    size_t count = fwrite(s, 1, len, _file);
  }
  update_position(s, len);
}

long fileStream::fileSize() {
  long size = -1;
  if (_file != NULL) {
    long pos = ::ftell(_file);
    if (pos < 0) return pos;
    if (::fseek(_file, 0, SEEK_END) == 0) {
      size = ::ftell(_file);
    }
    ::fseek(_file, pos, SEEK_SET);
  }
  return size;
}

char* fileStream::readln(char *data, int count) {
  char * ret = ::fgets(data, count, _file);
  //Get rid of annoying \n char
  data[::strlen(data)-1] = '\0';
  return ret;
}

fileStream::~fileStream() {
  if (_file != NULL) {
    if (_need_close) fclose(_file);
    _file      = NULL;
  }
}

void fileStream::flush() {
  fflush(_file);
}

fdStream::fdStream(const char* file_name) {
  _fd = open(file_name, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  _need_close = true;
}

fdStream::~fdStream() {
  if (_fd != -1) {
    if (_need_close) close(_fd);
    _fd = -1;
  }
}

void fdStream::write(const char* s, size_t len) {
  if (_fd != -1) {
    // Make an unused local variable to avoid warning from gcc 4.x compiler.
    size_t count = ::write(_fd, s, (int)len);
  }
  update_position(s, len);
}

defaultStream* defaultStream::instance = NULL;
int defaultStream::_output_fd = 1;
int defaultStream::_error_fd  = 2;
FILE* defaultStream::_output_stream = stdout;
FILE* defaultStream::_error_stream  = stderr;

fileStream* defaultStream::open_file(const char* log_name) {
  const char* try_name = make_log_name(log_name, NULL);
  if (try_name == NULL) {
    warning("Cannot open file %s: file name is too long.\n", log_name);
    return NULL;
  }

  fileStream* file = new(ResourceObj::C_HEAP, mtInternal) fileStream(try_name);
  FREE_C_HEAP_ARRAY(char, try_name);
  if (file->is_open()) {
    return file;
  }

  // Try again to open the file in the temp directory.
  delete file;
  // Note: This feature is for maintainer use only.  No need for L10N.
  jio_printf("Warning:  Cannot open log file: %s\n", log_name);
  try_name = make_log_name(log_name, os::get_temp_directory());
  if (try_name == NULL) {
    warning("Cannot open file %s: file name is too long for directory %s.\n", log_name, os::get_temp_directory());
    return NULL;
  }

  file = new(ResourceObj::C_HEAP, mtInternal) fileStream(try_name);
  FREE_C_HEAP_ARRAY(char, try_name);
  if (file->is_open()) {
    return file;
  }

  delete file;
  return NULL;
}

intx defaultStream::hold(intx writer_id) {
  if (// impossible, but who knows?
      writer_id == NO_WRITER ||
      // bootstrap problem
      tty_lock == NULL ||
      // can't grab a lock if current Thread isn't set
      Thread::current_or_null() == NULL ||
      // developer hook
      !SerializeVMOutput ||
      // VM already unhealthy
      VMError::is_error_reported() ||
      // safepoint == global lock (for VM only)
      (SafepointSynchronize::is_synchronizing() && Thread::current()->is_VM_thread())) {
    // do not attempt to lock unless we know the thread and the VM is healthy
    return NO_WRITER;
  }
  if (_writer == writer_id) {
    // already held, no need to re-grab the lock
    return NO_WRITER;
  }
  tty_lock->lock_without_safepoint_check();
  // got the lock
  if (writer_id != _last_writer) {
    _last_writer = writer_id;
  }
  _writer = writer_id;
  return writer_id;
}

void defaultStream::release(intx holder) {
  if (holder == NO_WRITER) {
    // nothing to release:  either a recursive lock, or we scribbled (too bad)
    return;
  }
  if (_writer != holder) {
    return;  // already unlocked, perhaps via break_tty_lock_for_safepoint
  }
  _writer = NO_WRITER;
  tty_lock->unlock();
}

void defaultStream::write(const char* s, size_t len) {
  intx thread_id = os::current_thread_id();
  intx holder = hold(thread_id);

  if (DisplayVMOutput && (_outer_xmlStream == NULL || !_outer_xmlStream->inside_attrs())) {
    // print to output stream. It can be redirected by a vfprintf hook
    jio_print(s, len);
  }

  update_position(s, len);

  release(holder);
}

intx ttyLocker::hold_tty() {
  if (defaultStream::instance == NULL)  return defaultStream::NO_WRITER;
  intx thread_id = os::current_thread_id();
  return defaultStream::instance->hold(thread_id);
}

void ttyLocker::release_tty(intx holder) {
  if (holder == defaultStream::NO_WRITER)  return;
  defaultStream::instance->release(holder);
}

bool ttyLocker::release_tty_if_locked() {
  intx thread_id = os::current_thread_id();
  if (defaultStream::instance->writer() == thread_id) {
    // release the lock and return true so callers know if was
    // previously held.
    release_tty(thread_id);
    return true;
  }
  return false;
}

void ttyLocker::break_tty_lock_for_safepoint(intx holder) {
  if (defaultStream::instance != NULL && defaultStream::instance->writer() == holder) {
    if (xtty != NULL) {
      xtty->print_cr("<!-- safepoint while printing -->");
    }
    defaultStream::instance->release(holder);
  }
  // (else there was no lock to break)
}

void ostream_init() {
  if (defaultStream::instance == NULL) {
    defaultStream::instance = new(ResourceObj::C_HEAP, mtInternal) defaultStream();
    tty = defaultStream::instance;

    // We want to ensure that time stamps in GC logs consider time 0
    // the time when the JVM is initialized, not the first time we ask
    // for a time stamp. So, here, we explicitly update the time stamp
    // of tty.
    tty->time_stamp().update_to(1);
  }
}

// ostream_exit() is called during normal VM exit to finish log files, flush
// output and free resource.
void ostream_exit() {
  static bool ostream_exit_called = false;
  if (ostream_exit_called)  return;
  ostream_exit_called = true;
  if (tty != defaultStream::instance) {
    delete tty;
  }
  if (defaultStream::instance != NULL) {
    delete defaultStream::instance;
  }
  tty = NULL;
  xtty = NULL;
  defaultStream::instance = NULL;
}

// ostream_abort() is called by os::abort() when VM is about to die.
void ostream_abort() {
  // Here we can't delete tty, just flush its output
  if (tty) tty->flush();
}

bufferedStream::bufferedStream(size_t initial_size, size_t bufmax) : outputStream() {
  buffer_length = initial_size;
  buffer        = NEW_C_HEAP_ARRAY(char, buffer_length, mtInternal);
  buffer_pos    = 0;
  buffer_fixed  = false;
  buffer_max    = bufmax;
}

bufferedStream::bufferedStream(char* fixed_buffer, size_t fixed_buffer_size, size_t bufmax) : outputStream() {
  buffer_length = fixed_buffer_size;
  buffer        = fixed_buffer;
  buffer_pos    = 0;
  buffer_fixed  = true;
  buffer_max    = bufmax;
}

void bufferedStream::write(const char* s, size_t len) {
  if (buffer_pos + len > buffer_max) {
    flush();
  }

  size_t end = buffer_pos + len;
  if (end >= buffer_length) {
    if (buffer_fixed) {
      // if buffer cannot resize, silently truncate
      len = buffer_length - buffer_pos - 1;
    } else {
      // For small overruns, double the buffer.  For larger ones,
      // increase to the requested size.
      if (end < buffer_length * 2) {
        end = buffer_length * 2;
      }
      buffer = REALLOC_C_HEAP_ARRAY(char, buffer, end, mtInternal);
      buffer_length = end;
    }
  }
  memcpy(buffer + buffer_pos, s, len);
  buffer_pos += len;
  update_position(s, len);
}

char* bufferedStream::as_string() {
  char* copy = NEW_RESOURCE_ARRAY(char, buffer_pos + 1);
  strncpy(copy, buffer, buffer_pos);
  copy[buffer_pos] = 0;  // terminating null
  return copy;
}

bufferedStream::~bufferedStream() {
  if (!buffer_fixed) {
    FREE_C_HEAP_ARRAY(char, buffer);
  }
}

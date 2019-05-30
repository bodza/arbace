#ifndef SHARE_VM_COMPILER_COMPILELOG_HPP
#define SHARE_VM_COMPILER_COMPILELOG_HPP

#include "utilities/xmlstream.hpp"

class ciBaseObject;
class ciKlass;
class ciObject;
class ciMetadata;
class ciSymbol;

// CompileLog
//
// An open stream for logging information about activities in a
// compiler thread.  There is exactly one per CompilerThread,
// if the +LogCompilation switch is enabled.
class CompileLog : public xmlStream {
 private:
  const char*   _file;           // name of file where XML goes
  julong        _file_end;       // last good end of file
  intx          _thread_id;      // which compile thread

  stringStream  _context;        // optional, killable context marker
  char          _context_buffer[100];

  char*         _identities;     // array of boolean
  int           _identities_limit;
  int           _identities_capacity;

  CompileLog*   _next;           // static chain of all logs

  static CompileLog* _first;     // head of static chain

  void va_tag(bool push, const char* format, va_list ap) ATTRIBUTE_PRINTF(3, 0);

 public:
  CompileLog(const char* file_name, FILE* fp, intx thread_id);
  ~CompileLog();

  intx          thread_id()                      { return _thread_id; }
  const char*   file()                           { return _file; }

  // Optional context marker, to help place actions that occur during
  // parsing. If there is no log output until the next context string
  // or reset, context string will be silently ignored
  stringStream* context()                        { return &_context; }
  void    clear_context()                        { context()->reset(); }
  void      set_context(const char* format, ...) ATTRIBUTE_PRINTF(2, 3);

  void          name(ciSymbol* s);               // name='s'
  void          name(Symbol* s)                  { xmlStream::name(s); }
  void          name(ciKlass* k);

  // Output an object description, return obj->ident().
  int           identify(ciBaseObject* obj);
  void          clear_identities();

  void inline_fail   (const char* reason);
  void inline_success(const char* reason);

  // virtuals
  virtual void see_tag(const char* tag, bool push);
  virtual void pop_tag(const char* tag);

  // make a provisional end of log mark
  void mark_file_end() { _file_end = out()->count(); }

  // Print code cache statistics
  void code_cache_state();

  // copy all logs to the given stream
  static void finish_log(outputStream* out);
  static void finish_log_on_error(outputStream* out, char *buf, int buflen);
};

#endif

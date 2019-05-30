#include "precompiled.hpp"
#include "runtime/timerTrace.hpp"

TraceTime::TraceTime(const char* title,
                     bool doit) {
  _active   = doit;
  _verbose  = true;
  _title    = title;
  _print    = NULL;

  if (_active) {
    _accum = NULL;
    _t.start();
  }
}

TraceTime::TraceTime(const char* title,
                     elapsedTimer* accumulator,
                     bool doit,
                     bool verbose) {
  _active   = doit;
  _verbose  = verbose;
  _title    = title;
  _print    = NULL;

  if (_active) {
    _accum = accumulator;
    _t.start();
  }
}

TraceTime::TraceTime(const char* title,
                     TraceTimerLogPrintFunc ttlpf) {
  _active   = ttlpf!= NULL;
  _verbose  = true;
  _title    = title;
  _print    = ttlpf;

  if (_active) {
    _accum = NULL;
    _t.start();
  }
}

TraceTime::~TraceTime() {
  if (!_active) {
    return;
  }
  _t.stop();
  if (_accum != NULL) {
    _accum->add(_t);
  }
  if (!_verbose) {
    return;
  }
  if (_print) {
    _print("%s, %3.7f secs", _title, _t.seconds());
  } else {
    tty->print_cr("[%s, %3.7f secs]", _title, _t.seconds());
    tty->flush();
  }
}

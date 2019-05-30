#include "precompiled.hpp"
#include "gc/shared/workerDataArray.inline.hpp"
#include "utilities/ostream.hpp"

template <>
size_t WorkerDataArray<size_t>::uninitialized() {
  return (size_t)-1;
}

template <>
double WorkerDataArray<double>::uninitialized() {
  return -1.0;
}

template <>
void WorkerDataArray<double>::WDAPrinter::summary(outputStream* out, double min, double avg, double max, double diff, double sum, bool print_sum) {
  out->print(" Min: %4.1lf, Avg: %4.1lf, Max: %4.1lf, Diff: %4.1lf", min * MILLIUNITS, avg * MILLIUNITS, max * MILLIUNITS, diff* MILLIUNITS);
  if (print_sum) {
    out->print(", Sum: %4.1lf", sum * MILLIUNITS);
  }
}

template <>
void WorkerDataArray<size_t>::WDAPrinter::summary(outputStream* out, size_t min, double avg, size_t max, size_t diff, size_t sum, bool print_sum) {
  out->print(" Min: " SIZE_FORMAT ", Avg: %4.1lf, Max: " SIZE_FORMAT ", Diff: " SIZE_FORMAT, min, avg, max, diff);
  if (print_sum) {
    out->print(", Sum: " SIZE_FORMAT, sum);
  }
}

template <>
void WorkerDataArray<double>::WDAPrinter::details(const WorkerDataArray<double>* phase, outputStream* out) {
  out->print("%-25s", "");
  for (uint i = 0; i < phase->_length; ++i) {
    double value = phase->get(i);
    if (value != phase->uninitialized()) {
      out->print(" %4.1lf", phase->get(i) * 1000.0);
    } else {
      out->print(" -");
    }
  }
  out->cr();
}

template <>
void WorkerDataArray<size_t>::WDAPrinter::details(const WorkerDataArray<size_t>* phase, outputStream* out) {
  out->print("%-25s", "");
  for (uint i = 0; i < phase->_length; ++i) {
    size_t value = phase->get(i);
    if (value != phase->uninitialized()) {
      out->print("  " SIZE_FORMAT, phase->get(i));
    } else {
      out->print(" -");
    }
  }
  out->cr();
}

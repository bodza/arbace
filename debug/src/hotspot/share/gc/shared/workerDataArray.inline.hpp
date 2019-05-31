#ifndef SHARE_VM_GC_SHARED_WORKERDATAARRAY_INLINE_HPP
#define SHARE_VM_GC_SHARED_WORKERDATAARRAY_INLINE_HPP

#include "gc/shared/workerDataArray.hpp"
#include "memory/allocation.inline.hpp"
#include "utilities/ostream.hpp"

template <typename T>
WorkerDataArray<T>::WorkerDataArray(uint length, const char* title) :
 _title(title),
 _length(0) {
  _length = length;
  _data = NEW_C_HEAP_ARRAY(T, _length, mtGC);
  for (uint i = 0; i < MaxThreadWorkItems; i++) {
    _thread_work_items[i] = NULL;
  }
  reset();
}

template <typename T>
void WorkerDataArray<T>::set(uint worker_i, T value) {
  _data[worker_i] = value;
}

template <typename T>
T WorkerDataArray<T>::get(uint worker_i) const {
  return _data[worker_i];
}

template <typename T>
WorkerDataArray<T>::~WorkerDataArray() {
  FREE_C_HEAP_ARRAY(T, _data);
}

template <typename T>
void WorkerDataArray<T>::link_thread_work_items(WorkerDataArray<size_t>* thread_work_items, uint index) {
  _thread_work_items[index] = thread_work_items;
}

template <typename T>
void WorkerDataArray<T>::set_thread_work_item(uint worker_i, size_t value, uint index) {
  _thread_work_items[index]->set(worker_i, value);
}

template <typename T>
void WorkerDataArray<T>::add_thread_work_item(uint worker_i, size_t value, uint index) {
  _thread_work_items[index]->add(worker_i, value);
}

template <typename T>
void WorkerDataArray<T>::add(uint worker_i, T value) {
  _data[worker_i] += value;
}

template <typename T>
double WorkerDataArray<T>::average() const {
  uint contributing_threads = 0;
  for (uint i = 0; i < _length; ++i) {
    if (get(i) != uninitialized()) {
      contributing_threads++;
    }
  }
  if (contributing_threads == 0) {
    return 0.0;
  }
  return sum() / (double) contributing_threads;
}

template <typename T>
T WorkerDataArray<T>::sum() const {
  T s = 0;
  for (uint i = 0; i < _length; ++i) {
    if (get(i) != uninitialized()) {
      s += get(i);
    }
  }
  return s;
}

template <typename T>
void WorkerDataArray<T>::set_all(T value) {
  for (uint i = 0; i < _length; i++) {
    _data[i] = value;
  }
}

template <class T>
void WorkerDataArray<T>::print_summary_on(outputStream* out, bool print_sum) const {
  out->print("%-25s", title());
  uint start = 0;
  while (start < _length && get(start) == uninitialized()) {
    start++;
  }
  if (start < _length) {
    T min = get(start);
    T max = min;
    T sum = 0;
    uint contributing_threads = 0;
    for (uint i = start; i < _length; ++i) {
      T value = get(i);
      if (value != uninitialized()) {
        max = MAX2(max, value);
        min = MIN2(min, value);
        sum += value;
        contributing_threads++;
      }
    }
    T diff = max - min;
    double avg = sum / (double) contributing_threads;
    WDAPrinter::summary(out, min, avg, max, diff, sum, print_sum);
    out->print_cr(", Workers: %d", contributing_threads);
  } else {
    // No data for this phase.
    out->print_cr(" skipped");
  }
}

template <class T>
void WorkerDataArray<T>::print_details_on(outputStream* out) const {
  WDAPrinter::details(this, out);
}

template <typename T>
void WorkerDataArray<T>::reset() {
  set_all(uninitialized());
  for (uint i = 0; i < MaxThreadWorkItems; i++) {
    if (_thread_work_items[i] != NULL) {
      _thread_work_items[i]->reset();
    }
  }
}

#endif

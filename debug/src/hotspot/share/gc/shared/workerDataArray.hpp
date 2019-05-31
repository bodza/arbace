#ifndef SHARE_VM_GC_SHARED_WORKERDATAARRAY_HPP
#define SHARE_VM_GC_SHARED_WORKERDATAARRAY_HPP

#include "memory/allocation.hpp"
#include "utilities/debug.hpp"

class outputStream;

template <class T>
class WorkerDataArray : public CHeapObj<mtGC> {
  friend class WDAPrinter;
public:
  static const uint MaxThreadWorkItems = 3;
private:
  T*          _data;
  uint        _length;
  const char* _title;

  WorkerDataArray<size_t>* _thread_work_items[MaxThreadWorkItems];

 public:
  WorkerDataArray(uint length, const char* title);
  ~WorkerDataArray();

  void link_thread_work_items(WorkerDataArray<size_t>* thread_work_items, uint index = 0);
  void set_thread_work_item(uint worker_i, size_t value, uint index = 0);
  void add_thread_work_item(uint worker_i, size_t value, uint index = 0);
  WorkerDataArray<size_t>* thread_work_items(uint index = 0) const {
    return _thread_work_items[index];
  }

  static T uninitialized();

  void set(uint worker_i, T value);
  T get(uint worker_i) const;

  void add(uint worker_i, T value);

  // The sum() and average() methods below consider uninitialized slots to be 0.
  double average() const;
  T sum() const;

  const char* title() const {
    return _title;
  }

  void reset();
  void set_all(T value);

 private:
  class WDAPrinter {
  public:
    static void summary(outputStream* out, double min, double avg, double max, double diff, double sum, bool print_sum);
    static void summary(outputStream* out, size_t min, double avg, size_t max, size_t diff, size_t sum, bool print_sum);

    static void details(const WorkerDataArray<double>* phase, outputStream* out);
    static void details(const WorkerDataArray<size_t>* phase, outputStream* out);
  };

 public:
  void print_summary_on(outputStream* out, bool print_sum = true) const;
  void print_details_on(outputStream* out) const;
};

#endif

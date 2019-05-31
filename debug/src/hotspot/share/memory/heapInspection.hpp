#ifndef SHARE_VM_MEMORY_HEAPINSPECTION_HPP
#define SHARE_VM_MEMORY_HEAPINSPECTION_HPP

#include "memory/allocation.hpp"
#include "oops/objArrayOop.hpp"
#include "oops/oop.hpp"
#include "oops/annotations.hpp"
#include "utilities/macros.hpp"

// These declarations are needed since the declaration of KlassInfoTable and
// KlassInfoClosure are guarded by #if INLCUDE_SERVICES
class KlassInfoTable;
class KlassInfoClosure;

class HeapInspection : public StackObj {
  bool _csv_format; // "comma separated values" format for spreadsheet.
  bool _print_help;
  bool _print_class_stats;
  const char* _columns;
 public:
  HeapInspection(bool csv_format, bool print_help,
                 bool print_class_stats, const char *columns) :
      _csv_format(csv_format), _print_help(print_help),
      _print_class_stats(print_class_stats), _columns(columns) { }
  void heap_inspection(outputStream* st) { };
  size_t populate_table(KlassInfoTable* cit, BoolObjectClosure* filter = NULL) { return 0; };
  static void find_instances_at_safepoint(Klass* k, GrowableArray<oop>* result) { };
 private:
  void iterate_over_heap(KlassInfoTable* cit, BoolObjectClosure* filter = NULL);
};

#endif

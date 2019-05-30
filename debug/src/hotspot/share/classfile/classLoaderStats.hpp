#ifndef SHARE_VM_CLASSFILE_CLASSLOADERSTATS_HPP
#define SHARE_VM_CLASSFILE_CLASSLOADERSTATS_HPP

#include "classfile/classLoaderData.hpp"
#include "oops/klass.hpp"
#include "oops/oopsHierarchy.hpp"
#include "runtime/vm_operations.hpp"
#include "services/diagnosticCommand.hpp"
#include "utilities/resourceHash.hpp"

class ClassLoaderStatsDCmd : public DCmd {
public:
  ClassLoaderStatsDCmd(outputStream* output, bool heap) :
    DCmd(output, heap) {
  }

  static const char* name() {
    return "VM.classloader_stats";
  }

  static const char* description() {
    return "Print statistics about all ClassLoaders.";
  }

  static const char* impact() {
    return "Low";
  }

  virtual void execute(DCmdSource source, TRAPS);

  static int num_arguments() {
    return 0;
  }

  static const JavaPermission permission() {
    JavaPermission p = {"java.lang.management.ManagementPermission",
                        "monitor", NULL};
    return p;
  }
};

class ClassLoaderStats : public ResourceObj {
public:
  ClassLoaderData*  _cld;
  oop               _class_loader;
  oop               _parent;

  size_t            _chunk_sz;
  size_t            _block_sz;
  uintx             _classes_count;

  size_t            _anon_chunk_sz;
  size_t            _anon_block_sz;
  uintx             _anon_classes_count;

  ClassLoaderStats() :
    _cld(0),
    _class_loader(0),
    _parent(0),
    _chunk_sz(0),
    _block_sz(0),
    _classes_count(0),
    _anon_block_sz(0),
    _anon_chunk_sz(0),
    _anon_classes_count(0) {
  }
};

class ClassLoaderStatsClosure : public CLDClosure {
protected:
  static bool oop_equals(oop const& s1, oop const& s2) {
    return s1 == s2;
  }

  static unsigned oop_hash(oop const& s1) {
    unsigned hash = (unsigned)((uintptr_t)&s1);
    return hash ^ (hash >> LogMinObjAlignment);
  }

  typedef ResourceHashtable<oop, ClassLoaderStats*,
      ClassLoaderStatsClosure::oop_hash, ClassLoaderStatsClosure::oop_equals> StatsTable;

  outputStream* _out;
  StatsTable* _stats;
  uintx   _total_loaders;
  uintx   _total_classes;
  size_t  _total_chunk_sz;
  size_t  _total_block_sz;

public:
  ClassLoaderStatsClosure(outputStream* out) :
    _out(out),
    _total_loaders(0),
    _total_block_sz(0),
    _total_chunk_sz(0),
    _total_classes(0),
    _stats(new StatsTable()) {
  }

  virtual void do_cld(ClassLoaderData* cld);
  virtual bool do_entry(oop const& key, ClassLoaderStats* const& cls);
  void print();

private:
  void addEmptyParents(oop cl);
};

class ClassLoaderStatsVMOperation : public VM_Operation {
  outputStream* _out;

public:
  ClassLoaderStatsVMOperation(outputStream* out) :
    _out(out) {
  }

  VMOp_Type type() const {
    return VMOp_ClassLoaderStatsOperation;
  }

  void doit();
};

#endif

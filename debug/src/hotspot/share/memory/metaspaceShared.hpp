#ifndef SHARE_VM_MEMORY_METASPACESHARED_HPP
#define SHARE_VM_MEMORY_METASPACESHARED_HPP

#include "classfile/compactHashtable.hpp"
#include "memory/allocation.hpp"
#include "memory/memRegion.hpp"
#include "memory/virtualspace.hpp"
#include "oops/oop.hpp"
#include "utilities/exceptions.hpp"
#include "utilities/macros.hpp"
#include "utilities/resourceHash.hpp"

#define MAX_SHARED_DELTA                (0x7FFFFFFF)

class FileMapInfo;

class MetaspaceSharedStats {
public:
  MetaspaceSharedStats() {
    memset(this, 0, sizeof(*this));
  }
  CompactHashtableStats symbol;
  CompactHashtableStats string;
};

// Class Data Sharing Support
class MetaspaceShared : AllStatic {

  // CDS support
  static ReservedSpace _shared_rs;
  static VirtualSpace _shared_vs;
  static int _max_alignment;
  static MetaspaceSharedStats _stats;
  static bool _has_error_classes;
  static bool _archive_loading_failed;
  static bool _remapped_readwrite;
  static bool _open_archive_heap_region_mapped;
  static address _cds_i2i_entry_code_buffers;
  static size_t  _cds_i2i_entry_code_buffers_size;
  static size_t  _core_spaces_size;
 public:
  enum {
    // core archive spaces
    mc = 0,  // miscellaneous code for method trampolines
    rw = 1,  // read-write shared space in the heap
    ro = 2,  // read-only shared space in the heap
    md = 3,  // miscellaneous data for initializing tables, etc.
    num_core_spaces = 4, // number of non-string regions

    // optional mapped spaces
    // Currently it only contains class file data.
    od = num_core_spaces,
    num_non_heap_spaces = od + 1,

    // mapped java heap regions
    first_string = od + 1, // index of first string region
    max_strings = 2, // max number of string regions in string space
    first_open_archive_heap_region = first_string + max_strings,
    max_open_archive_heap_region = 2,

    last_valid_region = first_open_archive_heap_region + max_open_archive_heap_region - 1,
    n_regions =  last_valid_region + 1 // total number of regions
  };

  static void prepare_for_dumping() { };
  static void preload_and_dump(TRAPS) { };
  static int preload_classes(const char * class_list_path, TRAPS) { return 0; };
  static bool is_archive_object(oop p) { return false; };
  static bool is_heap_object_archiving_allowed() { return false; }
  static void create_archive_object_cache() { }
  static void destroy_archive_object_cache() { }
  static void fixup_mapped_heap_regions() { }
  static void dump_closed_archive_heap_objects(GrowableArray<MemRegion> * closed_archive) { };
  static void dump_open_archive_heap_objects(GrowableArray<MemRegion> * open_archive) { };
  static void set_open_archive_heap_region_mapped() { }
  static bool open_archive_heap_region_mapped() { return false; }
  static ReservedSpace* shared_rs() { return NULL; }
  static void commit_shared_space_to(char* newtop) { };
  static size_t core_spaces_size() { return _core_spaces_size; }
  static void initialize_dumptime_shared_and_meta_spaces() { };
  static void initialize_runtime_shared_and_meta_spaces() { };
  static void post_initialize(TRAPS) { };

  // Delta of this object from the bottom of the archive.
  static uintx object_delta(void* obj) {
    address base_address = address(shared_rs()->base());
    uintx delta = address(obj) - base_address;
    return delta;
  }

  static void set_archive_loading_failed() {
    _archive_loading_failed = true;
  }
  static bool map_shared_spaces(FileMapInfo* mapinfo) { return false; };
  static void initialize_shared_spaces() { };

  // Return true if given address is in the shared metaspace regions (i.e., excluding any
  // mapped shared heap regions.)
  static bool is_in_shared_metaspace(const void* p) {
    // If no shared metaspace regions are mapped, MetaspceObj::_shared_metaspace_{base,top} will
    // both be NULL and all values of p will be rejected quickly.
    return (p < MetaspaceObj::_shared_metaspace_top && p >= MetaspaceObj::_shared_metaspace_base);
  }

  // Return true if given address is in the shared region corresponding to the idx
  static bool is_in_shared_region(const void* p, int idx) { return false; };

  static bool is_heap_region(int idx) {
    { return false; };
  }
  static bool is_string_region(int idx) {
    { return false; };
  }
  static bool is_open_archive_heap_region(int idx) {
    { return false; };
  }
  static bool is_in_trampoline_frame(address addr) { return false; };

  static void allocate_cpp_vtable_clones();
  static intptr_t* clone_cpp_vtables(intptr_t* p);
  static void zero_cpp_vtable_clones_for_writing();
  static void patch_cpp_vtable_pointers();
  static bool is_valid_shared_method(const Method* m) { return false; };
  static void serialize(SerializeClosure* sc) { };
  static void serialize_well_known_classes(SerializeClosure* soc) { };

  static MetaspaceSharedStats* stats() {
    return &_stats;
  }

  static void report_out_of_space(const char* name, size_t needed_bytes);

  // JVM/TI RedefineClasses() support:
  // Remap the shared readonly space to shared readwrite, private if
  // sharing is enabled. Simply returns true if sharing is not enabled
  // or if the remapping has already been done by a prior call.
  static bool remap_shared_readonly_as_readwrite() { return true; };
  static bool remapped_readwrite() {
    return false;
  }

  static void print_shared_spaces();

  static bool try_link_class(InstanceKlass* ik, TRAPS);
  static void link_and_cleanup_shared_classes(TRAPS);
  static void check_shared_class_loader_type(InstanceKlass* ik);

  // Allocate a block of memory from the "mc", "ro", or "rw" regions.
  static char* misc_code_space_alloc(size_t num_bytes);
  static char* read_only_space_alloc(size_t num_bytes);

  template <typename T>
  static Array<T>* new_ro_array(int length) {
    return NULL;
  }

  static address cds_i2i_entry_code_buffers(size_t total_size);

  static address cds_i2i_entry_code_buffers() {
    return _cds_i2i_entry_code_buffers;
  }
  static size_t cds_i2i_entry_code_buffers_size() {
    return _cds_i2i_entry_code_buffers_size;
  }
  static void relocate_klass_ptr(oop o);

  static Klass* get_relocated_klass(Klass *k);

private:
  static void read_extra_data(const char* filename, TRAPS) { };
};
#endif

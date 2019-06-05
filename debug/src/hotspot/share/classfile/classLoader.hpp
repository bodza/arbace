#ifndef SHARE_VM_CLASSFILE_CLASSLOADER_HPP
#define SHARE_VM_CLASSFILE_CLASSLOADER_HPP

#include "runtime/handles.hpp"
#include "utilities/exceptions.hpp"
#include "utilities/macros.hpp"

// The VM class loader.
#include <sys/stat.h>

// Name of boot "modules" image
#define  MODULES_IMAGE_NAME "modules"

// Class path entry (directory or zip file)

class ClassFileStream;
class PackageEntry;
template <typename T> class GrowableArray;

class ClassPathEntry : public CHeapObj<mtClass> {
private:
  ClassPathEntry* volatile _next;
public:
  ClassPathEntry* next() const;
  virtual ~ClassPathEntry() { }
  void set_next(ClassPathEntry* next);
  virtual bool is_modules_image() const = 0;
  virtual bool is_jar_file() const = 0;
  virtual const char* name() const = 0;
  // Constructor
  ClassPathEntry() : _next(NULL) { }
  // Attempt to locate file_name through this class path entry.
  // Returns a class file parsing stream if successfull.
  virtual ClassFileStream* open_stream(const char* name, TRAPS) = 0;
};

class ClassPathDirEntry: public ClassPathEntry {
 private:
  const char* _dir;           // Name of directory
 public:
  bool is_modules_image() const { return false; }
  bool is_jar_file() const { return false; }
  const char* name() const { return _dir; }
  ClassPathDirEntry(const char* dir);
  virtual ~ClassPathDirEntry() { }
  ClassFileStream* open_stream(const char* name, TRAPS);
};

// Type definitions for zip file and zip file entry
typedef void* jzfile;
typedef struct {
  char *name;                   /* entry name */
  jlong time;                   /* modification time */
  jlong size;                   /* size of uncompressed data */
  jlong csize;                  /* size of compressed data (zero if uncompressed) */
  jint crc;                     /* crc of uncompressed data */
  char *comment;                /* optional zip file comment */
  jbyte *extra;                 /* optional extra data */
  jlong pos;                    /* position of LOC header (if negative) or data */
} jzentry;

class ClassPathZipEntry: public ClassPathEntry {
 enum {
   _unknown = 0,
   _yes     = 1,
   _no      = 2
 };
 private:
  jzfile* _zip;              // The zip archive
  const char*   _zip_name;   // Name of zip archive
  bool _is_boot_append;      // entry coming from -Xbootclasspath/a
  u1 _multi_versioned;       // indicates if the jar file has multi-versioned entries.
                             // It can have value of "_unknown", "_yes", or "_no"
 public:
  bool is_modules_image() const { return false; }
  bool is_jar_file() const { return true; }
  const char* name() const { return _zip_name; }
  ClassPathZipEntry(jzfile* zip, const char* zip_name, bool is_boot_append);
  virtual ~ClassPathZipEntry();
  u1* open_entry(const char* name, jint* filesize, bool nul_terminate, TRAPS);
  u1* open_versioned_entry(const char* name, jint* filesize, TRAPS) { return NULL; };
  ClassFileStream* open_stream(const char* name, TRAPS);
  void contents_do(void f(const char* name, void* context), void* context);
  bool is_multiple_versioned(TRAPS) { return false; };
};

// ModuleClassPathList contains a linked list of ClassPathEntry's
// that have been specified for a specific module.  Currently,
// the only way to specify a module/path pair is via the --patch-module
// command line option.
class ModuleClassPathList : public CHeapObj<mtClass> {
private:
  Symbol* _module_name;
  // First and last entries of class path entries for a specific module
  ClassPathEntry* _module_first_entry;
  ClassPathEntry* _module_last_entry;

public:
  Symbol* module_name() const { return _module_name; }
  ClassPathEntry* module_first_entry() const { return _module_first_entry; }
  ModuleClassPathList(Symbol* module_name);
  ~ModuleClassPathList();
  void add_to_list(ClassPathEntry* new_entry);
};

class ClassLoader: AllStatic {
 public:
  enum ClassLoaderType {
    BOOT_LOADER = 1,      /* boot loader */
    PLATFORM_LOADER  = 2, /* PlatformClassLoader */
    APP_LOADER  = 3       /* AppClassLoader */
  };

 protected:
  // The boot class path consists of 3 ordered pieces:
  //  1. the module/path pairs specified to --patch-module
  //    --patch-module=<module>=<file>(<pathsep><file>)*
  //  2. the base piece
  //    [jimage | build with exploded modules]
  //  3. boot loader append path
  //    [-Xbootclasspath/a]; [jvmti appended entries]
  //
  // The boot loader must obey this order when attempting
  // to load a class.

  // 1. Contains the module/path pairs specified to --patch-module
  static GrowableArray<ModuleClassPathList*>* _patch_mod_entries;

  // 2. the base piece
  //    Contains the ClassPathEntry of the modular java runtime image.
  //    If no java runtime image is present, this indicates a
  //    build with exploded modules is being used instead.
  static ClassPathEntry* _jrt_entry;
  static GrowableArray<ModuleClassPathList*>* _exploded_entries;
  enum { EXPLODED_ENTRY_SIZE = 80 }; // Initial number of exploded modules

  // 3. the boot loader's append path
  //    [-Xbootclasspath/a]; [jvmti appended entries]
  //    Note: boot loader append path does not support named modules.
  static ClassPathEntry* _first_append_entry;
  // Last entry in linked list of appended ClassPathEntry instances
  static ClassPathEntry* _last_append_entry;

  // Info used by CDS

  static void add_to_app_classpath_entries(const char* path, ClassPathEntry* entry, bool check_for_duplicates);

 protected:
  // Initialization:
  //   - setup the boot loader's system class path
  //   - setup the boot loader's patch mod entries, if present
  //   - create the ModuleEntry for java.base
  static void setup_bootstrap_search_path();
  static void setup_boot_search_path(const char *class_path);
  static void setup_patch_mod_entries();
  static void create_javabase();

  static void load_zip_library();
  static ClassPathEntry* create_class_path_entry(const char *path, const struct stat* st, bool throw_exception, bool is_boot_append, TRAPS);

 public:
  // If the package for the fully qualified class name is in the boot
  // loader's package entry table then add_package() sets the classpath_index
  // field so that get_system_package() will know to return a non-null value
  // for the package's location.  And, so that the package will be added to
  // the list of packages returned by get_system_packages().
  // For packages whose classes are loaded from the boot loader class path, the
  // classpath_index indicates which entry on the boot loader class path.
  static bool add_package(const char *fullq_class_name, s2 classpath_index, TRAPS);

  // Canonicalizes path names, so strcmp will work properly. This is mainly
  // to avoid confusing the zip library
  static bool get_canonical_path(const char* orig, char* out, int len);
  static const char* file_name_for_class_name(const char* class_name, int class_name_len);
  static PackageEntry* get_package_entry(const char* class_name, ClassLoaderData* loader_data, TRAPS);

 public:
  static jboolean decompress(void *in, u8 inSize, void *out, u8 outSize, char **pmsg);
  static int crc32(int crc, const char* buf, int len);
  static bool update_class_path_entry_list(const char *path, bool check_for_duplicates, bool is_boot_append, bool throw_exception=true);
  static void print_bootclasspath();

  // Modular java runtime image is present vs. a build with exploded modules
  static bool has_jrt_entry() { return (_jrt_entry != NULL); }
  static ClassPathEntry* get_jrt_entry() { return _jrt_entry; }

  // Add a module's exploded directory to the boot loader's exploded module build list
  static void add_to_exploded_build_list(Symbol* module_name, TRAPS);

  // Attempt load of individual class from either the patched or exploded modules build lists
  static ClassFileStream* search_module_entries(const GrowableArray<ModuleClassPathList*>* const module_list, const char* const class_name, const char* const file_name, TRAPS);

  // Load individual .class file
  static InstanceKlass* load_class(Symbol* class_name, bool search_append_only, TRAPS);

  // If the specified package has been loaded by the system, then returns
  // the name of the directory or ZIP file that the package was loaded from.
  // Returns null if the package was not loaded.
  // Note: The specified name can either be the name of a class or package.
  // If a package name is specified, then it must be "/"-separator and also
  // end with a trailing "/".
  static oop get_system_package(const char* name, TRAPS);

  // Returns an array of Java strings representing all of the currently
  // loaded system packages.
  // Note: The package names returned are "/"-separated and end with a
  // trailing "/".
  static objArrayOop get_system_packages(TRAPS);

  // Initialization
  static void initialize();
  static void classLoader_init2(TRAPS);

  static int compute_Object_vtable();

  static ClassPathEntry* classpath_entry(int n);

  static bool is_in_patch_mod_entries(Symbol* module_name);

  // indicates if class path already contains a entry (exact match by name)
  static bool contains_append_entry(const char* name);

  // adds a class path to the boot append entries
  static void add_to_boot_append_entries(ClassPathEntry* new_entry);

  // creates a class path zip entry (returns NULL if JAR file cannot be opened)
  static ClassPathZipEntry* create_class_path_zip_entry(const char *apath, bool is_boot_append);

  static bool string_ends_with(const char* str, const char* str_to_find);

  // obtain package name from a fully qualified class name
  // *bad_class_name is set to true if there's a problem with parsing class_name, to
  // distinguish from a class_name with no package name, as both cases have a NULL return value
  static const char* package_from_name(const char* const class_name, bool* bad_class_name = NULL);

  static bool is_modules_image(const char* name) { return string_ends_with(name, MODULES_IMAGE_NAME); }
};

#endif

#include "precompiled.hpp"

#include "classfile/classLoader.hpp"
#include "classfile/sharedPathsMiscInfo.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/filemap.hpp"
#include "memory/metaspaceShared.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/arguments.hpp"
#include "runtime/os.inline.hpp"
#include "utilities/ostream.hpp"

SharedPathsMiscInfo::SharedPathsMiscInfo() {
  _app_offset = 0;
  _buf_size = INITIAL_BUF_SIZE;
  _cur_ptr = _buf_start = NEW_C_HEAP_ARRAY(char, _buf_size, mtClass);
  _allocated = true;
}

SharedPathsMiscInfo::~SharedPathsMiscInfo() {
  if (_allocated) {
    FREE_C_HEAP_ARRAY(char, _buf_start);
  }
}

void SharedPathsMiscInfo::add_path(const char* path, int type) {
  ClassLoader::trace_class_path("add misc shared path ", path);
  write(path, strlen(path) + 1);
  write_jint(jint(type));
}

void SharedPathsMiscInfo::ensure_size(size_t needed_bytes) {
  int used = get_used_bytes();
  int target = used + int(needed_bytes);
  if (target > _buf_size) {
    _buf_size = _buf_size * 2 + (int)needed_bytes;
    _buf_start = REALLOC_C_HEAP_ARRAY(char, _buf_start, _buf_size, mtClass);
    _cur_ptr = _buf_start + used;
    _end_ptr = _buf_start + _buf_size;
  }
}

void SharedPathsMiscInfo::write(const void* ptr, size_t size) {
  ensure_size(size);
  memcpy(_cur_ptr, ptr, size);
  _cur_ptr += size;
}

bool SharedPathsMiscInfo::read(void* ptr, size_t size) {
  if (_cur_ptr + size <= _end_ptr) {
    memcpy(ptr, _cur_ptr, size);
    _cur_ptr += size;
    return true;
  }
  return false;
}

bool SharedPathsMiscInfo::fail(const char* msg, const char* name) {
  ClassLoader::trace_class_path(msg, name);
  MetaspaceShared::set_archive_loading_failed();
  return false;
}

void SharedPathsMiscInfo::print_path(outputStream* out, int type, const char* path) {
  switch (type) {
  case BOOT_PATH:
    out->print("Expecting BOOT path=%s", path);
    break;
  case NON_EXIST:
    out->print("Expecting that %s does not exist", path);
    break;
  case APP_PATH:
    ClassLoader::trace_class_path("Expecting -Djava.class.path=", path);
    break;
  default:
    ShouldNotReachHere();
  }
}

bool SharedPathsMiscInfo::check() {
  // The whole buffer must be 0 terminated so that we can use strlen and strcmp
  // without fear.
  _end_ptr -= sizeof(jint);
  if (_cur_ptr >= _end_ptr) {
    return fail("Truncated archive file header");
  }
  if (*_end_ptr != 0) {
    return fail("Corrupted archive file header");
  }

  while (_cur_ptr < _end_ptr) {
    jint type;
    const char* path = _cur_ptr;
    _cur_ptr += strlen(path) + 1;
    if (!read_jint(&type)) {
      return fail("Corrupted archive file header");
    }
    if (!check(type, path)) {
      return false;
    } else {
      ClassLoader::trace_class_path("ok");
    }
  }

  return true;
}

char* skip_first_path_entry(const char* path) {
  size_t path_sep_len = strlen(os::path_separator());
  char* p = strstr((char*)path, os::path_separator());
  if (p != NULL) {
    p += path_sep_len;
  }
  return p;
}

bool SharedPathsMiscInfo::check(jint type, const char* path) {
  switch (type) {
  case BOOT_PATH:
    {
      //
      // - Archive contains boot classes only - relaxed boot path check:
      //   Extra path elements appended to the boot path at runtime are allowed.
      //
      // - Archive contains application or platform classes - strict boot path check:
      //   Validate the entire runtime boot path, which must be compactible
      //   with the dump time boot path. Appending boot path at runtime is not
      //   allowed.
      //

      // The first entry in boot path is the modules_image (guaranteed by
      // ClassLoader::setup_boot_search_path()). Skip the first entry. The
      // path of the runtime modules_image may be different from the dump
      // time path (e.g. the JDK image is copied to a different location
      // after generating the shared archive), which is acceptable. For most
      // common cases, the dump time boot path might contain modules_image only.
      char* runtime_boot_path = Arguments::get_sysclasspath();
      char* rp = skip_first_path_entry(runtime_boot_path);
      char* dp = skip_first_path_entry(path);

      bool relaxed_check = !FileMapInfo::current_info()->header()->has_platform_or_app_classes();
      if (dp == NULL && rp == NULL) {
        break;   // ok, both runtime and dump time boot paths have modules_images only
      } else if (dp == NULL && rp != NULL && relaxed_check) {
        break;   // ok, relaxed check, runtime has extra boot append path entries
      } else if (dp != NULL && rp != NULL) {
        size_t num;
        size_t dp_len = strlen(dp);
        size_t rp_len = strlen(rp);
        if (rp_len >= dp_len) {
          if (relaxed_check) {
            // only check the leading entries in the runtime boot path, up to
            // the length of the dump time boot path
            num = dp_len;
          } else {
            // check the full runtime boot path, must match with dump time
            num = rp_len;
          }

          if (os::file_name_strncmp(dp, rp, num) == 0) {
            // make sure it is the end of an entry in the runtime boot path
            if (rp[dp_len] == '\0' || rp[dp_len] == os::path_separator()[0]) {
              break; // ok, runtime and dump time paths match
            }
          }
        }
      }

      // The paths are different
      return fail("[BOOT classpath mismatch, actual =", runtime_boot_path);
    }
    break;
  case NON_EXIST:
    {
      struct stat st;
      if (os::stat(path, &st) == 0) {
        // The file actually exists
        // But we want it to not exist -> fail
        return fail("File must not exist");
      }
    }
    break;
  case APP_PATH:
    {
      size_t len = strlen(path);
      const char *appcp = Arguments::get_appclasspath();
      size_t appcp_len = strlen(appcp);
      if (appcp_len < len) {
        return fail("Run time APP classpath is shorter than the one at dump time: ", appcp);
      }
      // Prefix is OK: E.g., dump with -cp foo.jar, but run with -cp foo.jar:bar.jar.
      if (os::file_name_strncmp(path, appcp, len) != 0) {
        return fail("[APP classpath mismatch, actual: -Djava.class.path=", appcp);
      }
      if (appcp[len] != '\0' && appcp[len] != os::path_separator()[0]) {
        return fail("Dump time APP classpath is not a proper prefix of run time APP classpath: ", appcp);
      }
    }
    break;
  default:
    return fail("Corrupted archive file header");
  }

  return true;
}

#ifndef OS_LINUX_VM_OS_LINUX_INLINE_HPP
#define OS_LINUX_VM_OS_LINUX_INLINE_HPP

#include "runtime/os.hpp"

// System includes

#include <unistd.h>
#include <sys/socket.h>
#include <poll.h>
#include <netdb.h>

// File names are case-insensitive on windows only
inline int os::file_name_strncmp(const char* s1, const char* s2, size_t num) {
  return strncmp(s1, s2, num);
}

inline bool os::obsolete_option(const JavaVMOption *option) {
  return false;
}

inline bool os::uses_stack_guard_pages() {
  return true;
}

inline bool os::must_commit_stack_guard_pages() {
  return true;
}

// On Linux, reservations are made on a page by page basis, nothing to do.
inline void os::pd_split_reserved_memory(char *base, size_t size, size_t split, bool realloc) { }

// Bang the shadow pages if they need to be touched to be mapped.
inline void os::map_stack_shadow_pages(address sp) { }

inline void os::dll_unload(void *lib) {
  ::dlclose(lib);
}

inline const int os::default_file_open_flags() { return 0; }

inline DIR* os::opendir(const char* dirname) {
  return ::opendir(dirname);
}

inline int os::readdir_buf_size(const char *path) {
  return NAME_MAX + sizeof(dirent) + 1;
}

inline jlong os::lseek(int fd, jlong offset, int whence) {
  return (jlong) ::lseek64(fd, offset, whence);
}

inline int os::fsync(int fd) {
  return ::fsync(fd);
}

inline int os::ftruncate(int fd, jlong length) {
  return ::ftruncate64(fd, length);
}

inline struct dirent* os::readdir(DIR* dirp, dirent *dbuf) {
  return ::readdir(dirp);
}

inline int os::closedir(DIR *dirp) {
  return ::closedir(dirp);
}

// macros for restartable system calls

#define RESTARTABLE(_cmd, _result) do { \
    _result = _cmd; \
  } while (((int)_result == OS_ERR) && (errno == EINTR))

#define RESTARTABLE_RETURN_INT(_cmd) do { \
  int _result; \
  RESTARTABLE(_cmd, _result); \
  return _result; \
} while (false)

inline bool os::numa_has_static_binding() { return true; }
inline bool os::numa_has_group_homing() { return false; }

inline size_t os::restartable_read(int fd, void *buf, unsigned int nBytes) {
  size_t res;
  RESTARTABLE( (size_t) ::read(fd, buf, (size_t) nBytes), res);
  return res;
}

inline size_t os::write(int fd, const void *buf, unsigned int nBytes) {
  size_t res;
  RESTARTABLE((size_t) ::write(fd, buf, (size_t) nBytes), res);
  return res;
}

inline int os::close(int fd) {
  return ::close(fd);
}

inline int os::socket_close(int fd) {
  return ::close(fd);
}

inline int os::socket(int domain, int type, int protocol) {
  return ::socket(domain, type, protocol);
}

inline int os::recv(int fd, char* buf, size_t nBytes, uint flags) {
  RESTARTABLE_RETURN_INT(::recv(fd, buf, nBytes, flags));
}

inline int os::send(int fd, char* buf, size_t nBytes, uint flags) {
  RESTARTABLE_RETURN_INT(::send(fd, buf, nBytes, flags));
}

inline int os::raw_send(int fd, char* buf, size_t nBytes, uint flags) {
  return os::send(fd, buf, nBytes, flags);
}

inline int os::connect(int fd, struct sockaddr* him, socklen_t len) {
  RESTARTABLE_RETURN_INT(::connect(fd, him, len));
}

inline struct hostent* os::get_host_by_name(char* name) {
  return ::gethostbyname(name);
}

inline bool os::supports_monotonic_clock() {
  return Linux::_clock_gettime != NULL;
}

inline void os::exit(int num) {
  ::exit(num);
}

#endif

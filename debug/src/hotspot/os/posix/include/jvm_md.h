#ifndef _JAVASOFT_JVM_MD_H_
#define _JAVASOFT_JVM_MD_H_

/*
 * This file is currently collecting system-specific dregs for the
 * JNI conversion, which should be sorted out later.
 */

#include <dirent.h>             /* For DIR */
#include <sys/param.h>          /* For MAXPATHLEN */
#include <unistd.h>             /* For F_OK, R_OK, W_OK */
#include <stddef.h>             /* For ptrdiff_t */
#include <stdint.h>             /* For uintptr_t */

#define JNI_ONLOAD_SYMBOLS   {"JNI_OnLoad"}
#define JNI_ONUNLOAD_SYMBOLS {"JNI_OnUnload"}
#define JVM_ONLOAD_SYMBOLS      {"JVM_OnLoad"}
#define AGENT_ONLOAD_SYMBOLS    {"Agent_OnLoad"}
#define AGENT_ONUNLOAD_SYMBOLS  {"Agent_OnUnload"}
#define AGENT_ONATTACH_SYMBOLS  {"Agent_OnAttach"}

#define JNI_LIB_PREFIX "lib"
#ifdef __APPLE__
#define JNI_LIB_SUFFIX ".dylib"
#define VERSIONED_JNI_LIB_NAME(NAME, VERSION) JNI_LIB_PREFIX NAME "." VERSION JNI_LIB_SUFFIX
#else
#define JNI_LIB_SUFFIX ".so"
#define VERSIONED_JNI_LIB_NAME(NAME, VERSION) JNI_LIB_PREFIX NAME JNI_LIB_SUFFIX "." VERSION
#endif
#define JNI_LIB_NAME(NAME) JNI_LIB_PREFIX NAME JNI_LIB_SUFFIX

// Hack: MAXPATHLEN is 4095 on some Linux and 4096 on others. This may
//       cause problems if JVM and the rest of JDK are built on different
//       Linux releases. Here we define JVM_MAXPATHLEN to be MAXPATHLEN + 1,
//       so buffers declared in VM are always >= 4096.
#define JVM_MAXPATHLEN MAXPATHLEN + 1

#define JVM_R_OK    R_OK
#define JVM_W_OK    W_OK
#define JVM_X_OK    X_OK
#define JVM_F_OK    F_OK

/*
 * File I/O
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>

/* Signals */

#include <sys/socket.h>   // for socklen_t

#define JVM_SIGINT     SIGINT
#define JVM_SIGTERM    SIGTERM

#define BREAK_SIGNAL     SIGQUIT           /* Thread dumping support.    */
#define SHUTDOWN1_SIGNAL SIGHUP            /* Shutdown Hooks support.    */
#define SHUTDOWN2_SIGNAL SIGINT
#define SHUTDOWN3_SIGNAL SIGTERM

/* With 1.4.1 libjsig added versioning: used in os_solaris.cpp and jsig.c */
#define JSIG_VERSION_1_4_1   0x30140100

#endif

#
# Setup flags for C/C++ compiler
#

#
# How to compile shared libraries.
#
AC_DEFUN([FLAGS_SETUP_SHARED_LIBS],
[
  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    C_FLAG_REORDER=''

    # Default works for linux, might work on other platforms as well.
    SHARED_LIBRARY_FLAGS='-shared'
    SET_EXECUTABLE_ORIGIN='-Wl,-rpath,\$$ORIGIN[$]1'
    SET_SHARED_LIBRARY_ORIGIN="-Wl,-z,origin $SET_EXECUTABLE_ORIGIN"
    SET_SHARED_LIBRARY_NAME='-Wl,-soname=[$]1'
    SET_SHARED_LIBRARY_MAPFILE='-Wl,-version-script=[$]1'

  elif test "x$TOOLCHAIN_TYPE" = xclang; then
    C_FLAG_REORDER=''

    if test "x$OPENJDK_TARGET_OS" = xmacosx; then
      # Linking is different on MacOSX
      SHARED_LIBRARY_FLAGS="-dynamiclib -compatibility_version 1.0.0 -current_version 1.0.0"
      SET_EXECUTABLE_ORIGIN='-Wl,-rpath,@loader_path$(or [$]1,/.)'
      SET_SHARED_LIBRARY_ORIGIN="$SET_EXECUTABLE_ORIGIN"
      SET_SHARED_LIBRARY_NAME='-Wl,-install_name,@rpath/[$]1'
      SET_SHARED_LIBRARY_MAPFILE='-Wl,-exported_symbols_list,[$]1'

    else
      # Default works for linux, might work on other platforms as well.
      SHARED_LIBRARY_FLAGS='-shared'
      SET_EXECUTABLE_ORIGIN='-Wl,-rpath,\$$ORIGIN[$]1'
      SET_SHARED_LIBRARY_NAME='-Wl,-soname=[$]1'
      SET_SHARED_LIBRARY_MAPFILE='-Wl,-version-script=[$]1'

      # arm specific settings
      if test "x$OPENJDK_TARGET_CPU" = "xarm"; then
        # '-Wl,-z,origin' isn't used on arm.
        SET_SHARED_LIBRARY_ORIGIN='-Wl,-rpath,\$$$$ORIGIN[$]1'
      else
        SET_SHARED_LIBRARY_ORIGIN="-Wl,-z,origin $SET_EXECUTABLE_ORIGIN"
      fi
    fi
  fi

  AC_SUBST(C_FLAG_REORDER)
  AC_SUBST(SET_EXECUTABLE_ORIGIN)
  AC_SUBST(SET_SHARED_LIBRARY_ORIGIN)
  AC_SUBST(SET_SHARED_LIBRARY_NAME)
  AC_SUBST(SET_SHARED_LIBRARY_MAPFILE)
  AC_SUBST(SHARED_LIBRARY_FLAGS)
])

AC_DEFUN([FLAGS_SETUP_DEBUG_SYMBOLS],
[
  # By default don't set any specific assembler debug
  # info flags for toolchains unless we know they work.
  # See JDK-8207057.
  ASFLAGS_DEBUG_SYMBOLS=""
  # Debug symbols
  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    CFLAGS_DEBUG_SYMBOLS="-g"
    ASFLAGS_DEBUG_SYMBOLS="-g"
  elif test "x$TOOLCHAIN_TYPE" = xclang; then
    CFLAGS_DEBUG_SYMBOLS="-g"
    ASFLAGS_DEBUG_SYMBOLS="-g"
  fi

  AC_SUBST(CFLAGS_DEBUG_SYMBOLS)
  AC_SUBST(ASFLAGS_DEBUG_SYMBOLS)
])

AC_DEFUN([FLAGS_SETUP_WARNINGS],
[
  AC_ARG_ENABLE([warnings-as-errors], [AS_HELP_STRING([--disable-warnings-as-errors],
      [do not consider native warnings to be an error @<:@enabled@:>@])])

  # Set default value.
  WARNINGS_AS_ERRORS=true

  AC_MSG_CHECKING([if native warnings are errors])
  if test "x$enable_warnings_as_errors" = "xyes"; then
    AC_MSG_RESULT([yes (explicitly set)])
    WARNINGS_AS_ERRORS=true
  elif test "x$enable_warnings_as_errors" = "xno"; then
    AC_MSG_RESULT([no (explicitly set)])
    WARNINGS_AS_ERRORS=false
  elif test "x$enable_warnings_as_errors" = "x"; then
    AC_MSG_RESULT([${WARNINGS_AS_ERRORS} (default)])
  else
    AC_MSG_ERROR([--enable-warnings-as-errors accepts no argument])
  fi

  AC_SUBST(WARNINGS_AS_ERRORS)

  case "${TOOLCHAIN_TYPE}" in
    gcc)
      DISABLE_WARNING_PREFIX="-Wno-"
      CFLAGS_WARNINGS_ARE_ERRORS="-Werror"
      # Repeate the check for the BUILD_CC and BUILD_CXX. Need to also reset
      # CFLAGS since any target specific flags will likely not work with the
      # build compiler
      CC_OLD="$CC"
      CXX_OLD="$CXX"
      CC="$BUILD_CC"
      CXX="$BUILD_CXX"
      CFLAGS_OLD="$CFLAGS"
      CFLAGS=""
      BUILD_CC_DISABLE_WARNING_PREFIX="-Wno-"
      CC="$CC_OLD"
      CXX="$CXX_OLD"
      CFLAGS="$CFLAGS_OLD"
      ;;
    clang)
      DISABLE_WARNING_PREFIX="-Wno-"
      CFLAGS_WARNINGS_ARE_ERRORS="" # "-Werror" oops!
      ;;
  esac
  AC_SUBST(DISABLE_WARNING_PREFIX)
  AC_SUBST(BUILD_CC_DISABLE_WARNING_PREFIX)
  AC_SUBST(CFLAGS_WARNINGS_ARE_ERRORS)
])

AC_DEFUN([FLAGS_SETUP_QUALITY_CHECKS],
[
  # bounds, memory and behavior checking options
  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    case $DEBUG_LEVEL in
    release )
      # no adjustment
      ;;
    fastdebug )
      # no adjustment
      ;;
    slowdebug )
      # FIXME: By adding this to C(XX)FLAGS_DEBUG_OPTIONS/JVM_CFLAGS_SYMBOLS it
      # get's added conditionally on whether we produce debug symbols or not.
      # This is most likely not really correct.

      # Add runtime stack smashing and undefined behavior checks.
      CFLAGS_DEBUG_OPTIONS="-fstack-protector-all --param ssp-buffer-size=1"
      CXXFLAGS_DEBUG_OPTIONS="-fstack-protector-all --param ssp-buffer-size=1"

      JVM_CFLAGS_SYMBOLS="$JVM_CFLAGS_SYMBOLS -fstack-protector-all --param ssp-buffer-size=1"
      ;;
    esac
  fi
])

AC_DEFUN([FLAGS_SETUP_OPTIMIZATION],
[
  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    C_O_FLAG_HIGHEST_JVM="-O3"
    C_O_FLAG_HIGHEST="-O3"
    C_O_FLAG_HI="-O3"
    C_O_FLAG_NORM="-O2"
    C_O_FLAG_SIZE="-Os"
    C_O_FLAG_DEBUG="-O0"
    C_O_FLAG_DEBUG_JVM="-O0"
    C_O_FLAG_NONE="-O0"
  elif test "x$TOOLCHAIN_TYPE" = xclang; then
    if test "x$OPENJDK_TARGET_OS" = xmacosx; then
      # On MacOSX we optimize for size, something
      # we should do for all platforms?
      C_O_FLAG_HIGHEST_JVM="-Os"
      C_O_FLAG_HIGHEST="-Os"
      C_O_FLAG_HI="-Os"
      C_O_FLAG_NORM="-Os"
      C_O_FLAG_DEBUG_JVM=""
    else
      C_O_FLAG_HIGHEST_JVM="-O3"
      C_O_FLAG_HIGHEST="-O3"
      C_O_FLAG_HI="-O3"
      C_O_FLAG_NORM="-O2"
      C_O_FLAG_DEBUG_JVM="-O0"
    fi
    C_O_FLAG_SIZE="-Os"
    C_O_FLAG_DEBUG="-O0"
    C_O_FLAG_NONE="-O0"
  fi

  # Now copy to C++ flags
  CXX_O_FLAG_HIGHEST_JVM="$C_O_FLAG_HIGHEST_JVM"
  CXX_O_FLAG_HIGHEST="$C_O_FLAG_HIGHEST"
  CXX_O_FLAG_HI="$C_O_FLAG_HI"
  CXX_O_FLAG_NORM="$C_O_FLAG_NORM"
  CXX_O_FLAG_DEBUG="$C_O_FLAG_DEBUG"
  CXX_O_FLAG_DEBUG_JVM="$C_O_FLAG_DEBUG_JVM"
  CXX_O_FLAG_NONE="$C_O_FLAG_NONE"
  CXX_O_FLAG_SIZE="$C_O_FLAG_SIZE"

  # Adjust optimization flags according to debug level.
  case $DEBUG_LEVEL in
    release )
      # no adjustment
      ;;
    fastdebug )
      # Not quite so much optimization
      C_O_FLAG_HI="$C_O_FLAG_NORM"
      CXX_O_FLAG_HI="$CXX_O_FLAG_NORM"
      ;;
    slowdebug )
      # Disable optimization
      C_O_FLAG_HIGHEST_JVM="$C_O_FLAG_DEBUG_JVM"
      C_O_FLAG_HIGHEST="$C_O_FLAG_DEBUG"
      C_O_FLAG_HI="$C_O_FLAG_DEBUG"
      C_O_FLAG_NORM="$C_O_FLAG_DEBUG"
      C_O_FLAG_SIZE="$C_O_FLAG_DEBUG"
      CXX_O_FLAG_HIGHEST_JVM="$CXX_O_FLAG_DEBUG_JVM"
      CXX_O_FLAG_HIGHEST="$CXX_O_FLAG_DEBUG"
      CXX_O_FLAG_HI="$CXX_O_FLAG_DEBUG"
      CXX_O_FLAG_NORM="$CXX_O_FLAG_DEBUG"
      CXX_O_FLAG_SIZE="$CXX_O_FLAG_DEBUG"
      ;;
  esac

  AC_SUBST(C_O_FLAG_HIGHEST_JVM)
  AC_SUBST(C_O_FLAG_HIGHEST)
  AC_SUBST(C_O_FLAG_HI)
  AC_SUBST(C_O_FLAG_NORM)
  AC_SUBST(C_O_FLAG_NONE)
  AC_SUBST(C_O_FLAG_SIZE)
  AC_SUBST(CXX_O_FLAG_HIGHEST_JVM)
  AC_SUBST(CXX_O_FLAG_HIGHEST)
  AC_SUBST(CXX_O_FLAG_HI)
  AC_SUBST(CXX_O_FLAG_NORM)
  AC_SUBST(CXX_O_FLAG_NONE)
  AC_SUBST(CXX_O_FLAG_SIZE)
])

AC_DEFUN([FLAGS_SETUP_CFLAGS],
[
  ### CFLAGS

  FLAGS_SETUP_CFLAGS_HELPER

  FLAGS_OS=$OPENJDK_TARGET_OS
  FLAGS_OS_TYPE=$OPENJDK_TARGET_OS_TYPE
  FLAGS_CPU=$OPENJDK_TARGET_CPU
  FLAGS_CPU_ARCH=$OPENJDK_TARGET_CPU_ARCH
  FLAGS_CPU_BITS=$OPENJDK_TARGET_CPU_BITS
  FLAGS_CPU_ENDIAN=$OPENJDK_TARGET_CPU_ENDIAN
  FLAGS_CPU_LEGACY=$OPENJDK_TARGET_CPU_LEGACY
  FLAGS_CPU_LEGACY_LIB=$OPENJDK_TARGET_CPU_LEGACY_LIB

  FLAGS_SETUP_CFLAGS_CPU_DEP([TARGET])

  FLAGS_OS=$OPENJDK_BUILD_OS
  FLAGS_OS_TYPE=$OPENJDK_BUILD_OS_TYPE
  FLAGS_CPU=$OPENJDK_BUILD_CPU
  FLAGS_CPU_ARCH=$OPENJDK_BUILD_CPU_ARCH
  FLAGS_CPU_BITS=$OPENJDK_BUILD_CPU_BITS
  FLAGS_CPU_ENDIAN=$OPENJDK_BUILD_CPU_ENDIAN
  FLAGS_CPU_LEGACY=$OPENJDK_BUILD_CPU_LEGACY
  FLAGS_CPU_LEGACY_LIB=$OPENJDK_BUILD_CPU_LEGACY_LIB

  FLAGS_SETUP_CFLAGS_CPU_DEP([BUILD], [OPENJDK_BUILD_])

  COMPILER_FP_CONTRACT_OFF_FLAG="-ffp-contract=off"
  # Check that the compiler supports -ffp-contract=off flag
  # Set FDLIBM_CFLAGS to -ffp-contract=off if it does. Empty
  # otherwise.
  # These flags are required for GCC-based builds of
  # fdlibm with optimization without losing precision.
  # Notably, -ffp-contract=off needs to be added for GCC >= 4.6.
  if test "x$TOOLCHAIN_TYPE" = xgcc || test "x$TOOLCHAIN_TYPE" = xclang; then
    FLAGS_COMPILER_CHECK_ARGUMENTS(ARGUMENT: [${COMPILER_FP_CONTRACT_OFF_FLAG}],
	IF_TRUE: [FDLIBM_CFLAGS=${COMPILER_FP_CONTRACT_OFF_FLAG}],
	IF_FALSE: [FDLIBM_CFLAGS=""])
  fi
  AC_SUBST(FDLIBM_CFLAGS)

  # Tests are only ever compiled for TARGET
  CFLAGS_TESTLIB="$CFLAGS_JDKLIB"
  CXXFLAGS_TESTLIB="$CXXFLAGS_JDKLIB"
  CFLAGS_TESTEXE="$CFLAGS_JDKEXE"
  CXXFLAGS_TESTEXE="$CXXFLAGS_JDKEXE"

  AC_SUBST(CFLAGS_TESTLIB)
  AC_SUBST(CFLAGS_TESTEXE)
  AC_SUBST(CXXFLAGS_TESTLIB)
  AC_SUBST(CXXFLAGS_TESTEXE)
])

################################################################################
# platform independent
AC_DEFUN([FLAGS_SETUP_CFLAGS_HELPER],
[
  #### OS DEFINES, these should be independent on toolchain
  if test "x$OPENJDK_TARGET_OS" = xlinux; then
    CFLAGS_OS_DEF_JVM="-DLINUX"
    CFLAGS_OS_DEF_JDK="-D_GNU_SOURCE -D_REENTRANT -D_LARGEFILE64_SOURCE"
  elif test "x$OPENJDK_TARGET_OS" = xsolaris; then
    CFLAGS_OS_DEF_JVM="-DSOLARIS"
    CFLAGS_OS_DEF_JDK="-D__solaris__"
  elif test "x$OPENJDK_TARGET_OS" = xmacosx; then
    CFLAGS_OS_DEF_JVM="-D_ALLBSD_SOURCE -D_DARWIN_C_SOURCE -D_XOPEN_SOURCE"
    CFLAGS_OS_DEF_JDK="-D_ALLBSD_SOURCE -D_DARWIN_UNLIMITED_SELECT"
  elif test "x$OPENJDK_TARGET_OS" = xaix; then
    CFLAGS_OS_DEF_JVM="-DAIX"
  elif test "x$OPENJDK_TARGET_OS" = xbsd; then
    CFLAGS_OS_DEF_JDK="-D_ALLBSD_SOURCE"
  elif test "x$OPENJDK_TARGET_OS" = xwindows; then
    CFLAGS_OS_DEF_JVM="-D_WINDOWS -DWIN32 -D_JNI_IMPLEMENTATION_"
  fi

  CFLAGS_OS_DEF_JDK="$CFLAGS_OS_DEF_JDK -D$OPENJDK_TARGET_OS_UPPERCASE"

  #### GLOBAL DEFINES
  # Set some common defines. These works for all compilers, but assume
  # -D is universally accepted.

  # Always enable optional macros for VM.
  ALWAYS_CFLAGS_JVM="-D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS"

  # Setup some hard coded includes
  ALWAYS_CFLAGS_JDK=" \
      -I\$(SUPPORT_OUTPUTDIR)/modules_include/java.base \
      -I\$(SUPPORT_OUTPUTDIR)/modules_include/java.base/\$(OPENJDK_TARGET_OS_INCLUDE_SUBDIR) \
      -I${TOPDIR}/src/java.base/share/native/libjava \
      -I${TOPDIR}/src/java.base/$OPENJDK_TARGET_OS_TYPE/native/libjava \
      -I${TOPDIR}/src/hotspot/share/include \
      -I${TOPDIR}/src/hotspot/os/${HOTSPOT_TARGET_OS_TYPE}/include"

  ###############################################################################

  # Adjust flags according to debug level.
  # Setup debug/release defines
  if test "x$DEBUG_LEVEL" = xrelease; then
    DEBUG_CFLAGS_JDK="-DNDEBUG"
    if test "x$OPENJDK_TARGET_OS" = xsolaris; then
      DEBUG_CFLAGS_JDK="$DEBUG_CFLAGS_JDK -DTRIMMED"
    fi
  else
    DEBUG_CFLAGS_JDK="-DDEBUG"
  fi

  if test "x$DEBUG_LEVEL" != xrelease; then
    DEBUG_OPTIONS_FLAGS_JDK="$CFLAGS_DEBUG_OPTIONS"
    DEBUG_SYMBOLS_CFLAGS_JDK="$CFLAGS_DEBUG_SYMBOLS"
  fi

  #### TOOLCHAIN DEFINES

  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    ALWAYS_DEFINES_JVM="-D_GNU_SOURCE -D_REENTRANT"
  elif test "x$TOOLCHAIN_TYPE" = xclang; then
    ALWAYS_DEFINES_JVM="-D_GNU_SOURCE"
  fi

  ###############################################################################
  #
  #
  # CFLAGS BASIC
  if test "x$TOOLCHAIN_TYPE" = xgcc || test "x$TOOLCHAIN_TYPE" = xclang; then
    # COMMON to gcc and clang
    TOOLCHAIN_CFLAGS_JVM="-pipe -fno-rtti -fno-exceptions -fvisibility=hidden -fno-strict-aliasing -fno-omit-frame-pointer -ferror-limit=100"
  fi

  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    TOOLCHAIN_CFLAGS_JVM="$TOOLCHAIN_CFLAGS_JVM -fcheck-new -fstack-protector"
    TOOLCHAIN_CFLAGS_JDK="-pipe -fstack-protector"
    TOOLCHAIN_CFLAGS_JDK_CONLY="-fno-strict-aliasing" # technically NOT for CXX (but since this gives *worse* performance, use no-strict-aliasing everywhere!)

    CXXSTD_CXXFLAG="-std=gnu++98"
    FLAGS_CXX_COMPILER_CHECK_ARGUMENTS(ARGUMENT: [$CXXSTD_CXXFLAG -Werror],
    						 IF_FALSE: [CXXSTD_CXXFLAG=""])
    TOOLCHAIN_CFLAGS_JDK_CXXONLY="$CXXSTD_CXXFLAG"
    TOOLCHAIN_CFLAGS_JVM="$TOOLCHAIN_CFLAGS_JVM $CXXSTD_CXXFLAG"
    ADLC_CXXFLAG="$CXXSTD_CXXFLAG"

  elif test "x$TOOLCHAIN_TYPE" = xclang; then
    # Restrict the debug information created by Clang to avoid
    # too big object files and speed the build up a little bit
    # (see http://llvm.org/bugs/show_bug.cgi?id=7554)
    TOOLCHAIN_CFLAGS_JVM="$TOOLCHAIN_CFLAGS_JVM -flimit-debug-info"

    # In principle the stack alignment below is cpu- and ABI-dependent and
    # should agree with values of StackAlignmentInBytes in various
    # src/hotspot/cpu/*/globalDefinitions_*.hpp files, but this value currently
    # works for all platforms.
    TOOLCHAIN_CFLAGS_JVM="$TOOLCHAIN_CFLAGS_JVM -mno-omit-leaf-frame-pointer -mstack-alignment=16"

    if test "x$OPENJDK_TARGET_OS" = xlinux; then
      TOOLCHAIN_CFLAGS_JDK="-pipe"
      TOOLCHAIN_CFLAGS_JDK_CONLY="-fno-strict-aliasing" # technically NOT for CXX
    fi
  fi

  # CFLAGS WARNINGS STUFF
  # Set JVM_CFLAGS warning handling
  if test "x$TOOLCHAIN_TYPE" = xgcc || test "x$TOOLCHAIN_TYPE" = xclang; then
    # COMMON to gcc and clang
    WARNING_CFLAGS_JVM="-Wpointer-arith -Wsign-compare -Wunused-function"
    if ! HOTSPOT_CHECK_JVM_VARIANT(zero); then
      # Non-zero builds have stricter warnings
      WARNING_CFLAGS_JVM="$WARNING_CFLAGS_JVM -Wundef -Wformat=2"
    fi

  fi
  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    WARNING_CFLAGS_JDK="-Wall -Wextra -Wno-unused -Wno-unused-parameter -Wformat=2"
    WARNING_CFLAGS_JVM="$WARNING_CFLAGS_JVM -Wunused-value -Woverloaded-virtual"

    if ! HOTSPOT_CHECK_JVM_VARIANT(zero); then
      # Non-zero builds have stricter warnings
      WARNING_CFLAGS_JVM="$WARNING_CFLAGS_JVM -Wreturn-type"
    fi
  elif test "x$TOOLCHAIN_TYPE" = xclang; then
    WARNING_CFLAGS_JVM="$WARNING_CFLAGS_JVM -Wno-deprecated"
    if test "x$OPENJDK_TARGET_OS" = xlinux; then
      WARNING_CFLAGS_JVM="$WARNING_CFLAGS_JVM -Wno-sometimes-uninitialized"
      WARNING_CFLAGS_JDK="-Wall -Wextra -Wno-unused -Wno-unused-parameter -Wformat=2"
    fi
  fi

  # Set some additional per-OS defines.

  # Additional macosx handling
  if test "x$OPENJDK_TARGET_OS" = xmacosx; then
    OS_CFLAGS="-DMAC_OS_X_VERSION_MIN_REQUIRED=$MACOSX_VERSION_MIN_NODOTS -mmacosx-version-min=$MACOSX_VERSION_MIN"

    if test -n "$MACOSX_VERSION_MAX"; then
        OS_CFLAGS="$OS_CFLAGS -DMAC_OS_X_VERSION_MAX_ALLOWED=$MACOSX_VERSION_MAX_NODOTS"
    fi
  fi

  # Where does this really belong??
  if test "x$TOOLCHAIN_TYPE" = xgcc || test "x$TOOLCHAIN_TYPE" = xclang; then
    PICFLAG="-fPIC"
  fi

  JVM_PICFLAG="$PICFLAG"
  JDK_PICFLAG="$PICFLAG"

  if test "x$OPENJDK_TARGET_OS" = xmacosx; then
    # Linking is different on MacOSX
    JDK_PICFLAG=''
    if test "x$STATIC_BUILD" = xtrue; then
      JVM_PICFLAG=""
    fi
  fi

  # Optional POSIX functionality needed by the JVM
  #
  # Check if clock_gettime is available and in which library. This indicates
  # availability of CLOCK_MONOTONIC for hotspot. But we don't need to link, so
  # don't let it update LIBS.
  save_LIBS="$LIBS"
  AC_SEARCH_LIBS(clock_gettime, rt, [HAS_CLOCK_GETTIME=true], [])
  if test "x$LIBS" = "x-lrt "; then
    CLOCK_GETTIME_IN_LIBRT=true
  fi
  LIBS="$save_LIBS"

  if test "x$HAS_CLOCK_GETTIME" = "xtrue"; then
    OS_CFLAGS_JVM="$OS_CFLAGS_JVM -DSUPPORTS_CLOCK_MONOTONIC"
    if test "x$CLOCK_GETTIME_IN_LIBRT" = "xtrue"; then
      OS_CFLAGS_JVM="$OS_CFLAGS_JVM -DNEEDS_LIBRT"
    fi
  fi

  # EXPORT
  AC_SUBST(ADLC_CXXFLAG)
])

################################################################################
# $1 - Either BUILD or TARGET to pick the correct OS/CPU variables to check
#      conditionals against.
# $2 - Optional prefix for each variable defined.
AC_DEFUN([FLAGS_SETUP_CFLAGS_CPU_DEP],
[
  #### CPU DEFINES, these should (in theory) be independent on toolchain

  # Setup target CPU
  # Setup endianness
  if test "x$FLAGS_CPU_ENDIAN" = xlittle; then
    $1_DEFINES_CPU_JVM="-DVM_LITTLE_ENDIAN"
  fi
  if test "x$FLAGS_CPU_ENDIAN" = xlittle; then
    $1_DEFINES_CPU_JDK="-D_LITTLE_ENDIAN"
  else
    $1_DEFINES_CPU_JDK="-D_BIG_ENDIAN"
  fi

  # setup CPU bit size
  $1_DEFINES_CPU_JDK="${$1_DEFINES_CPU_JDK} -DARCH='\"$FLAGS_CPU_LEGACY\"' \
      -D$FLAGS_CPU_LEGACY"

  if test "x$FLAGS_CPU_BITS" = x64; then
    # -D_LP64=1 is only set on linux and mac. Setting on windows causes diff in
    # unpack200.exe.
    if test "x$FLAGS_OS" = xlinux || test "x$FLAGS_OS" = xmacosx; then
      $1_DEFINES_CPU_JDK="${$1_DEFINES_CPU_JDK} -D_LP64=1"
    fi
    if test "x$FLAGS_OS" != xsolaris && test "x$FLAGS_OS" != xaix; then
      # Solaris does not have _LP64=1 in the old build.
      # xlc on AIX defines _LP64=1 by default and issues a warning if we redefine it.
      $1_DEFINES_CPU_JVM="${$1_DEFINES_CPU_JVM} -D_LP64=1"
    fi
  fi

  # CFLAGS PER CPU
  if test "x$TOOLCHAIN_TYPE" = xgcc || test "x$TOOLCHAIN_TYPE" = xclang; then
    # COMMON to gcc and clang
    if test "x$FLAGS_CPU" = xx86; then
      # Force compatibility with i586 on 32 bit intel platforms.
      $1_CFLAGS_CPU="-march=i586"
    fi
  fi

  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    if test "x$FLAGS_CPU" = xarm; then
      # -Wno-psabi to get rid of annoying "note: the mangling of 'va_list' has changed in GCC 4.4"
      $1_CFLAGS_CPU="-fsigned-char -Wno-psabi $ARM_ARCH_TYPE_FLAGS $ARM_FLOAT_TYPE_FLAGS -DJDK_ARCH_ABI_PROP_NAME='\"\$(JDK_ARCH_ABI_PROP_NAME)\"'"
      $1_CFLAGS_CPU_JVM="-DARM"
    elif test "x$FLAGS_CPU" = xaarch64; then
      if test "x$HOTSPOT_TARGET_CPU_PORT" = xarm64; then
        $1_CFLAGS_CPU_JVM="-fsigned-char -DARM"
      fi
    elif test "x$FLAGS_CPU_ARCH" = xppc; then
      $1_CFLAGS_CPU_JVM="-minsert-sched-nops=regroup_exact -mno-multiple -mno-string"
      if test "x$FLAGS_CPU" = xppc64; then
        # -mminimal-toc fixes `relocation truncated to fit' error for gcc 4.1.
        # Use ppc64 instructions, but schedule for power5
        $1_CFLAGS_CPU_JVM="${$1_CFLAGS_CPU_JVM} -mminimal-toc -mcpu=powerpc64 -mtune=power5"
      elif test "x$FLAGS_CPU" = xppc64le; then
        # Little endian machine uses ELFv2 ABI.
        # Use Power8, this is the first CPU to support PPC64 LE with ELFv2 ABI.
        $1_CFLAGS_CPU_JVM="${$1_CFLAGS_CPU_JVM} -DABI_ELFv2 -mcpu=power8 -mtune=power8"
      fi
    elif test "x$FLAGS_CPU" = xs390x; then
      $1_CFLAGS_CPU="-mbackchain -march=z10"
    fi

    if test "x$FLAGS_CPU_ARCH" != xarm &&  test "x$FLAGS_CPU_ARCH" != xppc; then
      # for all archs except arm and ppc, prevent gcc to omit frame pointer
      $1_CFLAGS_CPU_JDK="${$1_CFLAGS_CPU_JDK} -fno-omit-frame-pointer"
    fi

  elif test "x$TOOLCHAIN_TYPE" = xclang; then
    if test "x$FLAGS_OS" = xlinux; then
      # ppc test not really needed for clang
      if test "x$FLAGS_CPU_ARCH" != xarm &&  test "x$FLAGS_CPU_ARCH" != xppc; then
        # for all archs except arm and ppc, prevent gcc to omit frame pointer
        $1_CFLAGS_CPU_JDK="${$1_CFLAGS_CPU_JDK} -fno-omit-frame-pointer"
      fi
    fi
  fi

  if test "x$TOOLCHAIN_TYPE" = xgcc; then
    TOOLCHAIN_CHECK_COMPILER_VERSION(VERSION: 6, PREFIX: $2, IF_AT_LEAST: FLAGS_SETUP_GCC6_COMPILER_FLAGS($1))
    $1_TOOLCHAIN_CFLAGS="${$1_GCC6_CFLAGS}"

    $1_WARNING_CFLAGS_JVM="-Wno-format-zero-length -Wtype-limits -Wuninitialized"
  fi

  # EXPORT to API
  CFLAGS_JVM_COMMON="$ALWAYS_CFLAGS_JVM $ALWAYS_DEFINES_JVM $TOOLCHAIN_CFLAGS_JVM \
      $OS_CFLAGS $OS_CFLAGS_JVM $CFLAGS_OS_DEF_JVM $DEBUG_CFLAGS_JVM \
      $WARNING_CFLAGS $WARNING_CFLAGS_JVM $JVM_PICFLAG"

  CFLAGS_JDK_COMMON="$ALWAYS_CFLAGS_JDK $ALWAYS_DEFINES_JDK $TOOLCHAIN_CFLAGS_JDK \
      $OS_CFLAGS $CFLAGS_OS_DEF_JDK $DEBUG_CFLAGS_JDK $DEBUG_OPTIONS_FLAGS_JDK \
      $WARNING_CFLAGS $WARNING_CFLAGS_JDK $DEBUG_SYMBOLS_CFLAGS_JDK"

  # Use ${$2EXTRA_CFLAGS} to block EXTRA_CFLAGS to be added to build flags.
  # (Currently we don't have any OPENJDK_BUILD_EXTRA_CFLAGS, but that might
  # change in the future.)

  CFLAGS_JDK_COMMON_CONLY="$TOOLCHAIN_CFLAGS_JDK_CONLY  \
      $WARNING_CFLAGS_JDK_CONLY ${$2EXTRA_CFLAGS}"
  CFLAGS_JDK_COMMON_CXXONLY="$ALWAYS_DEFINES_JDK_CXXONLY $TOOLCHAIN_CFLAGS_JDK_CXXONLY \
      $WARNING_CFLAGS_JDK_CXXONLY ${$2EXTRA_CXXFLAGS}"

  $1_CFLAGS_JVM="${$1_DEFINES_CPU_JVM} ${$1_CFLAGS_CPU} ${$1_CFLAGS_CPU_JVM} ${$1_TOOLCHAIN_CFLAGS} ${$1_WARNING_CFLAGS_JVM}"
  $1_CFLAGS_JDK="${$1_DEFINES_CPU_JDK} ${$1_CFLAGS_CPU} ${$1_CFLAGS_CPU_JDK} ${$1_TOOLCHAIN_CFLAGS}"

  $2JVM_CFLAGS="$CFLAGS_JVM_COMMON ${$1_CFLAGS_JVM} ${$2EXTRA_CXXFLAGS}"

  $2CFLAGS_JDKEXE="$CFLAGS_JDK_COMMON $CFLAGS_JDK_COMMON_CONLY ${$1_CFLAGS_JDK}"
  $2CXXFLAGS_JDKEXE="$CFLAGS_JDK_COMMON $CFLAGS_JDK_COMMON_CXXONLY ${$1_CFLAGS_JDK}"
  $2CFLAGS_JDKLIB="${$2CFLAGS_JDKEXE} $JDK_PICFLAG ${$1_CFLAGS_CPU_JDK_LIBONLY}"
  $2CXXFLAGS_JDKLIB="${$2CXXFLAGS_JDKEXE} $JDK_PICFLAG ${$1_CFLAGS_CPU_JDK_LIBONLY}"

  AC_SUBST($2JVM_CFLAGS)
  AC_SUBST($2CFLAGS_JDKLIB)
  AC_SUBST($2CFLAGS_JDKEXE)
  AC_SUBST($2CXXFLAGS_JDKLIB)
  AC_SUBST($2CXXFLAGS_JDKEXE)
])

# FLAGS_SETUP_GCC6_COMPILER_FLAGS([PREFIX])
# Arguments:
# $1 - Prefix for each variable defined.
AC_DEFUN([FLAGS_SETUP_GCC6_COMPILER_FLAGS],
[
  # These flags are required for GCC 6 builds as undefined behaviour in OpenJDK code
  # runs afoul of the more aggressive versions of these optimisations.
  # Notably, value range propagation now assumes that the this pointer of C++
  # member functions is non-null.
  NO_DELETE_NULL_POINTER_CHECKS_CFLAG="-fno-delete-null-pointer-checks"
  dnl Argument check is disabled until FLAGS_COMPILER_CHECK_ARGUMENTS handles cross-compilation
  dnl FLAGS_COMPILER_CHECK_ARGUMENTS(ARGUMENT: [$NO_DELETE_NULL_POINTER_CHECKS_CFLAG -Werror],
  dnl					     IF_FALSE: [NO_DELETE_NULL_POINTER_CHECKS_CFLAG=""])
  NO_LIFETIME_DSE_CFLAG="-fno-lifetime-dse"
  dnl Argument check is disabled until FLAGS_COMPILER_CHECK_ARGUMENTS handles cross-compilation
  dnl FLAGS_COMPILER_CHECK_ARGUMENTS(ARGUMENT: [$NO_LIFETIME_DSE_CFLAG -Werror],
  dnl					     IF_FALSE: [NO_LIFETIME_DSE_CFLAG=""])
  AC_MSG_NOTICE([GCC >= 6 detected; adding ${NO_DELETE_NULL_POINTER_CHECKS_CFLAG} and ${NO_LIFETIME_DSE_CFLAG}])
  $1_GCC6_CFLAGS="${NO_DELETE_NULL_POINTER_CHECKS_CFLAG} ${NO_LIFETIME_DSE_CFLAG}"
])

#include "precompiled.hpp"
// #include "jfr/jfrEvents.hpp"
#include "jvm.h"
#include "memory/allocation.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/arguments.hpp"
#include "runtime/globals.hpp"
#include "runtime/globals_extension.hpp"
#include "runtime/flags/jvmFlagConstraintList.hpp"
#include "runtime/flags/jvmFlagWriteableList.hpp"
#include "runtime/os.hpp"
#include "runtime/sharedRuntime.hpp"
#include "utilities/defaultStream.hpp"
#include "utilities/macros.hpp"
#include "utilities/ostream.hpp"
#include "utilities/stringUtils.hpp"
#include "c1/c1_globals.hpp"
#include "jvmci/jvmci_globals.hpp"

VM_FLAGS(MATERIALIZE_DEVELOPER_FLAG, \
         MATERIALIZE_PD_DEVELOPER_FLAG, \
         MATERIALIZE_PRODUCT_FLAG, \
         MATERIALIZE_PD_PRODUCT_FLAG, \
         MATERIALIZE_DIAGNOSTIC_FLAG, \
         MATERIALIZE_PD_DIAGNOSTIC_FLAG, \
         MATERIALIZE_EXPERIMENTAL_FLAG, \
         MATERIALIZE_NOTPRODUCT_FLAG, \
         MATERIALIZE_MANAGEABLE_FLAG, \
         MATERIALIZE_PRODUCT_RW_FLAG, \
         MATERIALIZE_LP64_PRODUCT_FLAG, \
         IGNORE_RANGE, \
         IGNORE_CONSTRAINT, \
         IGNORE_WRITEABLE)

RUNTIME_OS_FLAGS(MATERIALIZE_DEVELOPER_FLAG, \
                 MATERIALIZE_PD_DEVELOPER_FLAG, \
                 MATERIALIZE_PRODUCT_FLAG, \
                 MATERIALIZE_PD_PRODUCT_FLAG, \
                 MATERIALIZE_DIAGNOSTIC_FLAG, \
                 MATERIALIZE_PD_DIAGNOSTIC_FLAG, \
                 MATERIALIZE_NOTPRODUCT_FLAG, \
                 IGNORE_RANGE, \
                 IGNORE_CONSTRAINT, \
                 IGNORE_WRITEABLE)

ARCH_FLAGS(MATERIALIZE_DEVELOPER_FLAG, \
           MATERIALIZE_PRODUCT_FLAG, \
           MATERIALIZE_DIAGNOSTIC_FLAG, \
           MATERIALIZE_EXPERIMENTAL_FLAG, \
           MATERIALIZE_NOTPRODUCT_FLAG, \
           IGNORE_RANGE, \
           IGNORE_CONSTRAINT, \
           IGNORE_WRITEABLE)

MATERIALIZE_FLAGS_EXT

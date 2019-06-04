#include "precompiled.hpp"

#include "jvm.h"
#include "classfile/classFileStream.hpp"
#include "classfile/javaClasses.hpp"
#include "classfile/stackMapTableFormat.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/verifier.hpp"
#include "classfile/vmSymbols.hpp"
#include "interpreter/bytecodes.hpp"
#include "interpreter/bytecodeStream.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "oops/constantPool.inline.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayOop.hpp"
#include "runtime/fieldDescriptor.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/orderAccess.hpp"
#include "runtime/os.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/thread.hpp"
#include "services/threadService.hpp"
#include "utilities/align.hpp"
#include "utilities/bytes.hpp"

bool Verifier::verify(InstanceKlass* klass, Verifier::Mode mode, TRAPS) {
  HandleMark hm(THREAD);
  ResourceMark rm(THREAD);

  // Eagerly allocate the identity hash code for a klass. This is a fallout
  // from 6320749 and 8059924: hash code generator is not supposed to be called
  // during the safepoint, but it allows to sneak the hashcode in during
  // verification. Without this eager hashcode generation, we may end up
  // installing the hashcode during some other operation, which may be at
  // safepoint -- blowing up the checks. It was previously done as the side
  // effect (sic!) for external_name(), but instead of doing that, we opt to
  // explicitly push the hashcode in here. This is signify the following block
  // is IMPORTANT:
  if (klass->java_mirror() != NULL) {
    klass->java_mirror()->identity_hash();
  }

  return true;
}

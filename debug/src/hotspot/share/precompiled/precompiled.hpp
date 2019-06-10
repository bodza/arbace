// Precompiled headers are turned off for Sun Studion,
// or if the user passes --disable-precompiled-headers to configure.

#ifndef DONT_USE_PRECOMPILED_HEADER
# include "jni.h"
# include "jvm.h"
# include "asm/assembler.hpp"
# include "asm/assembler.inline.hpp"
# include "asm/codeBuffer.hpp"
# include "asm/register.hpp"
# include "ci/ciArray.hpp"
# include "ci/ciArrayKlass.hpp"
# include "ci/ciClassList.hpp"
# include "ci/ciConstant.hpp"
# include "ci/ciConstantPoolCache.hpp"
# include "ci/ciEnv.hpp"
# include "ci/ciExceptionHandler.hpp"
# include "ci/ciField.hpp"
# include "ci/ciFlags.hpp"
# include "ci/ciInstance.hpp"
# include "ci/ciInstanceKlass.hpp"
# include "ci/ciKlass.hpp"
# include "ci/ciMethod.hpp"
# include "ci/ciNullObject.hpp"
# include "ci/ciObjArrayKlass.hpp"
# include "ci/ciObject.hpp"
# include "ci/ciObjectFactory.hpp"
# include "ci/ciSignature.hpp"
# include "ci/ciStreams.hpp"
# include "ci/ciSymbol.hpp"
# include "ci/ciType.hpp"
# include "ci/ciTypeArrayKlass.hpp"
# include "ci/ciUtilities.inline.hpp"
# include "ci/compilerInterface.hpp"
# include "classfile/classFileParser.hpp"
# include "classfile/classFileStream.hpp"
# include "classfile/classLoader.hpp"
# include "classfile/javaClasses.hpp"
# include "classfile/moduleEntry.hpp"
# include "classfile/modules.hpp"
# include "classfile/packageEntry.hpp"
# include "classfile/symbolTable.hpp"
# include "classfile/systemDictionary.hpp"
# include "classfile/vmSymbols.hpp"
# include "code/codeBlob.hpp"
# include "code/codeCache.hpp"
# include "code/compressedStream.hpp"
# include "code/debugInfo.hpp"
# include "code/debugInfoRec.hpp"
# include "code/exceptionHandlerTable.hpp"
# include "code/location.hpp"
# include "code/nativeInst.hpp"
# include "code/nmethod.hpp"
# include "code/oopRecorder.hpp"
# include "code/pcDesc.hpp"
# include "code/relocInfo.hpp"
# include "code/stubs.hpp"
# include "code/vmreg.hpp"
# include "compiler/disassembler.hpp"
# include "compiler/methodLiveness.hpp"
# include "compiler/oopMap.hpp"
# include "gc/shared/adaptiveSizePolicy.hpp"
# include "gc/shared/ageTable.hpp"
# include "gc/shared/barrierSet.hpp"
# include "gc/shared/blockOffsetTable.hpp"
# include "gc/shared/cardTableBarrierSet.hpp"
# include "gc/shared/collectedHeap.hpp"
# include "gc/shared/collectorPolicy.hpp"
# include "gc/shared/gcCause.hpp"
# include "gc/shared/gcLocker.hpp"
# include "gc/shared/gcStats.hpp"
# include "gc/shared/gcUtil.hpp"
# include "gc/shared/genCollectedHeap.hpp"
# include "gc/shared/generation.hpp"
# include "gc/shared/jvmFlagConstraintsGC.hpp"
# include "gc/shared/modRefBarrierSet.hpp"
# include "gc/shared/referencePolicy.hpp"
# include "gc/shared/referenceProcessor.hpp"
# include "gc/shared/space.hpp"
# include "gc/shared/spaceDecorator.hpp"
# include "gc/shared/taskqueue.hpp"
# include "gc/shared/threadLocalAllocBuffer.hpp"
# include "gc/shared/workgroup.hpp"
# include "interpreter/bytecode.hpp"
# include "interpreter/bytecodes.hpp"
# include "interpreter/interp_masm.hpp"
# include "interpreter/invocationCounter.hpp"
# include "interpreter/linkResolver.hpp"
# include "memory/allocation.hpp"
# include "memory/arena.hpp"
# include "memory/heap.hpp"
# include "memory/iterator.hpp"
# include "memory/memRegion.hpp"
# include "memory/oopFactory.hpp"
# include "memory/resourceArea.hpp"
# include "memory/universe.hpp"
# include "memory/universe.hpp"
# include "memory/virtualspace.hpp"
# include "oops/array.hpp"
# include "oops/arrayKlass.hpp"
# include "oops/arrayOop.hpp"
# include "oops/constMethod.hpp"
# include "oops/instanceKlass.hpp"
# include "oops/instanceOop.hpp"
# include "oops/instanceRefKlass.hpp"
# include "oops/klass.hpp"
# include "oops/klassVtable.hpp"
# include "oops/markOop.hpp"
# include "oops/markOop.inline.hpp"
# include "oops/method.hpp"
# include "oops/methodData.hpp"
# include "oops/objArrayKlass.hpp"
# include "oops/objArrayOop.hpp"
# include "oops/oop.hpp"
# include "oops/oopsHierarchy.hpp"
# include "oops/symbol.hpp"
# include "oops/typeArrayKlass.hpp"
# include "oops/typeArrayOop.hpp"
# include "runtime/arguments.hpp"
# include "runtime/atomic.hpp"
# include "runtime/extendedPC.hpp"
# include "runtime/fieldDescriptor.hpp"
# include "runtime/fieldType.hpp"
# include "runtime/flags/flagSetting.hpp"
# include "runtime/flags/jvmFlag.hpp"
# include "runtime/flags/jvmFlagConstraintList.hpp"
# include "runtime/flags/jvmFlagConstraintsCompiler.hpp"
# include "runtime/flags/jvmFlagConstraintsRuntime.hpp"
# include "runtime/flags/jvmFlagRangeList.hpp"
# include "runtime/flags/jvmFlagWriteableList.hpp"
# include "runtime/frame.hpp"
# include "runtime/frame.inline.hpp"
# include "runtime/globals.hpp"
# include "runtime/globals_extension.hpp"
# include "runtime/handles.hpp"
# include "runtime/handles.inline.hpp"
# include "runtime/icache.hpp"
# include "runtime/init.hpp"
# include "runtime/interfaceSupport.inline.hpp"
# include "runtime/java.hpp"
# include "runtime/javaCalls.hpp"
# include "runtime/javaFrameAnchor.hpp"
# include "runtime/jniHandles.hpp"
# include "runtime/monitorChunk.hpp"
# include "runtime/mutex.hpp"
# include "runtime/mutexLocker.hpp"
# include "runtime/objectMonitor.hpp"
# include "runtime/orderAccess.hpp"
# include "runtime/orderAccess.hpp"
# include "runtime/os.hpp"
# include "runtime/osThread.hpp"
# include "runtime/prefetch.hpp"
# include "runtime/prefetch.inline.hpp"
# include "runtime/reflection.hpp"
# include "runtime/reflectionUtils.hpp"
# include "runtime/registerMap.hpp"
# include "runtime/safepoint.hpp"
# include "runtime/sharedRuntime.hpp"
# include "runtime/signature.hpp"
# include "runtime/stackValue.hpp"
# include "runtime/stackValueCollection.hpp"
# include "runtime/stubCodeGenerator.hpp"
# include "runtime/stubRoutines.hpp"
# include "runtime/synchronizer.hpp"
# include "runtime/thread.hpp"
# include "runtime/timer.hpp"
# include "runtime/vframe.hpp"
# include "runtime/vmThread.hpp"
# include "runtime/vm_operations.hpp"
# include "runtime/vm_version.hpp"
# include "services/memoryPool.hpp"
# include "services/memoryService.hpp"
# include "services/memoryUsage.hpp"
# include "utilities/accessFlags.hpp"
# include "utilities/bitMap.hpp"
# include "utilities/bitMap.inline.hpp"
# include "utilities/bytes.hpp"
# include "utilities/constantTag.hpp"
# include "utilities/copy.hpp"
# include "utilities/debug.hpp"
# include "utilities/exceptions.hpp"
# include "utilities/globalDefinitions.hpp"
# include "utilities/growableArray.hpp"
# include "utilities/hashtable.hpp"
# include "utilities/macros.hpp"
# include "utilities/numberSeq.hpp"
# include "utilities/ostream.hpp"
# include "utilities/preserveException.hpp"
# include "utilities/sizes.hpp"
# include "utilities/utf8.hpp"
# include "c1/c1_Compilation.hpp"
# include "c1/c1_Defs.hpp"
# include "c1/c1_FrameMap.hpp"
# include "c1/c1_LIR.hpp"
# include "c1/c1_MacroAssembler.hpp"
# include "c1/c1_ValueType.hpp"
# include "c1/c1_globals.hpp"
# include "jvmci/jvmci_globals.hpp"
# include "gc/g1/dirtyCardQueue.hpp"
# include "gc/g1/g1BlockOffsetTable.hpp"
# include "gc/g1/g1OopClosures.hpp"
# include "gc/g1/g1_globals.hpp"
# include "gc/g1/jvmFlagConstraintsG1.hpp"
# include "gc/g1/ptrQueue.hpp"
# include "gc/g1/satbMarkQueue.hpp"

#endif

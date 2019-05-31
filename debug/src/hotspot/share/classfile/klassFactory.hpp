#ifndef SHARE_VM_CLASSFILE_KLASSFACTORY_HPP
#define SHARE_VM_CLASSFILE_KLASSFACTORY_HPP

#include "memory/allocation.hpp"
#include "runtime/handles.hpp"

class ClassFileStream;
class ClassLoaderData;
template <typename>
class GrowableArray;
class Klass;
class Symbol;
class TempNewSymbol;

/*
 * KlassFactory is an interface to implementations of the following mapping/function:
 *
 * Summary: create a VM internal runtime representation ("Klass")
            from a bytestream (classfile).
 *
 * Input:  a named bytestream in the Java class file format (see JVMS, chapter 4).
 * Output: a VM runtime representation of a Java class
 *
 * Pre-conditions:
 *   a non-NULL ClassFileStream* // the classfile bytestream
 *   a non-NULL Symbol*          // the name of the class
 *   a non-NULL ClassLoaderData* // the metaspace allocator
 *   (no pending exceptions)
 *
 * Returns:
 *   if the returned value is non-NULL, that value is an indirection (pointer/handle)
 *   to a Klass. The caller will not have a pending exception.
 *
 *   On broken invariants and/or runtime errors the returned value will be
 *   NULL (or a NULL handle) and the caller *might* now have a pending exception.
 *
 */

class KlassFactory : AllStatic {

  // approved clients
  friend class ClassLoader;
  friend class ClassLoaderExt;
  friend class SystemDictionary;

 private:
  static InstanceKlass* create_from_stream(ClassFileStream* stream,
                                           Symbol* name,
                                           ClassLoaderData* loader_data,
                                           Handle protection_domain,
                                           const InstanceKlass* host_klass,
                                           GrowableArray<Handle>* cp_patches,
                                           TRAPS);
 public:
  static InstanceKlass* check_shared_class_file_load_hook(InstanceKlass* ik, Symbol* class_name, Handle class_loader, Handle protection_domain, TRAPS);
};

#endif

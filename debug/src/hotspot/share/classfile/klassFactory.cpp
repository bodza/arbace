#include "precompiled.hpp"
#include "classfile/classFileParser.hpp"
#include "classfile/classFileStream.hpp"
#include "classfile/classLoader.hpp"
#include "classfile/classLoaderData.hpp"
#include "classfile/classLoaderData.inline.hpp"
#include "classfile/klassFactory.hpp"
#include "memory/filemap.hpp"
#include "memory/metaspaceShared.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/handles.inline.hpp"
#include "utilities/macros.hpp"

// called during initial loading of a shared class
InstanceKlass* KlassFactory::check_shared_class_file_load_hook(InstanceKlass* ik, Symbol* class_name, Handle class_loader, Handle protection_domain, TRAPS) {

  return NULL;
}

InstanceKlass* KlassFactory::create_from_stream(ClassFileStream* stream,
                                                Symbol* name,
                                                ClassLoaderData* loader_data,
                                                Handle protection_domain,
                                                const InstanceKlass* host_klass,
                                                GrowableArray<Handle>* cp_patches,
                                                TRAPS) {

  ResourceMark rm;
  HandleMark hm;

  ClassFileStream* old_stream = stream;

  // increment counter
  THREAD->statistical_info().incr_define_class_count();

  ClassFileParser parser(stream,
                         name,
                         loader_data,
                         protection_domain,
                         host_klass,
                         cp_patches,
                         ClassFileParser::BROADCAST, // publicity level
                         CHECK_NULL);

  InstanceKlass* result = parser.create_instance_klass(old_stream != stream, CHECK_NULL);

  if (result == NULL) {
    return NULL;
  }

  if (result->should_store_fingerprint()) {
    result->store_fingerprint(stream->compute_fingerprint());
  }

  return result;
}

#include "precompiled.hpp"
#include "gc/shared/cardTableRS.hpp"
#include "gc/shared/generationSpec.hpp"
#include "memory/binaryTreeDictionary.hpp"
#include "memory/filemap.hpp"
#include "runtime/java.hpp"
#include "utilities/macros.hpp"

Generation* GenerationSpec::init(ReservedSpace rs, CardTableRS* remset) {
  switch (name()) {

    default:
      guarantee(false, "unrecognized GenerationName");
      return NULL;
  }
}

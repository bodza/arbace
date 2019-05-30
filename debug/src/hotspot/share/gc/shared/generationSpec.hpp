#ifndef SHARE_VM_GC_SHARED_GENERATIONSPEC_HPP
#define SHARE_VM_GC_SHARED_GENERATIONSPEC_HPP

#include "gc/shared/generation.hpp"
#include "utilities/align.hpp"

// The specification of a generation.  This class also encapsulates
// some generation-specific behavior.  This is done here rather than as a
// virtual function of Generation because these methods are needed in
// initialization of the Generations.
class GenerationSpec : public CHeapObj<mtGC> {
  friend class VMStructs;
private:
  Generation::Name _name;
  size_t           _init_size;
  size_t           _max_size;

public:
  GenerationSpec(Generation::Name name, size_t init_size, size_t max_size, size_t alignment) :
    _name(name),
    _init_size(align_up(init_size, alignment)),
    _max_size(align_up(max_size, alignment))
  { }

  Generation* init(ReservedSpace rs, CardTableRS* remset);

  // Accessors
  Generation::Name name()        const { return _name; }
  size_t init_size()             const { return _init_size; }
  void set_init_size(size_t size)      { _init_size = size; }
  size_t max_size()              const { return _max_size; }
  void set_max_size(size_t size)       { _max_size = size; }
};

typedef GenerationSpec* GenerationSpecPtr;

#endif

#ifndef SHARE_GC_SHARED_C2_CARDTABLEBARRIERSETC2_HPP
#define SHARE_GC_SHARED_C2_CARDTABLEBARRIERSETC2_HPP

#include "gc/shared/c2/modRefBarrierSetC2.hpp"

class CardTableBarrierSetC2: public ModRefBarrierSetC2 {
protected:
  virtual void post_barrier(GraphKit* kit,
                            Node* ctl,
                            Node* store,
                            Node* obj,
                            Node* adr,
                            uint adr_idx,
                            Node* val,
                            BasicType bt,
                            bool use_precise) const;

  Node* byte_map_base_node(GraphKit* kit) const;

public:
  virtual void clone(GraphKit* kit, Node* src, Node* dst, Node* size, bool is_array) const;
  virtual bool is_gc_barrier_node(Node* node) const;
  virtual void eliminate_gc_barrier(PhaseMacroExpand* macro, Node* node) const;
  virtual bool array_copy_requires_gc_barriers(BasicType type) const;

  bool use_ReduceInitialCardMarks() const;
};

#endif

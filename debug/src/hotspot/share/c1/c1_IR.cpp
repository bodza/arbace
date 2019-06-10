#include "precompiled.hpp"

#include "c1/c1_Compilation.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_GraphBuilder.hpp"
#include "c1/c1_IR.hpp"
#include "c1/c1_Optimizer.hpp"
#include "memory/resourceArea.hpp"
#include "utilities/bitMap.inline.hpp"

// Implementation of XHandlers
//
// Note: This code could eventually go away if we are
//       just using the ciExceptionHandlerStream.

XHandlers::XHandlers(ciMethod* method) : _list(method->exception_table_length()) {
  ciExceptionHandlerStream s(method);
  while (!s.is_done()) {
    _list.append(new XHandler(s.handler()));
    s.next();
  }
}

// deep copy of all XHandler contained in list
XHandlers::XHandlers(XHandlers* other) : _list(other->length()) {
  for (int i = 0; i < other->length(); i++) {
    _list.append(new XHandler(other->handler_at(i)));
  }
}

// Returns whether a particular exception type can be caught.  Also
// returns true if klass is unloaded or any exception handler
// classes are unloaded.  type_is_exact indicates whether the throw
// is known to be exactly that class or it might throw a subtype.
bool XHandlers::could_catch(ciInstanceKlass* klass, bool type_is_exact) const {
  // the type is unknown so be conservative
  if (!klass->is_loaded()) {
    return true;
  }

  for (int i = 0; i < length(); i++) {
    XHandler* handler = handler_at(i);
    if (handler->is_catch_all()) {
      // catch of ANY
      return true;
    }
    ciInstanceKlass* handler_klass = handler->catch_klass();
    // if it's unknown it might be catchable
    if (!handler_klass->is_loaded()) {
      return true;
    }
    // if the throw type is definitely a subtype of the catch type
    // then it can be caught.
    if (klass->is_subtype_of(handler_klass)) {
      return true;
    }
    if (!type_is_exact) {
      // If the type isn't exactly known then it can also be caught by
      // catch statements where the inexact type is a subtype of the
      // catch type.
      // given: foo extends bar extends Exception
      // throw bar can be caught by catch foo, catch bar, and catch
      // Exception, however it can't be caught by any handlers without
      // bar in its type hierarchy.
      if (handler_klass->is_subtype_of(klass)) {
        return true;
      }
    }
  }

  return false;
}

bool XHandlers::equals(XHandlers* others) const {
  if (others == NULL) return false;
  if (length() != others->length()) return false;

  for (int i = 0; i < length(); i++) {
    if (!handler_at(i)->equals(others->handler_at(i))) return false;
  }
  return true;
}

bool XHandler::equals(XHandler* other) const {
  if (entry_pco() != other->entry_pco()) return false;
  if (scope_count() != other->scope_count()) return false;
  if (_desc != other->_desc) return false;

  return true;
}

// Implementation of IRScope
BlockBegin* IRScope::build_graph(Compilation* compilation) {
  GraphBuilder gm(compilation, this);
  if (compilation->bailed_out()) return NULL;
  return gm.start();
}

IRScope::IRScope(Compilation* compilation, IRScope* caller, int caller_bci, ciMethod* method, bool create_graph)
: _callees(2)
, _compilation(compilation)
, _requires_phi_function(method->max_locals())
{
  _caller             = caller;
  _level              = caller == NULL ?  0 : caller->level() + 1;
  _method             = method;
  _xhandlers          = new XHandlers(method);
  _number_of_locks    = 0;
  _monitor_pairing_ok = method->has_balanced_monitors();
  _wrote_final        = false;
  _wrote_fields       = false;
  _wrote_volatile     = false;
  _start              = NULL;

  // build graph if monitor pairing is ok
  if (create_graph && monitor_pairing_ok()) _start = build_graph(compilation);
}

int IRScope::max_stack() const {
  int my_max = method()->max_stack();
  int callee_max = 0;
  for (int i = 0; i < number_of_callees(); i++) {
    callee_max = MAX2(callee_max, callee_no(i)->max_stack());
  }
  return my_max + callee_max;
}

bool IRScopeDebugInfo::should_reexecute() { return false; }

// Implementation of CodeEmitInfo

// Stack must be NON-null
CodeEmitInfo::CodeEmitInfo(ValueStack* stack, XHandlers* exception_handlers)
  : _scope(stack->scope())
  , _scope_debug_info(NULL)
  , _oop_map(NULL)
  , _stack(stack)
  , _exception_handlers(exception_handlers) {
}

CodeEmitInfo::CodeEmitInfo(CodeEmitInfo* info, ValueStack* stack)
  : _scope(info->_scope)
  , _exception_handlers(NULL)
  , _scope_debug_info(NULL)
  , _oop_map(NULL)
  , _stack(stack == NULL ? info->_stack : stack) {
  // deep copy of exception handlers
  if (info->_exception_handlers != NULL) {
    _exception_handlers = new XHandlers(info->_exception_handlers);
  }
}

void CodeEmitInfo::record_debug_info(DebugInformationRecorder* recorder, int pc_offset) {
  // record the safepoint before recording the debug info for enclosing scopes
  recorder->add_safepoint(pc_offset, _oop_map->deep_copy());
  _scope_debug_info->record_debug_info(recorder, pc_offset, /*topmost*/ true);
  recorder->end_safepoint(pc_offset);
}

void CodeEmitInfo::add_register_oop(LIR_Opr opr) {
  VMReg name = frame_map()->regname(opr);
  _oop_map->set_oop(name);
}

// Implementation of IR

IR::IR(Compilation* compilation, ciMethod* method) :
  _num_loops(0) {
  // setup IR fields
  _compilation = compilation;
  _top_scope   = new IRScope(compilation, NULL, -1, method, true);
  _code        = NULL;
}

void IR::optimize_blocks() {
  Optimizer opt(this);
  {
    if (DoCEE) {
      opt.eliminate_conditional_expressions();
    }
    if (EliminateBlocks) {
      opt.eliminate_blocks();
    }
  }
}

void IR::eliminate_null_checks() {
  Optimizer opt(this);
  if (EliminateNullChecks) {
    opt.eliminate_null_checks();
  }
}

static int sort_pairs(BlockPair** a, BlockPair** b) {
  if ((*a)->from() == (*b)->from()) {
    return (*a)->to()->block_id() - (*b)->to()->block_id();
  } else {
    return (*a)->from()->block_id() - (*b)->from()->block_id();
  }
}

class CriticalEdgeFinder: public BlockClosure {
  BlockPairList blocks;
  IR*       _ir;

 public:
  CriticalEdgeFinder(IR* ir): _ir(ir) { }
  void block_do(BlockBegin* bb) {
    BlockEnd* be = bb->end();
    int nos = be->number_of_sux();
    if (nos >= 2) {
      for (int i = 0; i < nos; i++) {
        BlockBegin* sux = be->sux_at(i);
        if (sux->number_of_preds() >= 2) {
          blocks.append(new BlockPair(bb, sux));
        }
      }
    }
  }

  void split_edges() {
    BlockPair* last_pair = NULL;
    blocks.sort(sort_pairs);
    for (int i = 0; i < blocks.length(); i++) {
      BlockPair* pair = blocks.at(i);
      if (last_pair != NULL && pair->is_same(last_pair)) continue;
      BlockBegin* from = pair->from();
      BlockBegin* to = pair->to();
      BlockBegin* split = from->insert_block_between(to);
      last_pair = pair;
    }
  }
};

void IR::split_critical_edges() {
  CriticalEdgeFinder cef(this);

  iterate_preorder(&cef);
  cef.split_edges();
}

class UseCountComputer: public ValueVisitor, BlockClosure {
 private:
  void visit(Value* n) {
    // Local instructions and Phis for expression stack values at the
    // start of basic blocks are not added to the instruction list
    if (!(*n)->is_linked() && (*n)->can_be_linked()) {
      ShouldNotReachHere();
      Compilation::current()->bailout("a node was not appended to the graph");
    }
    // use n's input if not visited before
    if (!(*n)->is_pinned() && !(*n)->has_uses()) {
      // note: a) if the instruction is pinned, it will be handled by compute_use_count
      //       b) if the instruction has uses, it was touched before
      //       => in both cases we don't need to update n's values
      uses_do(n);
    }
    // use n
    (*n)->_use_count++;
  }

  Values* worklist;
  int depth;
  enum {
    max_recurse_depth = 20
  };

  void uses_do(Value* n) {
    depth++;
    if (depth > max_recurse_depth) {
      // don't allow the traversal to recurse too deeply
      worklist->push(*n);
    } else {
      (*n)->input_values_do(this);
      // special handling for some instructions
      if ((*n)->as_BlockEnd() != NULL) {
        // note on BlockEnd:
        //   must 'use' the stack only if the method doesn't
        //   terminate, however, in those cases stack is empty
        (*n)->state_values_do(this);
      }
    }
    depth--;
  }

  void block_do(BlockBegin* b) {
    depth = 0;
    // process all pinned nodes as the roots of expression trees
    for (Instruction* n = b; n != NULL; n = n->next()) {
      if (n->is_pinned()) uses_do(&n);
    }

    // now process any unpinned nodes which recursed too deeply
    while (worklist->length() > 0) {
      Value t = worklist->pop();
      if (!t->is_pinned()) {
        // compute the use count
        uses_do(&t);

        // pin the instruction so that LIRGenerator doesn't recurse
        // too deeply during it's evaluation.
        t->pin();
      }
    }
  }

  UseCountComputer() {
    worklist = new Values();
    depth = 0;
  }

 public:
  static void compute(BlockList* blocks) {
    UseCountComputer ucc;
    blocks->iterate_backward(&ucc);
  }
};

class ComputeLinearScanOrder : public StackObj {
 private:
  int        _max_block_id;        // the highest block_id of a block
  int        _num_blocks;          // total number of blocks (smaller than _max_block_id)
  int        _num_loops;           // total number of loops
  bool       _iterative_dominators;// method requires iterative computation of dominatiors

  BlockList* _linear_scan_order;   // the resulting list of blocks in correct order

  ResourceBitMap _visited_blocks;   // used for recursive processing of blocks
  ResourceBitMap _active_blocks;    // used for recursive processing of blocks
  ResourceBitMap _dominator_blocks; // temproary BitMap used for computation of dominator
  intArray       _forward_branches; // number of incoming forward branches for each block
  BlockList      _loop_end_blocks;  // list of all loop end blocks collected during count_edges
  BitMap2D       _loop_map;         // two-dimensional bit set: a bit is set if a block is contained in a loop
  BlockList      _work_list;        // temporary list (used in mark_loops and compute_order)
  BlockList      _loop_headers;

  Compilation* _compilation;

  // accessors for _visited_blocks and _active_blocks
  void init_visited()                     { _active_blocks.clear(); _visited_blocks.clear(); }
  bool is_visited(BlockBegin* b)    const { return _visited_blocks.at(b->block_id()); }
  bool is_active(BlockBegin* b)     const { return _active_blocks.at(b->block_id()); }
  void set_visited(BlockBegin* b)         { _visited_blocks.set_bit(b->block_id()); }
  void set_active(BlockBegin* b)          { _active_blocks.set_bit(b->block_id()); }
  void clear_active(BlockBegin* b)        { _active_blocks.clear_bit(b->block_id()); }

  // accessors for _forward_branches
  void inc_forward_branches(BlockBegin* b) { _forward_branches.at_put(b->block_id(), _forward_branches.at(b->block_id()) + 1); }
  int  dec_forward_branches(BlockBegin* b) { _forward_branches.at_put(b->block_id(), _forward_branches.at(b->block_id()) - 1); return _forward_branches.at(b->block_id()); }

  // accessors for _loop_map
  bool is_block_in_loop   (int loop_idx, BlockBegin* b) const { return _loop_map.at(loop_idx, b->block_id()); }
  void set_block_in_loop  (int loop_idx, BlockBegin* b)       { _loop_map.set_bit(loop_idx, b->block_id()); }
  void clear_block_in_loop(int loop_idx, int block_id)        { _loop_map.clear_bit(loop_idx, block_id); }

  // count edges between blocks
  void count_edges(BlockBegin* cur, BlockBegin* parent);

  // loop detection
  void mark_loops();
  void clear_non_natural_loops(BlockBegin* start_block);
  void assign_loop_depth(BlockBegin* start_block);

  // computation of final block order
  BlockBegin* common_dominator(BlockBegin* a, BlockBegin* b);
  void compute_dominator(BlockBegin* cur, BlockBegin* parent);
  void compute_dominator_impl(BlockBegin* cur, BlockBegin* parent);
  int  compute_weight(BlockBegin* cur);
  bool ready_for_processing(BlockBegin* cur);
  void sort_into_work_list(BlockBegin* b);
  void append_block(BlockBegin* cur);
  void compute_order(BlockBegin* start_block);

  // fixup of dominators for non-natural loops
  bool compute_dominators_iter();
  void compute_dominators();

  Compilation* compilation() const { return _compilation; }
 public:
  ComputeLinearScanOrder(Compilation* c, BlockBegin* start_block);

  // accessors for final result
  BlockList* linear_scan_order()    const { return _linear_scan_order; }
  int        num_loops()            const { return _num_loops; }
};

ComputeLinearScanOrder::ComputeLinearScanOrder(Compilation* c, BlockBegin* start_block) :
  _max_block_id(BlockBegin::number_of_blocks()),
  _num_blocks(0),
  _num_loops(0),
  _iterative_dominators(false),
  _visited_blocks(_max_block_id),
  _active_blocks(_max_block_id),
  _dominator_blocks(_max_block_id),
  _forward_branches(_max_block_id, _max_block_id, 0),
  _loop_end_blocks(8),
  _work_list(8),
  _linear_scan_order(NULL), // initialized later with correct size
  _loop_map(0),             // initialized later with correct size
  _compilation(c)
{
  count_edges(start_block, NULL);

  if (_num_loops > 0) {
    mark_loops();
    clear_non_natural_loops(start_block);
    assign_loop_depth(start_block);
  }

  compute_order(start_block);
  compute_dominators();
}

// Traverse the CFG:
// * count total number of blocks
// * count all incoming edges and backward incoming edges
// * number loop header blocks
// * create a list with all loop end blocks
void ComputeLinearScanOrder::count_edges(BlockBegin* cur, BlockBegin* parent) {
  if (is_active(cur)) {
    cur->set(BlockBegin::backward_branch_target_flag);

    // When a loop header is also the start of an exception handler, then the backward branch is
    // an exception edge. Because such edges are usually critical edges which cannot be split, the
    // loop must be excluded here from processing.
    if (cur->is_set(BlockBegin::exception_entry_flag)) {
      // Make sure that dominators are correct in this weird situation
      _iterative_dominators = true;
      return;
    }

    cur->set(BlockBegin::linear_scan_loop_header_flag);
    parent->set(BlockBegin::linear_scan_loop_end_flag);

    _loop_end_blocks.append(parent);
    return;
  }

  // increment number of incoming forward branches
  inc_forward_branches(cur);

  if (is_visited(cur)) {
    return;
  }

  _num_blocks++;
  set_visited(cur);
  set_active(cur);

  // recursive call for all successors
  int i;
  for (i = cur->number_of_sux() - 1; i >= 0; i--) {
    count_edges(cur->sux_at(i), cur);
  }
  for (i = cur->number_of_exception_handlers() - 1; i >= 0; i--) {
    count_edges(cur->exception_handler_at(i), cur);
  }

  clear_active(cur);

  // Each loop has a unique number.
  // When multiple loops are nested, assign_loop_depth assumes that the
  // innermost loop has the lowest number. This is guaranteed by setting
  // the loop number after the recursive calls for the successors above
  // have returned.
  if (cur->is_set(BlockBegin::linear_scan_loop_header_flag)) {
    cur->set_loop_index(_num_loops);
    _loop_headers.append(cur);
    _num_loops++;
  }
}

void ComputeLinearScanOrder::mark_loops() {
  _loop_map = BitMap2D(_num_loops, _max_block_id);

  for (int i = _loop_end_blocks.length() - 1; i >= 0; i--) {
    BlockBegin* loop_end   = _loop_end_blocks.at(i);
    BlockBegin* loop_start = loop_end->sux_at(0);
    int         loop_idx   = loop_start->loop_index();

    // add the end-block of the loop to the working list
    _work_list.push(loop_end);
    set_block_in_loop(loop_idx, loop_end);
    do {
      BlockBegin* cur = _work_list.pop();

      // recursive processing of all predecessors ends when start block of loop is reached
      if (cur != loop_start) {
        for (int j = cur->number_of_preds() - 1; j >= 0; j--) {
          BlockBegin* pred = cur->pred_at(j);

          if (!is_block_in_loop(loop_idx, pred) /*&& !pred->is_set(BlockBeginosr_entry_flag)*/) {
            // this predecessor has not been processed yet, so add it to work list
            _work_list.push(pred);
            set_block_in_loop(loop_idx, pred);
          }
        }
      }
    } while (!_work_list.is_empty());
  }
}

// check for non-natural loops (loops where the loop header does not dominate
// all other loop blocks = loops with mulitple entries).
// such loops are ignored
void ComputeLinearScanOrder::clear_non_natural_loops(BlockBegin* start_block) {
  for (int i = _num_loops - 1; i >= 0; i--) {
    if (is_block_in_loop(i, start_block)) {
      // loop i contains the entry block of the method
      // -> this is not a natural loop, so ignore it
      BlockBegin *loop_header = _loop_headers.at(i);

      for (int j = 0; j < loop_header->number_of_preds(); j++) {
        BlockBegin *pred = loop_header->pred_at(j);
        pred->clear(BlockBegin::linear_scan_loop_end_flag);
      }

      loop_header->clear(BlockBegin::linear_scan_loop_header_flag);

      for (int block_id = _max_block_id - 1; block_id >= 0; block_id--) {
        clear_block_in_loop(i, block_id);
      }
      _iterative_dominators = true;
    }
  }
}

void ComputeLinearScanOrder::assign_loop_depth(BlockBegin* start_block) {
  init_visited();

  _work_list.append(start_block);

  do {
    BlockBegin* cur = _work_list.pop();

    if (!is_visited(cur)) {
      set_visited(cur);

      // compute loop-depth and loop-index for the block
      int i;
      int loop_depth = 0;
      int min_loop_idx = -1;
      for (i = _num_loops - 1; i >= 0; i--) {
        if (is_block_in_loop(i, cur)) {
          loop_depth++;
          min_loop_idx = i;
        }
      }
      cur->set_loop_depth(loop_depth);
      cur->set_loop_index(min_loop_idx);

      // append all unvisited successors to work list
      for (i = cur->number_of_sux() - 1; i >= 0; i--) {
        _work_list.append(cur->sux_at(i));
      }
      for (i = cur->number_of_exception_handlers() - 1; i >= 0; i--) {
        _work_list.append(cur->exception_handler_at(i));
      }
    }
  } while (!_work_list.is_empty());
}

BlockBegin* ComputeLinearScanOrder::common_dominator(BlockBegin* a, BlockBegin* b) {
  _dominator_blocks.clear();
  while (a != NULL) {
    _dominator_blocks.set_bit(a->block_id());
    a = a->dominator();
  }
  while (b != NULL && !_dominator_blocks.at(b->block_id())) {
    b = b->dominator();
  }

  return b;
}

void ComputeLinearScanOrder::compute_dominator(BlockBegin* cur, BlockBegin* parent) {
  init_visited();
  compute_dominator_impl(cur, parent);
}

void ComputeLinearScanOrder::compute_dominator_impl(BlockBegin* cur, BlockBegin* parent) {
  // Mark as visited to avoid recursive calls with same parent
  set_visited(cur);

  if (cur->dominator() == NULL) {
    cur->set_dominator(parent);
  } else if (!(cur->is_set(BlockBegin::linear_scan_loop_header_flag) && parent->is_set(BlockBegin::linear_scan_loop_end_flag))) {
    // Does not hold for exception blocks
    cur->set_dominator(common_dominator(cur->dominator(), parent));
  }

  // Additional edge to xhandler of all our successors
  // range check elimination needs that the state at the end of a
  // block be valid in every block it dominates so cur must dominate
  // the exception handlers of its successors.
  int num_cur_xhandler = cur->number_of_exception_handlers();
  for (int j = 0; j < num_cur_xhandler; j++) {
    BlockBegin* xhandler = cur->exception_handler_at(j);
    if (!is_visited(xhandler)) {
      compute_dominator_impl(xhandler, parent);
    }
  }
}

int ComputeLinearScanOrder::compute_weight(BlockBegin* cur) {
  BlockBegin* single_sux = NULL;
  if (cur->number_of_sux() == 1) {
    single_sux = cur->sux_at(0);
  }

  // limit loop-depth to 15 bit (only for security reason, it will never be so big)
  int weight = (cur->loop_depth() & 0x7FFF) << 16;

  // general macro for short definition of weight flags
  // the first instance of INC_WEIGHT_IF has the highest priority
  int cur_bit = 15;
  #define INC_WEIGHT_IF(condition) if ((condition)) { weight |= (1 << cur_bit); } cur_bit--;

  // this is necessery for the (very rare) case that two successing blocks have
  // the same loop depth, but a different loop index (can happen for endless loops
  // with exception handlers)
  INC_WEIGHT_IF(!cur->is_set(BlockBegin::linear_scan_loop_header_flag));

  // loop end blocks (blocks that end with a backward branch) are added
  // after all other blocks of the loop.
  INC_WEIGHT_IF(!cur->is_set(BlockBegin::linear_scan_loop_end_flag));

  // critical edge split blocks are prefered because than they have a bigger
  // proability to be completely empty
  INC_WEIGHT_IF(cur->is_set(BlockBegin::critical_edge_split_flag));

  // exceptions should not be thrown in normal control flow, so these blocks
  // are added as late as possible
  INC_WEIGHT_IF(cur->end()->as_Throw() == NULL  && (single_sux == NULL || single_sux->end()->as_Throw()  == NULL));
  INC_WEIGHT_IF(cur->end()->as_Return() == NULL && (single_sux == NULL || single_sux->end()->as_Return() == NULL));

  // exceptions handlers are added as late as possible
  INC_WEIGHT_IF(!cur->is_set(BlockBegin::exception_entry_flag));

  // guarantee that weight is > 0
  weight |= 1;

  #undef INC_WEIGHT_IF

  return weight;
}

bool ComputeLinearScanOrder::ready_for_processing(BlockBegin* cur) {
  // Discount the edge just traveled.
  // When the number drops to zero, all forward branches were processed
  if (dec_forward_branches(cur) != 0) {
    return false;
  }

  return true;
}

void ComputeLinearScanOrder::sort_into_work_list(BlockBegin* cur) {
  int cur_weight = compute_weight(cur);

  // the linear_scan_number is used to cache the weight of a block
  cur->set_linear_scan_number(cur_weight);

  _work_list.append(NULL); // provide space for new element

  int insert_idx = _work_list.length() - 1;
  while (insert_idx > 0 && _work_list.at(insert_idx - 1)->linear_scan_number() > cur_weight) {
    _work_list.at_put(insert_idx, _work_list.at(insert_idx - 1));
    insert_idx--;
  }
  _work_list.at_put(insert_idx, cur);
}

void ComputeLinearScanOrder::append_block(BlockBegin* cur) {
  // currently, the linear scan order and code emit order are equal.
  // therefore the linear_scan_number and the weight of a block must also
  // be equal.
  cur->set_linear_scan_number(_linear_scan_order->length());
  _linear_scan_order->append(cur);
}

void ComputeLinearScanOrder::compute_order(BlockBegin* start_block) {
  // the start block is always the first block in the linear scan order
  _linear_scan_order = new BlockList(_num_blocks);
  append_block(start_block);

  BlockBegin* std_entry = ((Base*)start_block->end())->std_entry();

  compute_dominator(std_entry, start_block);

  // start processing with standard entry block

  if (ready_for_processing(std_entry)) {
    sort_into_work_list(std_entry);
  } else {
    ShouldNotReachHere();
  }

  do {
    BlockBegin* cur = _work_list.pop();

    append_block(cur);

    int i;
    int num_sux = cur->number_of_sux();
    // changed loop order to get "intuitive" order of if- and else-blocks
    for (i = 0; i < num_sux; i++) {
      BlockBegin* sux = cur->sux_at(i);
      compute_dominator(sux, cur);
      if (ready_for_processing(sux)) {
        sort_into_work_list(sux);
      }
    }
    num_sux = cur->number_of_exception_handlers();
    for (i = 0; i < num_sux; i++) {
      BlockBegin* sux = cur->exception_handler_at(i);
      if (ready_for_processing(sux)) {
        sort_into_work_list(sux);
      }
    }
  } while (_work_list.length() > 0);
}

bool ComputeLinearScanOrder::compute_dominators_iter() {
  bool changed = false;
  int num_blocks = _linear_scan_order->length();

  for (int i = 1; i < num_blocks; i++) {
    BlockBegin* block = _linear_scan_order->at(i);

    BlockBegin* dominator = block->pred_at(0);
    int num_preds = block->number_of_preds();

    for (int j = 0; j < num_preds; j++) {
      BlockBegin *pred = block->pred_at(j);

      if (block->is_set(BlockBegin::exception_entry_flag)) {
        dominator = common_dominator(dominator, pred);
        int num_pred_preds = pred->number_of_preds();
        for (int k = 0; k < num_pred_preds; k++) {
          dominator = common_dominator(dominator, pred->pred_at(k));
        }
      } else {
        dominator = common_dominator(dominator, pred);
      }
    }

    if (dominator != block->dominator()) {
      block->set_dominator(dominator);
      changed = true;
    }
  }
  return changed;
}

void ComputeLinearScanOrder::compute_dominators() {
  // iterative computation of dominators is only required for methods with non-natural loops.
  // For all other methods, the dominators computed when generating the
  // linear scan block order are correct.
  if (_iterative_dominators) {
    do {
    } while (compute_dominators_iter());
  }

  // Add Blocks to dominates-Array
  int num_blocks = _linear_scan_order->length();
  for (int i = 0; i < num_blocks; i++) {
    BlockBegin* block = _linear_scan_order->at(i);

    BlockBegin *dom = block->dominator();
    if (dom) {
      dom->dominates()->append(block);
      block->set_dominator_depth(dom->dominator_depth() + 1);
    } else {
      block->set_dominator_depth(0);
    }
  }
}

void IR::compute_code() {
  ComputeLinearScanOrder compute_order(compilation(), start());
  _num_loops = compute_order.num_loops();
  _code = compute_order.linear_scan_order();
}

void IR::compute_use_counts() {
  // make sure all values coming out of this block get evaluated.
  int num_blocks = _code->length();
  for (int i = 0; i < num_blocks; i++) {
    _code->at(i)->end()->state()->pin_stack_for_linear_scan();
  }

  // compute use counts
  UseCountComputer::compute(_code);
}

void IR::iterate_preorder(BlockClosure* closure) {
  start()->iterate_preorder(closure);
}

void IR::iterate_postorder(BlockClosure* closure) {
  start()->iterate_postorder(closure);
}

void IR::iterate_linear_scan_order(BlockClosure* closure) {
  linear_scan_order()->iterate_forward(closure);
}

void SubstitutionResolver::visit(Value* v) {
  Value v0 = *v;
  if (v0) {
    Value vs = v0->subst();
    if (vs != v0) {
      *v = v0->subst();
    }
  }
}

void SubstitutionResolver::block_do(BlockBegin* block) {
  Instruction* last = NULL;
  for (Instruction* n = block; n != NULL;) {
    n->values_do(this);
    // need to remove this instruction from the instruction stream
    if (n->subst() != n) {
      guarantee(last != NULL, "must have last");
      last->set_next(n->next());
    } else {
      last = n;
    }
    n = last->next();
  }
}

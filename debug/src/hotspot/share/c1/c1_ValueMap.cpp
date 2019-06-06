#include "precompiled.hpp"

#include "c1/c1_Canonicalizer.hpp"
#include "c1/c1_IR.hpp"
#include "c1/c1_ValueMap.hpp"
#include "c1/c1_ValueSet.inline.hpp"
#include "c1/c1_ValueStack.hpp"

ValueMap::ValueMap()
  : _nesting(0)
  , _entries(ValueMapInitialSize, ValueMapInitialSize, NULL)
  , _killed_values()
  , _entry_count(0)
{ }

ValueMap::ValueMap(ValueMap* old)
  : _nesting(old->_nesting + 1)
  , _entries(old->_entries.length(), old->_entries.length(), NULL)
  , _killed_values()
  , _entry_count(old->_entry_count)
{
  for (int i = size() - 1; i >= 0; i--) {
    _entries.at_put(i, old->entry_at(i));
  }
  _killed_values.set_from(&old->_killed_values);
}

void ValueMap::increase_table_size() {
  int old_size = size();
  int new_size = old_size * 2 + 1;

  ValueMapEntryList worklist(8);
  ValueMapEntryArray new_entries(new_size, new_size, NULL);
  int new_entry_count = 0;

  for (int i = old_size - 1; i >= 0; i--) {
    ValueMapEntry* entry;
    for (entry = entry_at(i); entry != NULL; entry = entry->next()) {
      if (!is_killed(entry->value())) {
        worklist.push(entry);
      }
    }

    while (!worklist.is_empty()) {
      entry = worklist.pop();
      int new_index = entry_index(entry->hash(), new_size);

      if (entry->nesting() != nesting() && new_entries.at(new_index) != entry->next()) {
        // changing entries with a lower nesting than the current nesting of the table
        // is not allowed because then the same entry is contained in multiple value maps.
        // clone entry when next-pointer must be changed
        entry = new ValueMapEntry(entry->hash(), entry->value(), entry->nesting(), NULL);
      }
      entry->set_next(new_entries.at(new_index));
      new_entries.at_put(new_index, entry);
      new_entry_count++;
    }
  }

  _entries = new_entries;
  _entry_count = new_entry_count;
}

Value ValueMap::find_insert(Value x) {
  const intx hash = x->hash();
  if (hash != 0) {
    for (ValueMapEntry* entry = entry_at(entry_index(hash, size())); entry != NULL; entry = entry->next()) {
      if (entry->hash() == hash) {
        Value f = entry->value();

        if (!is_killed(f) && f->is_equal(x)) {
          if (entry->nesting() != nesting() && f->as_Constant() == NULL) {
            // non-constant values of of another block must be pinned,
            // otherwise it is possible that they are not evaluated
            f->pin(Instruction::PinGlobalValueNumbering);
          }

          return f;
        }
      }
    }

    // x not found, so insert it
    if (entry_count() >= size_threshold()) {
      increase_table_size();
    }
    int idx = entry_index(hash, size());
    _entries.at_put(idx, new ValueMapEntry(hash, x, nesting(), entry_at(idx)));
    _entry_count++;
  }

  return x;
}

#define GENERIC_KILL_VALUE(must_kill_implementation) \
 \
  for (int i = size() - 1; i >= 0; i--) { \
    ValueMapEntry* prev_entry = NULL; \
    for (ValueMapEntry* entry = entry_at(i); entry != NULL; entry = entry->next()) { \
      Value value = entry->value(); \
 \
      must_kill_implementation(must_kill, entry, value) \
 \
      if (must_kill) { \
        kill_value(value); \
 \
        if (prev_entry == NULL) { \
          _entries.at_put(i, entry->next()); \
          _entry_count--; \
        } else if (prev_entry->nesting() == nesting()) { \
          prev_entry->set_next(entry->next()); \
          _entry_count--; \
        } else { \
          prev_entry = entry; \
        } \
      } else { \
        prev_entry = entry; \
      } \
    } \
  }

#define MUST_KILL_MEMORY(must_kill, entry, value) \
  bool must_kill = value->as_LoadField() != NULL || value->as_LoadIndexed() != NULL;

#define MUST_KILL_ARRAY(must_kill, entry, value) \
  bool must_kill = value->as_LoadIndexed() != NULL \
                   && value->type()->tag() == type->tag();

#define MUST_KILL_FIELD(must_kill, entry, value) \
  /* ciField's are not unique; must compare their contents */ \
  LoadField* lf = value->as_LoadField(); \
  bool must_kill = lf != NULL \
                   && lf->field()->holder() == field->holder() \
                   && (all_offsets || lf->field()->offset() == field->offset());

void ValueMap::kill_memory() {
  GENERIC_KILL_VALUE(MUST_KILL_MEMORY);
}

void ValueMap::kill_array(ValueType* type) {
  GENERIC_KILL_VALUE(MUST_KILL_ARRAY);
}

void ValueMap::kill_field(ciField* field, bool all_offsets) {
  GENERIC_KILL_VALUE(MUST_KILL_FIELD);
}

void ValueMap::kill_map(ValueMap* map) {
  _killed_values.set_union(&map->_killed_values);
}

void ValueMap::kill_all() {
  for (int i = size() - 1; i >= 0; i--) {
    _entries.at_put(i, NULL);
  }
  _entry_count = 0;
}

class ShortLoopOptimizer : public ValueNumberingVisitor {
 private:
  GlobalValueNumbering* _gvn;
  BlockList             _loop_blocks;
  bool                  _too_complicated_loop;
  bool                  _has_field_store[T_ARRAY + 1];
  bool                  _has_indexed_store[T_ARRAY + 1];

  // simplified access to methods of GlobalValueNumbering
  ValueMap* current_map()                        { return _gvn->current_map(); }
  ValueMap* value_map_of(BlockBegin* block)      { return _gvn->value_map_of(block); }

  // implementation for abstract methods of ValueNumberingVisitor
  void      kill_memory()                                { _too_complicated_loop = true; }
  void      kill_field(ciField* field, bool all_offsets) {
    current_map()->kill_field(field, all_offsets);
    _has_field_store[field->type()->basic_type()] = true;
  }
  void      kill_array(ValueType* type)                  {
    current_map()->kill_array(type);
    BasicType basic_type = as_BasicType(type);
    _has_indexed_store[basic_type] = true;
  }

 public:
  ShortLoopOptimizer(GlobalValueNumbering* gvn)
    : _gvn(gvn)
    , _loop_blocks(ValueMapMaxLoopSize)
    , _too_complicated_loop(false)
  {
    for (int i = 0; i<= T_ARRAY; i++) {
      _has_field_store[i] = false;
      _has_indexed_store[i] = false;
    }
  }

  bool has_field_store(BasicType type) {
    return _has_field_store[type];
  }

  bool has_indexed_store(BasicType type) {
    return _has_indexed_store[type];
  }

  bool process(BlockBegin* loop_header);
};

class LoopInvariantCodeMotion : public StackObj {
 private:
  GlobalValueNumbering* _gvn;
  ShortLoopOptimizer*   _short_loop_optimizer;
  Instruction*          _insertion_point;
  ValueStack *          _state;
  bool                  _insert_is_pred;

  void set_invariant(Value v) const    { _gvn->set_processed(v); }
  bool is_invariant(Value v) const     { return _gvn->is_processed(v); }

  void process_block(BlockBegin* block);

 public:
  LoopInvariantCodeMotion(ShortLoopOptimizer *slo, GlobalValueNumbering* gvn, BlockBegin* loop_header, BlockList* loop_blocks);
};

LoopInvariantCodeMotion::LoopInvariantCodeMotion(ShortLoopOptimizer *slo, GlobalValueNumbering* gvn, BlockBegin* loop_header, BlockList* loop_blocks)
  : _gvn(gvn), _short_loop_optimizer(slo) {
  BlockBegin* insertion_block = loop_header->dominator();
  if (insertion_block->number_of_preds() == 0) {
    return;  // only the entry block does not have a predecessor
  }

  _insertion_point = insertion_block->end()->prev();
  _insert_is_pred = loop_header->is_predecessor(insertion_block);

  BlockEnd *block_end = insertion_block->end();
  _state = block_end->state_before();

  if (!_state) {
    // If, TableSwitch and LookupSwitch always have state_before when
    // loop invariant code motion happens..
    _state = block_end->state();
  }

  // the loop_blocks are filled by going backward from the loop header, so this processing order is best
  process_block(loop_header);
  for (int i = loop_blocks->length() - 1; i >= 1; i--) {
    process_block(loop_blocks->at(i));
  }
}

void LoopInvariantCodeMotion::process_block(BlockBegin* block) {
  Instruction* prev = block;
  Instruction* cur = block->next();

  while (cur != NULL) {
    // determine if cur instruction is loop invariant
    // only selected instruction types are processed here
    bool cur_invariant = false;

    if (cur->as_Constant() != NULL) {
      cur_invariant = !cur->can_trap();
    } else if (cur->as_ArithmeticOp() != NULL || cur->as_LogicOp() != NULL || cur->as_ShiftOp() != NULL) {
      Op2* op2 = (Op2*)cur;
      cur_invariant = !op2->can_trap() && is_invariant(op2->x()) && is_invariant(op2->y());
    } else if (cur->as_LoadField() != NULL) {
      LoadField* lf = (LoadField*)cur;
      // deoptimizes on NullPointerException
      cur_invariant = !lf->needs_patching() && !lf->field()->is_volatile() && !_short_loop_optimizer->has_field_store(lf->field()->type()->basic_type()) && is_invariant(lf->obj()) && _insert_is_pred;
    } else if (cur->as_ArrayLength() != NULL) {
      ArrayLength *length = cur->as_ArrayLength();
      cur_invariant = is_invariant(length->array());
    } else if (cur->as_LoadIndexed() != NULL) {
      LoadIndexed *li = (LoadIndexed *)cur->as_LoadIndexed();
      cur_invariant = !_short_loop_optimizer->has_indexed_store(as_BasicType(cur->type())) && is_invariant(li->array()) && is_invariant(li->index()) && _insert_is_pred;
    }

    if (cur_invariant) {
      // perform value numbering and mark instruction as loop-invariant
      _gvn->substitute(cur);

      if (cur->as_Constant() == NULL) {
        // ensure that code for non-constant instructions is always generated
        cur->pin();
      }

      // remove cur instruction from loop block and append it to block before loop
      Instruction* next = cur->next();
      Instruction* in = _insertion_point->next();
      _insertion_point = _insertion_point->set_next(cur);
      cur->set_next(in);

      //  Deoptimize on exception
      cur->set_flag(Instruction::DeoptimizeOnException, true);

      //  Clear exception handlers
      cur->set_exception_handlers(NULL);

      if (cur->state_before() != NULL) {
        cur->set_state_before(_state->copy());
      }
      if (cur->exception_state() != NULL) {
        cur->set_exception_state(_state->copy());
      }

      cur = prev->set_next(next);
    } else {
      prev = cur;
      cur = cur->next();
    }
  }
}

bool ShortLoopOptimizer::process(BlockBegin* loop_header) {
  _too_complicated_loop = false;
  _loop_blocks.clear();
  _loop_blocks.append(loop_header);

  for (int i = 0; i < _loop_blocks.length(); i++) {
    BlockBegin* block = _loop_blocks.at(i);

    if (block->is_set(BlockBegin::exception_entry_flag)) {
      // this would be too complicated
      return false;
    }

    // add predecessors to worklist
    for (int j = block->number_of_preds() - 1; j >= 0; j--) {
      BlockBegin* pred = block->pred_at(j);

      if (pred->is_set(BlockBegin::osr_entry_flag)) {
        return false;
      }

      ValueMap* pred_map = value_map_of(pred);
      if (pred_map != NULL) {
        current_map()->kill_map(pred_map);
      } else if (!_loop_blocks.contains(pred)) {
        if (_loop_blocks.length() >= ValueMapMaxLoopSize) {
          return false;
        }
        _loop_blocks.append(pred);
      }
    }

    // use the instruction visitor for killing values
    for (Value instr = block->next(); instr != NULL; instr = instr->next()) {
      instr->visit(this);
      if (_too_complicated_loop) {
        return false;
      }
    }
  }

  bool optimistic = this->_gvn->compilation()->is_optimistic();

  if (UseLoopInvariantCodeMotion && optimistic) {
    LoopInvariantCodeMotion code_motion(this, _gvn, loop_header, &_loop_blocks);
  }

  return true;
}

GlobalValueNumbering::GlobalValueNumbering(IR* ir)
  : _current_map(NULL)
  , _value_maps(ir->linear_scan_order()->length(), ir->linear_scan_order()->length(), NULL)
  , _compilation(ir->compilation())
{
  ShortLoopOptimizer short_loop_optimizer(this);

  BlockList* blocks = ir->linear_scan_order();
  int num_blocks = blocks->length();

  BlockBegin* start_block = blocks->at(0);

  // method parameters are not linked in instructions list, so process them separateley
  for_each_state_value(start_block->state(), value,
     set_processed(value);
  );

  // initial, empty value map with nesting 0
  set_value_map_of(start_block, new ValueMap());

  for (int i = 1; i < num_blocks; i++) {
    BlockBegin* block = blocks->at(i);

    int num_preds = block->number_of_preds();

    BlockBegin* dominator = block->dominator();

    // create new value map with increased nesting
    _current_map = new ValueMap(value_map_of(dominator));

    if (num_preds == 1 && !block->is_set(BlockBegin::exception_entry_flag)) {
      // nothing to do here
    } else if (block->is_set(BlockBegin::linear_scan_loop_header_flag)) {
      // block has incoming backward branches -> try to optimize short loops
      if (!short_loop_optimizer.process(block)) {
        // loop is too complicated, so kill all memory loads because there might be
        // stores to them in the loop
        current_map()->kill_memory();
      }
    } else {
      // only incoming forward branches that are already processed
      for (int j = 0; j < num_preds; j++) {
        BlockBegin* pred = block->pred_at(j);
        ValueMap* pred_map = value_map_of(pred);

        if (pred_map != NULL) {
          // propagate killed values of the predecessor to this block
          current_map()->kill_map(value_map_of(pred));
        } else {
          // kill all memory loads because predecessor not yet processed
          // (this can happen with non-natural loops and OSR-compiles)
          current_map()->kill_memory();
        }
      }
    }

    // phi functions are not linked in instructions list, so process them separateley
    for_each_phi_fun(block, phi,
      set_processed(phi);
    );

    // visit all instructions of this block
    for (Value instr = block->next(); instr != NULL; instr = instr->next()) {
      // check if instruction kills any values
      instr->visit(this);
      // perform actual value numbering
      substitute(instr);
    }

    // remember value map for successors
    set_value_map_of(block, current_map());
  }

  if (_has_substitutions) {
    SubstitutionResolver resolver(ir);
  }
}

void GlobalValueNumbering::substitute(Instruction* instr) {
  Value subst = current_map()->find_insert(instr);
  if (subst != instr) {
    instr->set_subst(subst);
    _has_substitutions = true;
  }
  set_processed(instr);
}

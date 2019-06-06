#include "precompiled.hpp"

#include "ci/ciArrayKlass.hpp"
#include "ci/ciEnv.hpp"
#include "ci/ciKlass.hpp"
#include "ci/ciMethod.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "code/dependencies.hpp"
#include "compiler/compileBroker.hpp"
#include "compiler/compileTask.hpp"
#include "memory/resourceArea.hpp"
#include "oops/klass.hpp"
#include "oops/oop.inline.hpp"
#include "oops/objArrayKlass.hpp"
#include "runtime/flags/flagSetting.hpp"
#include "runtime/handles.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/copy.hpp"

void Dependencies::initialize(ciEnv* env) {
  Arena* arena = env->arena();
  _oop_recorder = env->oop_recorder();
  _dep_seen = new(arena) GrowableArray<int>(arena, 500, 0, 0);
  _using_dep_values = false;
  for (int i = (int)FIRST_TYPE; i < (int)TYPE_LIMIT; i++) {
    _deps[i] = new(arena) GrowableArray<ciBaseObject*>(arena, 10, 0, 0);
  }
  _content_bytes = NULL;
  _size_in_bytes = (size_t)-1;
}

void Dependencies::assert_evol_method(ciMethod* m) {
  assert_common_1(evol_method, m);
}

void Dependencies::assert_leaf_type(ciKlass* ctxk) {
  if (ctxk->is_array_klass()) {
    // As a special case, support this assertion on an array type,
    // which reduces to an assertion on its element type.
    // Note that this cannot be done with assertions that
    // relate to concreteness or abstractness.
    ciType* elemt = ctxk->as_array_klass()->base_element_type();
    if (!elemt->is_instance_klass())  return;   // Ex:  int[][]
    ctxk = elemt->as_instance_klass();
    //if (ctxk->is_final())  return;            // Ex:  String[][]
  }
  check_ctxk(ctxk);
  assert_common_1(leaf_type, ctxk);
}

void Dependencies::assert_abstract_with_unique_concrete_subtype(ciKlass* ctxk, ciKlass* conck) {
  check_ctxk_abstract(ctxk);
  assert_common_2(abstract_with_unique_concrete_subtype, ctxk, conck);
}

void Dependencies::assert_abstract_with_no_concrete_subtype(ciKlass* ctxk) {
  check_ctxk_abstract(ctxk);
  assert_common_1(abstract_with_no_concrete_subtype, ctxk);
}

void Dependencies::assert_concrete_with_no_concrete_subtype(ciKlass* ctxk) {
  check_ctxk_concrete(ctxk);
  assert_common_1(concrete_with_no_concrete_subtype, ctxk);
}

void Dependencies::assert_unique_concrete_method(ciKlass* ctxk, ciMethod* uniqm) {
  check_ctxk(ctxk);
  assert_common_2(unique_concrete_method, ctxk, uniqm);
}

void Dependencies::assert_abstract_with_exclusive_concrete_subtypes(ciKlass* ctxk, ciKlass* k1, ciKlass* k2) {
  check_ctxk(ctxk);
  assert_common_3(abstract_with_exclusive_concrete_subtypes_2, ctxk, k1, k2);
}

void Dependencies::assert_exclusive_concrete_methods(ciKlass* ctxk, ciMethod* m1, ciMethod* m2) {
  check_ctxk(ctxk);
  assert_common_3(exclusive_concrete_methods_2, ctxk, m1, m2);
}

void Dependencies::assert_has_no_finalizable_subclasses(ciKlass* ctxk) {
  check_ctxk(ctxk);
  assert_common_1(no_finalizable_subclasses, ctxk);
}

void Dependencies::assert_call_site_target_value(ciCallSite* call_site, ciMethodHandle* method_handle) {
  assert_common_2(call_site_target_value, call_site, method_handle);
}

Dependencies::Dependencies(Arena* arena, OopRecorder* oop_recorder) {
  _oop_recorder = oop_recorder;
  _dep_seen = new(arena) GrowableArray<int>(arena, 500, 0, 0);
  _using_dep_values = true;
  for (int i = (int)FIRST_TYPE; i < (int)TYPE_LIMIT; i++) {
    _dep_values[i] = new(arena) GrowableArray<DepValue>(arena, 10, 0, DepValue());
  }
  _content_bytes = NULL;
  _size_in_bytes = (size_t)-1;
}

void Dependencies::assert_evol_method(Method* m) {
  assert_common_1(evol_method, DepValue(_oop_recorder, m));
}

void Dependencies::assert_has_no_finalizable_subclasses(Klass* ctxk) {
  check_ctxk(ctxk);
  assert_common_1(no_finalizable_subclasses, DepValue(_oop_recorder, ctxk));
}

void Dependencies::assert_leaf_type(Klass* ctxk) {
  if (ctxk->is_array_klass()) {
    // As a special case, support this assertion on an array type,
    // which reduces to an assertion on its element type.
    // Note that this cannot be done with assertions that
    // relate to concreteness or abstractness.
    BasicType elemt = ArrayKlass::cast(ctxk)->element_type();
    if (is_java_primitive(elemt))  return;   // Ex:  int[][]
    ctxk = ObjArrayKlass::cast(ctxk)->bottom_klass();
    //if (ctxk->is_final())  return;            // Ex:  String[][]
  }
  check_ctxk(ctxk);
  assert_common_1(leaf_type, DepValue(_oop_recorder, ctxk));
}

void Dependencies::assert_abstract_with_unique_concrete_subtype(Klass* ctxk, Klass* conck) {
  check_ctxk_abstract(ctxk);
  DepValue ctxk_dv(_oop_recorder, ctxk);
  DepValue conck_dv(_oop_recorder, conck, &ctxk_dv);
  assert_common_2(abstract_with_unique_concrete_subtype, ctxk_dv, conck_dv);
}

void Dependencies::assert_unique_concrete_method(Klass* ctxk, Method* uniqm) {
  check_ctxk(ctxk);
  assert_common_2(unique_concrete_method, DepValue(_oop_recorder, ctxk), DepValue(_oop_recorder, uniqm));
}

void Dependencies::assert_call_site_target_value(oop call_site, oop method_handle) {
  assert_common_2(call_site_target_value, DepValue(_oop_recorder, JNIHandles::make_local(call_site)), DepValue(_oop_recorder, JNIHandles::make_local(method_handle)));
}

// Helper function.  If we are adding a new dep. under ctxk2,
// try to find an old dep. under a broader* ctxk1.  If there is
//
bool Dependencies::maybe_merge_ctxk(GrowableArray<ciBaseObject*>* deps, int ctxk_i, ciKlass* ctxk2) {
  ciKlass* ctxk1 = deps->at(ctxk_i)->as_metadata()->as_klass();
  if (ctxk2->is_subtype_of(ctxk1)) {
    return true;  // success, and no need to change
  } else if (ctxk1->is_subtype_of(ctxk2)) {
    // new context class fully subsumes previous one
    deps->at_put(ctxk_i, ctxk2);
    return true;
  } else {
    return false;
  }
}

void Dependencies::assert_common_1(DepType dept, ciBaseObject* x) {
  GrowableArray<ciBaseObject*>* deps = _deps[dept];

  // see if the same (or a similar) dep is already recorded
  if (note_dep_seen(dept, x)) {
  } else {
    deps->append(x);
  }
}

void Dependencies::assert_common_2(DepType dept, ciBaseObject* x0, ciBaseObject* x1) {
  GrowableArray<ciBaseObject*>* deps = _deps[dept];

  // see if the same (or a similar) dep is already recorded
  bool has_ctxk = has_explicit_context_arg(dept);
  if (has_ctxk) {
    if (note_dep_seen(dept, x1)) {
      // look in this bucket for redundant assertions
      const int stride = 2;
      for (int i = deps->length(); (i -= stride) >= 0; ) {
        ciBaseObject* y1 = deps->at(i + 1);
        if (x1 == y1) {  // same subject; check the context
          if (maybe_merge_ctxk(deps, i + 0, x0->as_metadata()->as_klass())) {
            return;
          }
        }
      }
    }
  } else {
    if (note_dep_seen(dept, x0) && note_dep_seen(dept, x1)) {
      // look in this bucket for redundant assertions
      const int stride = 2;
      for (int i = deps->length(); (i -= stride) >= 0; ) {
        ciBaseObject* y0 = deps->at(i + 0);
        ciBaseObject* y1 = deps->at(i + 1);
        if (x0 == y0 && x1 == y1) {
          return;
        }
      }
    }
  }

  // append the assertion in the correct bucket:
  deps->append(x0);
  deps->append(x1);
}

void Dependencies::assert_common_3(DepType dept, ciKlass* ctxk, ciBaseObject* x, ciBaseObject* x2) {
  GrowableArray<ciBaseObject*>* deps = _deps[dept];

  // try to normalize an unordered pair:
  bool swap = false;
  switch (dept) {
  case abstract_with_exclusive_concrete_subtypes_2:
    swap = (x->ident() > x2->ident() && x->as_metadata()->as_klass() != ctxk);
    break;
  case exclusive_concrete_methods_2:
    swap = (x->ident() > x2->ident() && x->as_metadata()->as_method()->holder() != ctxk);
    break;
  default:
    break;
  }
  if (swap) { ciBaseObject* t = x; x = x2; x2 = t; }

  // see if the same (or a similar) dep is already recorded
  if (note_dep_seen(dept, x) && note_dep_seen(dept, x2)) {
    // look in this bucket for redundant assertions
    const int stride = 3;
    for (int i = deps->length(); (i -= stride) >= 0; ) {
      ciBaseObject* y  = deps->at(i + 1);
      ciBaseObject* y2 = deps->at(i + 2);
      if (x == y && x2 == y2) {  // same subjects; check the context
        if (maybe_merge_ctxk(deps, i + 0, ctxk)) {
          return;
        }
      }
    }
  }
  // append the assertion in the correct bucket:
  deps->append(ctxk);
  deps->append(x);
  deps->append(x2);
}

bool Dependencies::maybe_merge_ctxk(GrowableArray<DepValue>* deps, int ctxk_i, DepValue ctxk2_dv) {
  Klass* ctxk1 = deps->at(ctxk_i).as_klass(_oop_recorder);
  Klass* ctxk2 = ctxk2_dv.as_klass(_oop_recorder);
  if (ctxk2->is_subtype_of(ctxk1)) {
    return true;  // success, and no need to change
  } else if (ctxk1->is_subtype_of(ctxk2)) {
    // new context class fully subsumes previous one
    deps->at_put(ctxk_i, ctxk2_dv);
    return true;
  } else {
    return false;
  }
}

void Dependencies::assert_common_1(DepType dept, DepValue x) {
  GrowableArray<DepValue>* deps = _dep_values[dept];

  // see if the same (or a similar) dep is already recorded
  if (note_dep_seen(dept, x)) {
  } else {
    deps->append(x);
  }
}

void Dependencies::assert_common_2(DepType dept, DepValue x0, DepValue x1) {
  GrowableArray<DepValue>* deps = _dep_values[dept];

  // see if the same (or a similar) dep is already recorded
  bool has_ctxk = has_explicit_context_arg(dept);
  if (has_ctxk) {
    if (note_dep_seen(dept, x1)) {
      // look in this bucket for redundant assertions
      const int stride = 2;
      for (int i = deps->length(); (i -= stride) >= 0; ) {
        DepValue y1 = deps->at(i + 1);
        if (x1 == y1) {  // same subject; check the context
          if (maybe_merge_ctxk(deps, i + 0, x0)) {
            return;
          }
        }
      }
    }
  } else {
    if (note_dep_seen(dept, x0) && note_dep_seen(dept, x1)) {
      // look in this bucket for redundant assertions
      const int stride = 2;
      for (int i = deps->length(); (i -= stride) >= 0; ) {
        DepValue y0 = deps->at(i + 0);
        DepValue y1 = deps->at(i + 1);
        if (x0 == y0 && x1 == y1) {
          return;
        }
      }
    }
  }

  // append the assertion in the correct bucket:
  deps->append(x0);
  deps->append(x1);
}

/// Support for encoding dependencies into an nmethod:

void Dependencies::copy_to(nmethod* nm) {
  address beg = nm->dependencies_begin();
  address end = nm->dependencies_end();
  guarantee(end - beg >= (ptrdiff_t) size_in_bytes(), "bad sizing");
  Copy::disjoint_words((HeapWord*) content_bytes(), (HeapWord*) beg, size_in_bytes() / sizeof(HeapWord));
}

static int sort_dep(ciBaseObject** p1, ciBaseObject** p2, int narg) {
  for (int i = 0; i < narg; i++) {
    int diff = p1[i]->ident() - p2[i]->ident();
    if (diff != 0)  return diff;
  }
  return 0;
}
static int sort_dep_arg_1(ciBaseObject** p1, ciBaseObject** p2) { return sort_dep(p1, p2, 1); }
static int sort_dep_arg_2(ciBaseObject** p1, ciBaseObject** p2) { return sort_dep(p1, p2, 2); }
static int sort_dep_arg_3(ciBaseObject** p1, ciBaseObject** p2) { return sort_dep(p1, p2, 3); }

// metadata deps are sorted before object deps
static int sort_dep_value(Dependencies::DepValue* p1, Dependencies::DepValue* p2, int narg) {
  for (int i = 0; i < narg; i++) {
    int diff = p1[i].sort_key() - p2[i].sort_key();
    if (diff != 0)  return diff;
  }
  return 0;
}
static int sort_dep_value_arg_1(Dependencies::DepValue* p1, Dependencies::DepValue* p2) { return sort_dep_value(p1, p2, 1); }
static int sort_dep_value_arg_2(Dependencies::DepValue* p1, Dependencies::DepValue* p2) { return sort_dep_value(p1, p2, 2); }
static int sort_dep_value_arg_3(Dependencies::DepValue* p1, Dependencies::DepValue* p2) { return sort_dep_value(p1, p2, 3); }

void Dependencies::sort_all_deps() {
  if (_using_dep_values) {
    for (int deptv = (int)FIRST_TYPE; deptv < (int)TYPE_LIMIT; deptv++) {
      DepType dept = (DepType)deptv;
      GrowableArray<DepValue>* deps = _dep_values[dept];
      if (deps->length() <= 1)  continue;
      switch (dep_args(dept)) {
      case 1: deps->sort(sort_dep_value_arg_1, 1); break;
      case 2: deps->sort(sort_dep_value_arg_2, 2); break;
      case 3: deps->sort(sort_dep_value_arg_3, 3); break;
      default: ShouldNotReachHere(); break;
      }
    }
    return;
  }
  for (int deptv = (int)FIRST_TYPE; deptv < (int)TYPE_LIMIT; deptv++) {
    DepType dept = (DepType)deptv;
    GrowableArray<ciBaseObject*>* deps = _deps[dept];
    if (deps->length() <= 1)  continue;
    switch (dep_args(dept)) {
    case 1: deps->sort(sort_dep_arg_1, 1); break;
    case 2: deps->sort(sort_dep_arg_2, 2); break;
    case 3: deps->sort(sort_dep_arg_3, 3); break;
    default: ShouldNotReachHere(); break;
    }
  }
}

size_t Dependencies::estimate_size_in_bytes() {
  size_t est_size = 100;
  if (_using_dep_values) {
    for (int deptv = (int)FIRST_TYPE; deptv < (int)TYPE_LIMIT; deptv++) {
      DepType dept = (DepType)deptv;
      GrowableArray<DepValue>* deps = _dep_values[dept];
      est_size += deps->length() * 2;  // tags and argument(s)
    }
    return est_size;
  }
  for (int deptv = (int)FIRST_TYPE; deptv < (int)TYPE_LIMIT; deptv++) {
    DepType dept = (DepType)deptv;
    GrowableArray<ciBaseObject*>* deps = _deps[dept];
    est_size += deps->length()*2;  // tags and argument(s)
  }
  return est_size;
}

ciKlass* Dependencies::ctxk_encoded_as_null(DepType dept, ciBaseObject* x) {
  switch (dept) {
  case abstract_with_exclusive_concrete_subtypes_2:
    return x->as_metadata()->as_klass();
  case unique_concrete_method:
  case exclusive_concrete_methods_2:
    return x->as_metadata()->as_method()->holder();
  default:
    return NULL;  // let NULL be NULL
  }
}

Klass* Dependencies::ctxk_encoded_as_null(DepType dept, Metadata* x) {
  switch (dept) {
  case abstract_with_exclusive_concrete_subtypes_2:
    return (Klass*) x;
  case unique_concrete_method:
  case exclusive_concrete_methods_2:
    return ((Method*)x)->method_holder();
  default:
    return NULL;  // let NULL be NULL
  }
}

void Dependencies::encode_content_bytes() {
  sort_all_deps();

  // cast is safe, no deps can overflow INT_MAX
  CompressedWriteStream bytes((int)estimate_size_in_bytes());

  if (_using_dep_values) {
    for (int deptv = (int)FIRST_TYPE; deptv < (int)TYPE_LIMIT; deptv++) {
      DepType dept = (DepType)deptv;
      GrowableArray<DepValue>* deps = _dep_values[dept];
      if (deps->length() == 0)  continue;
      int stride = dep_args(dept);
      int ctxkj  = dep_context_arg(dept);  // -1 if no context arg
      for (int i = 0; i < deps->length(); i += stride) {
        jbyte code_byte = (jbyte)dept;
        int skipj = -1;
        if (ctxkj >= 0 && ctxkj + 1 < stride) {
          Klass*  ctxk = deps->at(i + ctxkj + 0).as_klass(_oop_recorder);
          DepValue x = deps->at(i + ctxkj + 1);  // following argument
          if (ctxk == ctxk_encoded_as_null(dept, x.as_metadata(_oop_recorder))) {
            skipj = ctxkj;  // we win:  maybe one less oop to keep track of
            code_byte |= default_context_type_bit;
          }
        }
        bytes.write_byte(code_byte);
        for (int j = 0; j < stride; j++) {
          if (j == skipj)  continue;
          DepValue v = deps->at(i+j);
          int idx = v.index();
          bytes.write_int(idx);
        }
      }
    }
  } else {
  for (int deptv = (int)FIRST_TYPE; deptv < (int)TYPE_LIMIT; deptv++) {
    DepType dept = (DepType)deptv;
    GrowableArray<ciBaseObject*>* deps = _deps[dept];
    if (deps->length() == 0)  continue;
    int stride = dep_args(dept);
    int ctxkj  = dep_context_arg(dept);  // -1 if no context arg
    for (int i = 0; i < deps->length(); i += stride) {
      jbyte code_byte = (jbyte)dept;
      int skipj = -1;
      if (ctxkj >= 0 && ctxkj + 1 < stride) {
        ciKlass*  ctxk = deps->at(i + ctxkj + 0)->as_metadata()->as_klass();
        ciBaseObject* x     = deps->at(i + ctxkj + 1);  // following argument
        if (ctxk == ctxk_encoded_as_null(dept, x)) {
          skipj = ctxkj;  // we win:  maybe one less oop to keep track of
          code_byte |= default_context_type_bit;
        }
      }
      bytes.write_byte(code_byte);
      for (int j = 0; j < stride; j++) {
        if (j == skipj)  continue;
        ciBaseObject* v = deps->at(i+j);
        int idx;
        if (v->is_object()) {
          idx = _oop_recorder->find_index(v->as_object()->constant_encoding());
        } else {
          ciMetadata* meta = v->as_metadata();
          idx = _oop_recorder->find_index(meta->constant_encoding());
        }
        bytes.write_int(idx);
      }
    }
  }
  }

  // write a sentinel byte to mark the end
  bytes.write_byte(end_marker);

  // round it out to a word boundary
  while (bytes.position() % sizeof(HeapWord) != 0) {
    bytes.write_byte(end_marker);
  }

  _content_bytes = bytes.buffer();
  _size_in_bytes = bytes.position();
}

const char* Dependencies::_dep_name[TYPE_LIMIT] = {
  "end_marker",
  "evol_method",
  "leaf_type",
  "abstract_with_unique_concrete_subtype",
  "abstract_with_no_concrete_subtype",
  "concrete_with_no_concrete_subtype",
  "unique_concrete_method",
  "abstract_with_exclusive_concrete_subtypes_2",
  "exclusive_concrete_methods_2",
  "no_finalizable_subclasses",
  "call_site_target_value"
};

int Dependencies::_dep_args[TYPE_LIMIT] = {
  -1,// end_marker
  1, // evol_method m
  1, // leaf_type ctxk
  2, // abstract_with_unique_concrete_subtype ctxk, k
  1, // abstract_with_no_concrete_subtype ctxk
  1, // concrete_with_no_concrete_subtype ctxk
  2, // unique_concrete_method ctxk, m
  3, // unique_concrete_subtypes_2 ctxk, k1, k2
  3, // unique_concrete_methods_2 ctxk, m1, m2
  1, // no_finalizable_subclasses ctxk
  2  // call_site_target_value call_site, method_handle
};

const char* Dependencies::dep_name(Dependencies::DepType dept) {
  if (!dept_in_mask(dept, all_types))  return "?bad-dep?";
  return _dep_name[dept];
}

int Dependencies::dep_args(Dependencies::DepType dept) {
  if (!dept_in_mask(dept, all_types))  return -1;
  return _dep_args[dept];
}

void Dependencies::check_valid_dependency_type(DepType dept) {
  guarantee(FIRST_TYPE <= dept && dept < TYPE_LIMIT, "invalid dependency type: %d", (int) dept);
}

Dependencies::DepType Dependencies::validate_dependencies(CompileTask* task, bool counter_changed, char** failure_detail) {
  // First, check non-klass dependencies as we might return early and
  // not check klass dependencies if the system dictionary
  // modification counter hasn't changed (see below).
  for (Dependencies::DepStream deps(this); deps.next(); ) {
    if (deps.is_klass_type())  continue;  // skip klass dependencies
    Klass* witness = deps.check_dependency();
    if (witness != NULL) {
      return deps.type();
    }
  }

  // Klass dependencies must be checked when the system dictionary
  // changes.  If logging is enabled all violated dependences will be
  // recorded in the log.  In debug mode check dependencies even if
  // the system dictionary hasn't changed to verify that no invalid
  // dependencies were inserted.  Any violated dependences in this
  // case are dumped to the tty.
  if (!counter_changed && !false) {
    return end_marker;
  }

  int klass_violations = 0;
  DepType result = end_marker;
  for (Dependencies::DepStream deps(this); deps.next(); ) {
    if (!deps.is_klass_type())  continue;  // skip non-klass dependencies
    Klass* witness = deps.check_dependency();
    if (witness != NULL) {
      if (klass_violations == 0) {
        result = deps.type();
      }
      klass_violations++;
      if (!counter_changed) {
        // Dependence failed but counter didn't change.
      } else if (xtty == NULL) {
        // If we're not logging then a single violation is sufficient,
        // otherwise we want to log all the dependences which were
        // violated.
        break;
      }
    }
  }

  return result;
}

/// Dependency stream support (decodes dependencies from an nmethod):

bool Dependencies::DepStream::next() {
  if (_bytes.position() == 0 && _code != NULL && _code->dependencies_size() == 0) {
    // Method has no dependencies at all.
    return false;
  }
  int code_byte = (_bytes.read_byte() & 0xFF);
  if (code_byte == end_marker) {
    return false;
  } else {
    int ctxk_bit = (code_byte & Dependencies::default_context_type_bit);
    code_byte -= ctxk_bit;
    DepType dept = (DepType)code_byte;
    _type = dept;
    Dependencies::check_valid_dependency_type(dept);
    int stride = _dep_args[dept];
    int skipj = -1;
    if (ctxk_bit != 0) {
      skipj = 0;  // currently the only context argument is at zero
    }
    for (int j = 0; j < stride; j++) {
      _xi[j] = (j == skipj) ? 0 : _bytes.read_int();
    }
    return true;
  }
}

inline Metadata* Dependencies::DepStream::recorded_metadata_at(int i) {
  Metadata* o = NULL;
  if (_code != NULL) {
    o = _code->metadata_at(i);
  } else {
    o = _deps->oop_recorder()->metadata_at(i);
  }
  return o;
}

inline oop Dependencies::DepStream::recorded_oop_at(int i) {
  return (_code != NULL)
         ? _code->oop_at(i)
    : JNIHandles::resolve(_deps->oop_recorder()->oop_at(i));
}

Metadata* Dependencies::DepStream::argument(int i) {
  Metadata* result = recorded_metadata_at(argument_index(i));

  if (result == NULL) { // Explicit context argument can be compressed
    int ctxkj = dep_context_arg(type());  // -1 if no explicit context arg
    if (ctxkj >= 0 && i == ctxkj && ctxkj + 1 < argument_count()) {
      result = ctxk_encoded_as_null(type(), argument(ctxkj + 1));
    }
  }

  return result;
}

/**
 * Returns a unique identifier for each dependency argument.
 */
uintptr_t Dependencies::DepStream::get_identifier(int i) {
  if (is_oop_argument(i)) {
    return (uintptr_t)(oopDesc*)argument_oop(i);
  } else {
    return (uintptr_t)argument(i);
  }
}

oop Dependencies::DepStream::argument_oop(int i) {
  oop result = recorded_oop_at(argument_index(i));
  return result;
}

Klass* Dependencies::DepStream::context_type() {
  // Most dependencies have an explicit context type argument.
  {
    int ctxkj = dep_context_arg(type());  // -1 if no explicit context arg
    if (ctxkj >= 0) {
      Metadata* k = argument(ctxkj);
      return (Klass*)k;
    }
  }

  // Some dependencies are using the klass of the first object
  // argument as implicit context type.
  {
    int ctxkj = dep_implicit_context_arg(type());
    if (ctxkj >= 0) {
      Klass* k = argument_oop(ctxkj)->klass();
      return (Klass*) k;
    }
  }

  // And some dependencies don't have a context type at all,
  // e.g. evol_method.
  return NULL;
}

// ----------------- DependencySignature --------------------------------------
bool DependencySignature::equals(DependencySignature const& s1, DependencySignature const& s2) {
  if ((s1.type() != s2.type()) || (s1.args_count() != s2.args_count())) {
    return false;
  }

  for (int i = 0; i < s1.args_count(); i++) {
    if (s1.arg(i) != s2.arg(i)) {
      return false;
    }
  }
  return true;
}

/// Checking dependencies:

// This hierarchy walker inspects subtypes of a given type,
// trying to find a "bad" class which breaks a dependency.
// Such a class is called a "witness" to the broken dependency.
// While searching around, we ignore "participants", which
// are already known to the dependency.
class ClassHierarchyWalker {
 public:
  enum { PARTICIPANT_LIMIT = 3 };

 private:
  // optional method descriptor to check for:
  Symbol* _name;
  Symbol* _signature;

  // special classes which are not allowed to be witnesses:
  Klass*    _participants[PARTICIPANT_LIMIT+1];
  int       _num_participants;

  // cache of method lookups
  Method* _found_methods[PARTICIPANT_LIMIT+1];

  // if non-zero, tells how many witnesses to convert to participants
  int       _record_witnesses;

  void initialize(Klass* participant) {
    _record_witnesses = 0;
    _participants[0]  = participant;
    _found_methods[0] = NULL;
    _num_participants = 0;
    if (participant != NULL) {
      // Terminating NULL.
      _participants[1] = NULL;
      _found_methods[1] = NULL;
      _num_participants = 1;
    }
  }

  void initialize_from_method(Method* m) {
    _name      = m->name();
    _signature = m->signature();
  }

 public:
  // The walker is initialized to recognize certain methods and/or types
  // as friendly participants.
  ClassHierarchyWalker(Klass* participant, Method* m) {
    initialize_from_method(m);
    initialize(participant);
  }
  ClassHierarchyWalker(Method* m) {
    initialize_from_method(m);
    initialize(NULL);
  }
  ClassHierarchyWalker(Klass* participant = NULL) {
    _name      = NULL;
    _signature = NULL;
    initialize(participant);
  }
  ClassHierarchyWalker(Klass* participants[], int num_participants) {
    _name      = NULL;
    _signature = NULL;
    initialize(NULL);
    for (int i = 0; i < num_participants; ++i) {
      add_participant(participants[i]);
    }
  }

  // This is common code for two searches:  One for concrete subtypes,
  // the other for concrete method implementations and overrides.
  bool doing_subtype_search() {
    return _name == NULL;
  }

  int num_participants() { return _num_participants; }
  Klass* participant(int n) {
    return _participants[n];
  }

  // Note:  If n == num_participants, returns NULL.
  Method* found_method(int n) {
    Method* fm = _found_methods[n];
    if (fm != NULL && fm->method_holder() != _participants[n]) {
      // Default methods from interfaces can be added to classes. In
      // that case the holder of the method is not the class but the
      // interface where it's defined.
      return NULL;
    }
    return fm;
  }

  void add_participant(Klass* participant) {
    int np = _num_participants++;
    _participants[np] = participant;
    _participants[np + 1] = NULL;
    _found_methods[np + 1] = NULL;
  }

  void record_witnesses(int add) {
    if (add > PARTICIPANT_LIMIT)  add = PARTICIPANT_LIMIT;
    _record_witnesses = add;
  }

  bool is_witness(Klass* k) {
    if (doing_subtype_search()) {
      return Dependencies::is_concrete_klass(k);
    } else if (!k->is_instance_klass()) {
      return false; // no methods to find in an array type
    } else {
      // Search class hierarchy first, skipping private implementations
      // as they never override any inherited methods
      Method* m = InstanceKlass::cast(k)->find_instance_method(_name, _signature, Klass::skip_private);
      if (!Dependencies::is_concrete_method(m, k)) {
        // Check for re-abstraction of method
        if (!k->is_interface() && m != NULL && m->is_abstract()) {
          // Found a matching abstract method 'm' in the class hierarchy.
          // This is fine iff 'k' is an abstract class and all concrete subtypes
          // of 'k' override 'm' and are participates of the current search.
          ClassHierarchyWalker wf(_participants, _num_participants);
          Klass* w = wf.find_witness_subtype(k);
          if (w != NULL) {
            Method* wm = InstanceKlass::cast(w)->find_instance_method(_name, _signature);
            if (!Dependencies::is_concrete_method(wm, w)) {
              // Found a concrete subtype 'w' which does not override abstract method 'm'.
              // Bail out because 'm' could be called with 'w' as receiver (leading to an
              // AbstractMethodError) and thus the method we are looking for is not unique.
              _found_methods[_num_participants] = m;
              return true;
            }
          }
        }
        // Check interface defaults also, if any exist.
        Array<Method*>* default_methods = InstanceKlass::cast(k)->default_methods();
        if (default_methods == NULL)
            return false;
        m = InstanceKlass::cast(k)->find_method(default_methods, _name, _signature);
        if (!Dependencies::is_concrete_method(m, NULL))
            return false;
      }
      _found_methods[_num_participants] = m;
      // Note:  If add_participant(k) is called,
      // the method m will already be memoized for it.
      return true;
    }
  }

  bool is_participant(Klass* k) {
    if (k == _participants[0]) {
      return true;
    } else if (_num_participants <= 1) {
      return false;
    } else {
      return in_list(k, &_participants[1]);
    }
  }
  bool ignore_witness(Klass* witness) {
    if (_record_witnesses == 0) {
      return false;
    } else {
      --_record_witnesses;
      add_participant(witness);
      return true;
    }
  }
  static bool in_list(Klass* x, Klass** list) {
    for (int i = 0; ; i++) {
      Klass* y = list[i];
      if (y == NULL)  break;
      if (y == x)  return true;
    }
    return false;  // not in list
  }

 private:
  // the actual search method:
  Klass* find_witness_anywhere(Klass* context_type, bool participants_hide_witnesses, bool top_level_call = true);
  // the spot-checking version:
  Klass* find_witness_in(KlassDepChange& changes, Klass* context_type, bool participants_hide_witnesses);
 public:
  Klass* find_witness_subtype(Klass* context_type, KlassDepChange* changes = NULL) {
    // When looking for unexpected concrete types,
    // do not look beneath expected ones.
    const bool participants_hide_witnesses = true;
    // CX > CC > C' is OK, even if C' is new.
    // CX > { CC,  C' } is not OK if C' is new, and C' is the witness.
    if (changes != NULL) {
      return find_witness_in(*changes, context_type, participants_hide_witnesses);
    } else {
      return find_witness_anywhere(context_type, participants_hide_witnesses);
    }
  }
  Klass* find_witness_definer(Klass* context_type, KlassDepChange* changes = NULL) {
    // When looking for unexpected concrete methods,
    // look beneath expected ones, to see if there are overrides.
    const bool participants_hide_witnesses = true;
    // CX.m > CC.m > C'.m is not OK, if C'.m is new, and C' is the witness.
    if (changes != NULL) {
      return find_witness_in(*changes, context_type, !participants_hide_witnesses);
    } else {
      return find_witness_anywhere(context_type, !participants_hide_witnesses);
    }
  }
};

#define count_find_witness_calls() (0)

Klass* ClassHierarchyWalker::find_witness_in(KlassDepChange& changes, Klass* context_type, bool participants_hide_witnesses) {
  Klass* new_type = changes.new_type();

  (void)count_find_witness_calls();

  int nof_impls = InstanceKlass::cast(context_type)->nof_implementors();
  if (nof_impls > 1) {
    // Avoid this case: *I.m > { A.m, C }; B.m > C
    // %%% Until this is fixed more systematically, bail out.
    // See corresponding comment in find_witness_anywhere.
    return context_type;
  }

  if (participants_hide_witnesses) {
    // If the new type is a subtype of a participant, we are done.
    for (int i = 0; i < num_participants(); i++) {
      Klass* part = participant(i);
      if (part == NULL)  continue;
      if (changes.involves_context(part)) {
        // new guy is protected from this check by previous participant
        return NULL;
      }
    }
  }

  if (is_witness(new_type) && !ignore_witness(new_type)) {
    return new_type;
  }

  return NULL;
}

// Walk hierarchy under a context type, looking for unexpected types.
// Do not report participant types, and recursively walk beneath
// them only if participants_hide_witnesses is false.
// If top_level_call is false, skip testing the context type,
// because the caller has already considered it.
Klass* ClassHierarchyWalker::find_witness_anywhere(Klass* context_type, bool participants_hide_witnesses, bool top_level_call) {
  bool do_counts = count_find_witness_calls();

  // Check the root of the sub-hierarchy first.
  if (top_level_call) {
    if (is_participant(context_type)) {
      if (participants_hide_witnesses)  return NULL;
      // else fall through to search loop...
    } else if (is_witness(context_type) && !ignore_witness(context_type)) {
      // The context is an abstract class or interface, to start with.
      return context_type;
    }
  }

  // Now we must check each implementor and each subclass.
  // Use a short worklist to avoid blowing the stack.
  // Each worklist entry is a *chain* of subklass siblings to process.
  const int CHAINMAX = 100;  // >= 1 + InstanceKlass::implementors_limit
  Klass* chains[CHAINMAX];
  int    chaini = 0;  // index into worklist
  Klass* chain;       // scratch variable
#define ADD_SUBCLASS_CHAIN(k) { chain = k->subklass(); if (chain != NULL) chains[chaini++] = chain; }

  // Look for non-abstract subclasses.
  // (Note:  Interfaces do not have subclasses.)
  ADD_SUBCLASS_CHAIN(context_type);

  // If it is an interface, search its direct implementors.
  // (Their subclasses are additional indirect implementors.
  // See InstanceKlass::add_implementor.)
  // (Note:  nof_implementors is always zero for non-interfaces.)
  if (top_level_call) {
    int nof_impls = InstanceKlass::cast(context_type)->nof_implementors();
    if (nof_impls > 1) {
      // Avoid this case: *I.m > { A.m, C }; B.m > C
      // Here, I.m has 2 concrete implementations, but m appears unique
      // as A.m, because the search misses B.m when checking C.
      // The inherited method B.m was getting missed by the walker
      // when interface 'I' was the starting point.
      // %%% Until this is fixed more systematically, bail out.
      // (Old CHA had the same limitation.)
      return context_type;
    }
    if (nof_impls > 0) {
      Klass* impl = InstanceKlass::cast(context_type)->implementor();
      // If impl is the same as the context_type, then more than one
      // implementor has seen. No exact info in this case.
      if (impl == context_type) {
        return context_type;  // report an inexact witness to this sad affair
      }
      if (is_participant(impl)) {
        if (!participants_hide_witnesses) {
          ADD_SUBCLASS_CHAIN(impl);
        }
      } else if (is_witness(impl) && !ignore_witness(impl)) {
        return impl;
      } else {
        ADD_SUBCLASS_CHAIN(impl);
      }
    }
  }

  // Recursively process each non-trivial sibling chain.
  while (chaini > 0) {
    Klass* chain = chains[--chaini];
    for (Klass* sub = chain; sub != NULL; sub = sub->next_sibling()) {
      if (is_participant(sub)) {
        if (participants_hide_witnesses)  continue;
        // else fall through to process this guy's subclasses
      } else if (is_witness(sub) && !ignore_witness(sub)) {
        return sub;
      }
      if (chaini < CHAINMAX) {
        // Fast path.  (Partially disabled if ...)
        ADD_SUBCLASS_CHAIN(sub);
      } else {
        // Worklist overflow.  Do a recursive call.  Should be rare.
        // The recursive call will have its own worklist, of course.
        // (Note that sub has already been tested, so that there is
        // no need for the recursive call to re-test.  That's handy,
        // since the recursive call sees sub as the context_type.)
        Klass* witness = find_witness_anywhere(sub, participants_hide_witnesses, /*top_level_call=*/ false);
        if (witness != NULL)  return witness;
      }
    }
  }

  // No witness found.  The dependency remains unbroken.
  return NULL;
#undef ADD_SUBCLASS_CHAIN
}

bool Dependencies::is_concrete_klass(Klass* k) {
  if (k->is_abstract())  return false;
  // %%% We could treat classes which are concrete but
  // have not yet been instantiated as virtually abstract.
  // This would require a deoptimization barrier on first instantiation.
  //if (k->is_not_instantiated())  return false;
  return true;
}

bool Dependencies::is_concrete_method(Method* m, Klass * k) {
  // NULL is not a concrete method,
  // statics are irrelevant to virtual call sites,
  // abstract methods are not concrete,
  // overpass (error) methods are not concrete if k is abstract
  //
  // note "true" is conservative answer --
  //     overpass clause is false if k == NULL, implies return true if
  //     answer depends on overpass clause.
  return ! ( m == NULL || m -> is_static() || m -> is_abstract() || (m->is_overpass() && k != NULL && k -> is_abstract()));
}

Klass* Dependencies::find_finalizable_subclass(Klass* k) {
  if (k->is_interface())  return NULL;
  if (k->has_finalizer()) return k;
  k = k->subklass();
  while (k != NULL) {
    Klass* result = find_finalizable_subclass(k);
    if (result != NULL) return result;
    k = k->next_sibling();
  }
  return NULL;
}

bool Dependencies::is_concrete_klass(ciInstanceKlass* k) {
  if (k->is_abstract())  return false;
  // We could also return false if k does not yet appear to be
  // instantiated, if the VM version supports this distinction also.
  //if (k->is_not_instantiated())  return false;
  return true;
}

bool Dependencies::has_finalizable_subclass(ciInstanceKlass* k) {
  return k->has_finalizable_subclass();
}

// Any use of the contents (bytecodes) of a method must be
// marked by an "evol_method" dependency, if those contents
// can change.  (Note: A method is always dependent on itself.)
Klass* Dependencies::check_evol_method(Method* m) {
  // Did somebody do a JVMTI RedefineClasses while our backs were turned?
  // Or is there a now a breakpoint?
  // (Assumes compiled code cannot handle bkpts; change if UseFastBreakpoints.)
  if (m->is_old()) {
    return m->method_holder();
  } else {
    return NULL;
  }
}

// This is a strong assertion:  It is that the given type
// has no subtypes whatever.  It is most useful for
// optimizing checks on reflected types or on array types.
// (Checks on types which are derived from real instances
// can be optimized more strongly than this, because we
// know that the checked type comes from a concrete type,
// and therefore we can disregard abstract types.)
Klass* Dependencies::check_leaf_type(Klass* ctxk) {
  InstanceKlass* ctx = InstanceKlass::cast(ctxk);
  Klass* sub = ctx->subklass();
  if (sub != NULL) {
    return sub;
  } else if (ctx->nof_implementors() != 0) {
    // if it is an interface, it must be unimplemented
    // (if it is not an interface, nof_implementors is always zero)
    Klass* impl = ctx->implementor();
    return impl;
  } else {
    return NULL;
  }
}

// Test the assertion that conck is the only concrete subtype* of ctxk.
// The type conck itself is allowed to have have further concrete subtypes.
// This allows the compiler to narrow occurrences of ctxk by conck,
// when dealing with the types of actual instances.
Klass* Dependencies::check_abstract_with_unique_concrete_subtype(Klass* ctxk, Klass* conck, KlassDepChange* changes) {
  ClassHierarchyWalker wf(conck);
  return wf.find_witness_subtype(ctxk, changes);
}

// If a non-concrete class has no concrete subtypes, it is not (yet)
// instantiatable.  This can allow the compiler to make some paths go
// dead, if they are gated by a test of the type.
Klass* Dependencies::check_abstract_with_no_concrete_subtype(Klass* ctxk, KlassDepChange* changes) {
  // Find any concrete subtype, with no participants:
  ClassHierarchyWalker wf;
  return wf.find_witness_subtype(ctxk, changes);
}

// If a concrete class has no concrete subtypes, it can always be
// exactly typed.  This allows the use of a cheaper type test.
Klass* Dependencies::check_concrete_with_no_concrete_subtype(Klass* ctxk, KlassDepChange* changes) {
  // Find any concrete subtype, with only the ctxk as participant:
  ClassHierarchyWalker wf(ctxk);
  return wf.find_witness_subtype(ctxk, changes);
}

// Find the unique concrete proper subtype of ctxk, or NULL if there
// is more than one concrete proper subtype.  If there are no concrete
// proper subtypes, return ctxk itself, whether it is concrete or not.
// The returned subtype is allowed to have have further concrete subtypes.
// That is, return CC1 for CX > CC1 > CC2, but NULL for CX > { CC1, CC2 }.
Klass* Dependencies::find_unique_concrete_subtype(Klass* ctxk) {
  ClassHierarchyWalker wf(ctxk);   // Ignore ctxk when walking.
  wf.record_witnesses(1);          // Record one other witness when walking.
  Klass* wit = wf.find_witness_subtype(ctxk);
  if (wit != NULL)  return NULL;   // Too many witnesses.
  Klass* conck = wf.participant(0);
  if (conck == NULL) {
    return ctxk;                   // Return ctxk as a flag for "no subtypes".
  } else {
    return conck;
  }
}

// Test the assertion that the k[12] are the only concrete subtypes of ctxk,
// except possibly for further subtypes of k[12] themselves.
// The context type must be abstract.  The types k1 and k2 are themselves
// allowed to have further concrete subtypes.
Klass* Dependencies::check_abstract_with_exclusive_concrete_subtypes(Klass* ctxk, Klass* k1, Klass* k2, KlassDepChange* changes) {
  ClassHierarchyWalker wf;
  wf.add_participant(k1);
  wf.add_participant(k2);
  return wf.find_witness_subtype(ctxk, changes);
}

// Search ctxk for concrete implementations.  If there are klen or fewer,
// pack them into the given array and return the number.
// Otherwise, return -1, meaning the given array would overflow.
// (Note that a return of 0 means there are exactly no concrete subtypes.)
// In this search, if ctxk is concrete, it will be reported alone.
// For any type CC reported, no proper subtypes of CC will be reported.
int Dependencies::find_exclusive_concrete_subtypes(Klass* ctxk, int klen, Klass* karray[]) {
  ClassHierarchyWalker wf;
  wf.record_witnesses(klen);
  Klass* wit = wf.find_witness_subtype(ctxk);
  if (wit != NULL)  return -1;  // Too many witnesses.
  int num = wf.num_participants();
  // Pack the result array with the good news.
  for (int i = 0; i < num; i++)
    karray[i] = wf.participant(i);
  return num;
}

// If a class (or interface) has a unique concrete method uniqm, return NULL.
// Otherwise, return a class that contains an interfering method.
Klass* Dependencies::check_unique_concrete_method(Klass* ctxk, Method* uniqm, KlassDepChange* changes) {
  // Here is a missing optimization:  If uniqm->is_final(),
  // we don't really need to search beneath it for overrides.
  // This is probably not important, since we don't use dependencies
  // to track final methods.  (They can't be "definalized".)
  ClassHierarchyWalker wf(uniqm->method_holder(), uniqm);
  return wf.find_witness_definer(ctxk, changes);
}

// Find the set of all non-abstract methods under ctxk that match m.
// (The method m must be defined or inherited in ctxk.)
// Include m itself in the set, unless it is abstract.
// If this set has exactly one element, return that element.
Method* Dependencies::find_unique_concrete_method(Klass* ctxk, Method* m) {
  // Return NULL if m is marked old; must have been a redefined method.
  if (m->is_old()) {
    return NULL;
  }
  ClassHierarchyWalker wf(m);
  wf.record_witnesses(1);
  Klass* wit = wf.find_witness_definer(ctxk);
  if (wit != NULL)  return NULL;  // Too many witnesses.
  Method* fm = wf.found_method(0);  // Will be NULL if num_parts == 0.
  if (Dependencies::is_concrete_method(m, ctxk)) {
    if (fm == NULL) {
      // It turns out that m was always the only implementation.
      fm = m;
    } else if (fm != m) {
      // Two conflicting implementations after all.
      // (This can happen if m is inherited into ctxk and fm overrides it.)
      return NULL;
    }
  }
  return fm;
}

Klass* Dependencies::check_exclusive_concrete_methods(Klass* ctxk, Method* m1, Method* m2, KlassDepChange* changes) {
  ClassHierarchyWalker wf(m1);
  wf.add_participant(m1->method_holder());
  wf.add_participant(m2->method_holder());
  return wf.find_witness_definer(ctxk, changes);
}

Klass* Dependencies::check_has_no_finalizable_subclasses(Klass* ctxk, KlassDepChange* changes) {
  Klass* search_at = ctxk;
  if (changes != NULL)
    search_at = changes->new_type(); // just look at the new bit
  return find_finalizable_subclass(search_at);
}

Klass* Dependencies::check_call_site_target_value(oop call_site, oop method_handle, CallSiteDepChange* changes) {
  if (changes == NULL) {
    // Validate all CallSites
    if (!oopDesc::equals(java_lang_invoke_CallSite::target(call_site), method_handle))
      return call_site->klass();  // assertion failed
  } else {
    // Validate the given CallSite
    if (oopDesc::equals(call_site, changes->call_site()) && !oopDesc::equals(java_lang_invoke_CallSite::target(call_site), changes->method_handle())) {
      return call_site->klass();  // assertion failed
    }
  }
  return NULL;  // assertion still valid
}

void Dependencies::DepStream::trace_and_log_witness(Klass* witness) { }

Klass* Dependencies::DepStream::check_klass_dependency(KlassDepChange* changes) {
  Dependencies::check_valid_dependency_type(type());

  Klass* witness = NULL;
  switch (type()) {
  case evol_method:
    witness = check_evol_method(method_argument(0));
    break;
  case leaf_type:
    witness = check_leaf_type(context_type());
    break;
  case abstract_with_unique_concrete_subtype:
    witness = check_abstract_with_unique_concrete_subtype(context_type(), type_argument(1), changes);
    break;
  case abstract_with_no_concrete_subtype:
    witness = check_abstract_with_no_concrete_subtype(context_type(), changes);
    break;
  case concrete_with_no_concrete_subtype:
    witness = check_concrete_with_no_concrete_subtype(context_type(), changes);
    break;
  case unique_concrete_method:
    witness = check_unique_concrete_method(context_type(), method_argument(1), changes);
    break;
  case abstract_with_exclusive_concrete_subtypes_2:
    witness = check_abstract_with_exclusive_concrete_subtypes(context_type(), type_argument(1), type_argument(2), changes);
    break;
  case exclusive_concrete_methods_2:
    witness = check_exclusive_concrete_methods(context_type(), method_argument(1), method_argument(2), changes);
    break;
  case no_finalizable_subclasses:
    witness = check_has_no_finalizable_subclasses(context_type(), changes);
    break;
  default:
    witness = NULL;
    break;
  }
  trace_and_log_witness(witness);
  return witness;
}

Klass* Dependencies::DepStream::check_call_site_dependency(CallSiteDepChange* changes) {
  Dependencies::check_valid_dependency_type(type());

  Klass* witness = NULL;
  switch (type()) {
  case call_site_target_value:
    witness = check_call_site_target_value(argument_oop(0), argument_oop(1), changes);
    break;
  default:
    witness = NULL;
    break;
  }
  trace_and_log_witness(witness);
  return witness;
}

Klass* Dependencies::DepStream::spot_check_dependency_at(DepChange& changes) {
  // Handle klass dependency
  if (changes.is_klass_change() && changes.as_klass_change()->involves_context(context_type()))
    return check_klass_dependency(changes.as_klass_change());

  // Handle CallSite dependency
  if (changes.is_call_site_change())
    return check_call_site_dependency(changes.as_call_site_change());

  // irrelevant dependency; skip it
  return NULL;
}

void DepChange::print() {
  int nsup = 0, nint = 0;
  for (ContextStream str(*this); str.next(); ) {
    Klass* k = str.klass();
    switch (str.change_type()) {
    case Change_new_type:
      tty->print_cr("  dependee = %s", k->external_name());
      break;
    case Change_new_sub:
      ++nsup;
      break;
    case Change_new_impl:
      ++nint;
      break;

    default:
      break;
    }
  }
  if (nsup + nint != 0) {
    tty->print_cr("  context supers = %d, interfaces = %d", nsup, nint);
  }
}

void DepChange::ContextStream::start() {
  Klass* new_type = _changes.is_klass_change() ? _changes.as_klass_change()->new_type() : (Klass*) NULL;
  _change_type = (new_type == NULL ? NO_CHANGE : Start_Klass);
  _klass = new_type;
  _ti_base = NULL;
  _ti_index = 0;
  _ti_limit = 0;
}

bool DepChange::ContextStream::next() {
  switch (_change_type) {
  case Start_Klass:             // initial state; _klass is the new type
    _ti_base = InstanceKlass::cast(_klass)->transitive_interfaces();
    _ti_index = 0;
    _change_type = Change_new_type;
    return true;
  case Change_new_type:
    // fall through:
    _change_type = Change_new_sub;
  case Change_new_sub:
    // 6598190: brackets workaround Sun Studio C++ compiler bug 6629277
    {
      _klass = _klass->super();
      if (_klass != NULL) {
        return true;
      }
    }
    // else set up _ti_limit and fall through:
    _ti_limit = (_ti_base == NULL) ? 0 : _ti_base->length();
    _change_type = Change_new_impl;
  case Change_new_impl:
    if (_ti_index < _ti_limit) {
      _klass = _ti_base->at(_ti_index++);
      return true;
    }
    // fall through:
    _change_type = NO_CHANGE;  // iterator is exhausted
  case NO_CHANGE:
    break;
  default:
    ShouldNotReachHere();
  }
  return false;
}

void KlassDepChange::initialize() {
  // Mark all dependee and all its superclasses
  // Mark transitive interfaces
  for (ContextStream str(*this); str.next(); ) {
    Klass* d = str.klass();
    InstanceKlass::cast(d)->set_is_marked_dependent(true);
  }
}

KlassDepChange::~KlassDepChange() {
  // Unmark all dependee and all its superclasses
  // Unmark transitive interfaces
  for (ContextStream str(*this); str.next(); ) {
    Klass* d = str.klass();
    InstanceKlass::cast(d)->set_is_marked_dependent(false);
  }
}

bool KlassDepChange::involves_context(Klass* k) {
  if (k == NULL || !k->is_instance_klass()) {
    return false;
  }
  InstanceKlass* ik = InstanceKlass::cast(k);
  bool is_contained = ik->is_marked_dependent();
  return is_contained;
}

CallSiteDepChange::CallSiteDepChange(Handle call_site, Handle method_handle) :
  _call_site(call_site),
  _method_handle(method_handle) {
}

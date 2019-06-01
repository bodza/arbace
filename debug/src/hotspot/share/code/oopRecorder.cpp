#include "precompiled.hpp"

#include "ci/ciEnv.hpp"
#include "ci/ciInstance.hpp"
#include "ci/ciMetadata.hpp"
#include "code/oopRecorder.hpp"
#include "memory/allocation.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "utilities/copy.hpp"

template <class T> ValueRecorder<T>::ValueRecorder(Arena* arena) {
  _handles  = NULL;
  _indexes  = NULL;
  _arena    = arena;
  _complete = false;
}

template <class T> template <class X> ValueRecorder<T>::IndexCache<X>::IndexCache() {
  Copy::zero_to_bytes(&_cache[0], sizeof(_cache));
}

template <class T> int ValueRecorder<T>::size() {
  _complete = true;
  if (_handles == NULL)  return 0;
  return _handles->length() * sizeof(T);
}

template <class T> void ValueRecorder<T>::copy_values_to(nmethod* nm) {
  maybe_initialize();  // get non-null handles, even if we have no oops
  nm->copy_values(_handles);
}

template <class T> void ValueRecorder<T>::maybe_initialize() {
  if (_handles == NULL) {
    if (_arena != NULL) {
      _handles  = new(_arena) GrowableArray<T>(_arena, 10, 0, 0);
      _no_finds = new(_arena) GrowableArray<int>(    _arena, 10, 0, 0);
    } else {
      _handles  = new GrowableArray<T>(10, 0, 0);
      _no_finds = new GrowableArray<int>(    10, 0, 0);
    }
  }
}

template <class T> T ValueRecorder<T>::at(int index) {
  // there is always a NULL virtually present as first object
  if (index == null_index)  return NULL;
  return _handles->at(index - first_index);
}

template <class T> int ValueRecorder<T>::add_handle(T h, bool make_findable) {
  maybe_initialize();
  // indexing uses 1 as an origin--0 means null
  int index = _handles->length() + first_index;
  _handles->append(h);

  // Support correct operation of find_index().
  if (make_findable) {
    // This index may be returned from find_index().
    if (_indexes != NULL) {
      int* cloc = _indexes->cache_location(h);
      _indexes->set_cache_location_index(cloc, index);
    } else if (index == index_cache_threshold && _arena != NULL) {
      _indexes = new(_arena) IndexCache<T>();
      for (int i = 0; i < _handles->length(); i++) {
        // Load the cache with pre-existing elements.
        int index0 = i + first_index;
        if (_no_finds->contains(index0))  continue;
        int* cloc = _indexes->cache_location(_handles->at(i));
        _indexes->set_cache_location_index(cloc, index0);
      }
    }
  } else if (is_real(h)) {
    // Remember that this index is not to be returned from find_index().
    // This case is rare, because most or all uses of allocate_index pass
    // an argument of NULL or Universe::non_oop_word.
    // Thus, the expected length of _no_finds is zero.
    _no_finds->append(index);
  }

  return index;
}

template <class T> int ValueRecorder<T>::maybe_find_index(T h) {
  maybe_initialize();
  if (h == NULL)  return null_index;
  int* cloc = (_indexes == NULL)? NULL: _indexes->cache_location(h);
  if (cloc != NULL) {
    int cindex = _indexes->cache_location_index(cloc);
    if (cindex == 0) {
      return -1;   // We know this handle is completely new.
    }
    if (cindex >= first_index && _handles->at(cindex - first_index) == h) {
      return cindex;
    }
    if (!_indexes->cache_location_collision(cloc)) {
      return -1;   // We know the current cache occupant is unique to that cloc.
    }
  }

  // Not found in cache, due to a cache collision.  (Or, no cache at all.)
  // Do a linear search, most recent to oldest.
  for (int i = _handles->length() - 1; i >= 0; i--) {
    if (_handles->at(i) == h) {
      int findex = i + first_index;
      if (_no_finds->contains(findex))  continue;  // oops; skip this one
      if (cloc != NULL) {
        _indexes->set_cache_location_index(cloc, findex);
      }
      return findex;
    }
  }
  return -1;
}

// Explicitly instantiate these types
template class ValueRecorder<Metadata*>;
template class ValueRecorder<jobject>;

oop ObjectLookup::ObjectEntry::oop_value() const { return JNIHandles::resolve(_value); }

ObjectLookup::ObjectLookup(): _gc_count(Universe::heap()->total_collections()), _values(4) { }

void ObjectLookup::maybe_resort() {
  // The values are kept sorted by address which may be invalidated
  // after a GC, so resort if a GC has occurred since last time.
  if (_gc_count != Universe::heap()->total_collections()) {
    _gc_count = Universe::heap()->total_collections();
    _values.sort(sort_by_address);
  }
}

int ObjectLookup::sort_by_address(oop a, oop b) {
  // oopDesc::compare returns the opposite of what this function returned
  return -(oopDesc::compare(a, b));
}

int ObjectLookup::sort_by_address(ObjectEntry* a, ObjectEntry* b) {
  return sort_by_address(a->oop_value(), b->oop_value());
}

int ObjectLookup::sort_oop_by_address(oop const& a, ObjectEntry const& b) {
  return sort_by_address(a, b.oop_value());
}

int ObjectLookup::find_index(jobject handle, OopRecorder* oop_recorder) {
  if (handle == NULL) {
    return 0;
  }
  oop object = JNIHandles::resolve(handle);
  maybe_resort();
  bool found;
  int location = _values.find_sorted<oop, sort_oop_by_address>(object, found);
  if (!found) {
    jobject handle = JNIHandles::make_local(object);
    ObjectEntry r(handle, oop_recorder->allocate_oop_index(handle));
    _values.insert_before(location, r);
    return r.index();
  }
  return _values.at(location).index();
}

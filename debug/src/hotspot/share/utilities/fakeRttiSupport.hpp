#ifndef SHARE_VM_UTILITIES_FAKERTTISUPPORT_HPP
#define SHARE_VM_UTILITIES_FAKERTTISUPPORT_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/debug.hpp"

// Provides support for checked downcasts in a hierarchy of classes.
// The base class provides a member of this type, specialized on that
// base class and an associated tag type.  Tags are small non-negative
// integer values uniquely associated with distinct classes in the
// hierarchy.  A tag type is often an enum type.
//
// The concrete class specifies the concrete tag.
//
// The tag set specifies the set of classes in the derivation
// sequence.  Classes in the derivation sequence add their associated
// tag during construction.  Given the tag associated with a class, an
// object is an instance of that class if the tag is included in the
// object's set of recorded tags.
//
// A tag T is present in a tag set if the T'th bit of the tag set is
// one.
//
// Note: The representation of a tag set being uintx sets an upper
// bound on the size of a class hierarchy this utility can be used
// with.
template<typename T, typename TagType>
class FakeRttiSupport {
  friend class VMStructs;
public:
  // Construct with the indicated concrete tag, and include the
  // concrete tag in the associated tag set.
  explicit FakeRttiSupport(TagType concrete_tag) :
    _tag_set(tag_bit(concrete_tag)), _concrete_tag(concrete_tag) { }

  // Construct with the indicated concrete tag and tag set.
  // Note: This constructor is public only to allow clients to set up
  // "unusual" (or perhaps buggy) fake RTTI configurations.
  FakeRttiSupport(TagType concrete_tag, uintx tag_set) :
    _tag_set(tag_set), _concrete_tag(validate_tag(concrete_tag)) { }

  // Get the concrete tag.
  TagType concrete_tag() const { return _concrete_tag; }

  // Test whether tag is in the tag set.
  bool has_tag(TagType tag) const {
    return (_tag_set & tag_bit(tag)) != 0;
  }

  // Return a new support object which is the same as this, except tag
  // has been added to the tag set.  The tag must not already be
  // present in the tag set.
  FakeRttiSupport add_tag(TagType tag) const {
    uintx tbit = tag_bit(tag);
    return FakeRttiSupport(_concrete_tag, _tag_set | tbit);
  }

private:
  uintx _tag_set;
  TagType _concrete_tag;

  static uintx tag_bit(TagType tag) {
    return ((uintx)1) << validate_tag(tag);
  }

  static TagType validate_tag(TagType tag) {
    return tag;
  }
};

#endif

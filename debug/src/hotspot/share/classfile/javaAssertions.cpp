#include "precompiled.hpp"

#include "classfile/javaAssertions.hpp"
#include "classfile/javaClasses.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/oopFactory.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "runtime/handles.inline.hpp"

bool                            JavaAssertions::_userDefault = false;
bool                            JavaAssertions::_sysDefault = false;
JavaAssertions::OptionList*     JavaAssertions::_classes = 0;
JavaAssertions::OptionList*     JavaAssertions::_packages = 0;

JavaAssertions::OptionList::OptionList(const char* name, bool enabled, OptionList* next) {
  _name = name;
  _enabled = enabled;
  _next = next;
}

int JavaAssertions::OptionList::count(OptionList* p) {
  int rc;
  for (rc = 0; p != 0; p = p->next(), ++rc) /* empty */;
  return rc;
}

void JavaAssertions::addOption(const char* name, bool enable) {
  // Copy the name.  The storage needs to exist for the the lifetime of the vm;
  // it is never freed, so will be leaked (along with other option strings -
  // e.g., bootclasspath) if a process creates/destroys multiple VMs.
  int len = (int)strlen(name);
  char *name_copy = NEW_C_HEAP_ARRAY(char, len + 1, mtClass);
  strcpy(name_copy, name);

  // Figure out which list the new item should go on.  Names that end in "..."
  // go on the package tree list.
  OptionList** head = &_classes;
  if (len >= 3 && strcmp(name_copy + len - 3, "...") == 0) {
    // Delete the "...".
    len -= 3;
    name_copy[len] = '\0';
    head = &_packages;
  }

  // Convert class/package names to internal format.  Will have to convert back
  // when copying to java in createJavaAssertionStatusDirectives, but that
  // should happen only once.  Alternative would require that
  // JVM_DesiredAssertionStatus pass the external_name() to
  // JavaAssertion::enabled(), but that is done once per loaded class.
  for (int i = 0; i < len; ++i) {
    if (name_copy[i] == '.') name_copy[i] = '/';
  }

  // Prepend a new item to the list.  Items added later take precedence, so
  // prepending allows us to stop searching the list after the first match.
  *head = new OptionList(name_copy, enable, *head);
}

oop JavaAssertions::createAssertionStatusDirectives(TRAPS) {
  Symbol* asd_sym = vmSymbols::java_lang_AssertionStatusDirectives();
  Klass* k = SystemDictionary::resolve_or_fail(asd_sym, true, CHECK_NULL);
  InstanceKlass* asd_klass = InstanceKlass::cast(k);
  asd_klass->initialize(CHECK_NULL);
  Handle h = asd_klass->allocate_instance_handle(CHECK_NULL);

  int len;
  typeArrayOop t;
  len = OptionList::count(_packages);
  objArrayOop pn = oopFactory::new_objArray(SystemDictionary::String_klass(), len, CHECK_NULL);
  objArrayHandle pkgNames (THREAD, pn);
  t = oopFactory::new_typeArray(T_BOOLEAN, len, CHECK_NULL);
  typeArrayHandle pkgEnabled(THREAD, t);
  fillJavaArrays(_packages, len, pkgNames, pkgEnabled, CHECK_NULL);

  len = OptionList::count(_classes);
  objArrayOop cn = oopFactory::new_objArray(SystemDictionary::String_klass(), len, CHECK_NULL);
  objArrayHandle classNames (THREAD, cn);
  t = oopFactory::new_typeArray(T_BOOLEAN, len, CHECK_NULL);
  typeArrayHandle classEnabled(THREAD, t);
  fillJavaArrays(_classes, len, classNames, classEnabled, CHECK_NULL);

  java_lang_AssertionStatusDirectives::set_packages(h(), pkgNames());
  java_lang_AssertionStatusDirectives::set_packageEnabled(h(), pkgEnabled());
  java_lang_AssertionStatusDirectives::set_classes(h(), classNames());
  java_lang_AssertionStatusDirectives::set_classEnabled(h(), classEnabled());
  java_lang_AssertionStatusDirectives::set_deflt(h(), userClassDefault());
  return h();
}

void JavaAssertions::fillJavaArrays(const OptionList* p, int len, objArrayHandle names, typeArrayHandle enabled, TRAPS) {
  // Fill in the parallel names and enabled (boolean) arrays.  Start at the end
  // of the array and work backwards, so the order of items in the arrays
  // matches the order on the command line (the list is in reverse order, since
  // it was created by prepending successive items from the command line).
  int index;
  for (index = len - 1; p != 0; p = p->next(), --index) {
    Handle s = java_lang_String::create_from_str(p->name(), CHECK);
    s = java_lang_String::char_converter(s, '/', '.', CHECK);
    names->obj_at_put(index, s());
    enabled->bool_at_put(index, p->enabled());
  }
}

inline JavaAssertions::OptionList*
JavaAssertions::match_class(const char* classname) {
  for (OptionList* p = _classes; p != 0; p = p->next()) {
    if (strcmp(p->name(), classname) == 0) {
      return p;
    }
  }
  return 0;
}

JavaAssertions::OptionList*
JavaAssertions::match_package(const char* classname) {
  // Search the package list for any items that apply to classname.  Each
  // sub-package in classname is checked, from most-specific to least, until one
  // is found.
  if (_packages == 0) return 0;

  // Find the length of the "most-specific" package in classname.  If classname
  // does not include a package, length will be 0 which will match items for the
  // default package (from options "-ea:..."  or "-da:...").
  size_t len = strlen(classname);
  for (/* empty */; len > 0 && classname[len] != '/'; --len) /* empty */;

  do {
    for (OptionList* p = _packages; p != 0; p = p->next()) {
      if (strncmp(p->name(), classname, len) == 0 && p->name()[len] == '\0') {
        return p;
      }
    }

    // Find the length of the next package, taking care to avoid decrementing
    // past 0 (len is unsigned).
    while (len > 0 && classname[--len] != '/') /* empty */;
  } while (len > 0);

  return 0;
}

inline void JavaAssertions::trace(const char* name, const char* typefound, const char* namefound, bool enabled) { }

bool JavaAssertions::enabled(const char* classname, bool systemClass) {
  // This will be slow if the number of assertion options on the command line is
  // large--it traverses two lists, one of them multiple times.  Could use a
  // single n-ary tree instead of lists if someone ever notices.

  // First check options that apply to classes.  If we find a match we're done.
  OptionList* p;
  if ((p = match_class(classname))) {
    trace(classname, "class", p->name(), p->enabled());
    return p->enabled();
  }

  // Now check packages, from most specific to least.
  if ((p = match_package(classname))) {
    trace(classname, "package", p->name(), p->enabled());
    return p->enabled();
  }

  // No match.  Return the default status.
  bool result = systemClass ? systemClassDefault() : userClassDefault();
  trace(classname, systemClass ? "system" : "user", "default", result);
  return result;
}

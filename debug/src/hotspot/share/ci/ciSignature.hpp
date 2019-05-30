#ifndef SHARE_VM_CI_CISIGNATURE_HPP
#define SHARE_VM_CI_CISIGNATURE_HPP

#include "ci/ciClassList.hpp"
#include "ci/ciSymbol.hpp"
#include "interpreter/bytecodes.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/growableArray.hpp"

// ciSignature
//
// This class represents the signature of a method.
class ciSignature : public ResourceObj {
private:
  ciSymbol* _symbol;
  ciKlass*  _accessing_klass;

  GrowableArray<ciType*>* _types;
  int _size;   // number of stack slots required for arguments
  int _count;  // number of parameter types in the signature

  friend class ciMethod;
  friend class ciBytecodeStream;
  friend class ciObjectFactory;

  ciSignature(ciKlass* accessing_klass, const constantPoolHandle& cpool, ciSymbol* signature);
  ciSignature(ciKlass* accessing_klass,                           ciSymbol* signature, ciMethodType* method_type);

  void get_all_klasses();

  Symbol* get_symbol() const                     { return _symbol->get_symbol(); }

public:
  ciSymbol* as_symbol() const                    { return _symbol; }
  ciKlass*  accessing_klass() const              { return _accessing_klass; }

  ciType*   return_type() const;
  ciType*   type_at(int index) const;

  int       size() const                         { return _size; }
  int       count() const                        { return _count; }

  int       arg_size_for_bc(Bytecodes::Code bc)  { return size() + (Bytecodes::has_receiver(bc) ? 1 : 0); }

  bool equals(ciSignature* that);

  void print_signature();
  void print();
};

#endif

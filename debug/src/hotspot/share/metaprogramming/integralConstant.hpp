#ifndef SHARE_VM_METAPROGRAMMING_INTEGRALCONSTANT_HPP
#define SHARE_VM_METAPROGRAMMING_INTEGRALCONSTANT_HPP

// An Integral Constant is a class providing a compile-time value of an
// integral type.  An Integral Constant is also a nullary metafunction,
// returning itself.  An integral constant object is implicitly
// convertible to the associated value.
//
// A type n is a model of Integral Constant if it meets the following
// requirements:
//
// n::ValueType                : The integral type of n::value
// n::value                    : An integral constant expression
// n::type                     : IsSame<n::type, n>::value is true
// n::value_type const c = n() : c == n::value

// A model of the Integer Constant concept.
// T is an integral type, and is the value_type.
// v is an integral constant, and is the value.
template<typename T, T v>
struct IntegralConstant {
  typedef T value_type;
  static const value_type value = v;
  typedef IntegralConstant<T, v> type;
  operator value_type() { return value; }
};

// A bool valued IntegralConstant whose value is true.
typedef IntegralConstant<bool, true> TrueType;

// A bool valued IntegralConstant whose value is false.
typedef IntegralConstant<bool, false> FalseType;

#endif

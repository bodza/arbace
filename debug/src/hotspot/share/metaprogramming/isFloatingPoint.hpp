#ifndef SHARE_VM_METAPROGRAMMING_ISFLOATINGPOINT_HPP
#define SHARE_VM_METAPROGRAMMING_ISFLOATINGPOINT_HPP

#include "metaprogramming/integralConstant.hpp"

// This metafunction returns true iff the type T (irrespective of CV qualifiers)
// is a floating point type.

template <typename T> struct IsFloatingPoint: public FalseType {};

template <> struct IsFloatingPoint<float>: public TrueType {};
template <> struct IsFloatingPoint<const float>: public TrueType {};
template <> struct IsFloatingPoint<volatile float>: public TrueType {};
template <> struct IsFloatingPoint<const volatile float>: public TrueType {};

template <> struct IsFloatingPoint<double>: public TrueType {};
template <> struct IsFloatingPoint<const double>: public TrueType {};
template <> struct IsFloatingPoint<volatile double>: public TrueType {};
template <> struct IsFloatingPoint<const volatile double>: public TrueType {};

template <> struct IsFloatingPoint<long double>: public TrueType {};
template <> struct IsFloatingPoint<const long double>: public TrueType {};
template <> struct IsFloatingPoint<volatile long double>: public TrueType {};
template <> struct IsFloatingPoint<const volatile long double>: public TrueType {};

#endif

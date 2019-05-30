#ifndef SHARE_VM_UTILITIES_HISTOGRAM_HPP
#define SHARE_VM_UTILITIES_HISTOGRAM_HPP

#include "memory/allocation.hpp"
#include "runtime/os.hpp"
#include "utilities/growableArray.hpp"

// This class provides a framework for collecting various statistics.
// The current implementation is oriented towards counting invocations
// of various types, but that can be easily changed.
//
// To use it, you need to declare a Histogram*, and a subtype of
// HistogramElement:
//
//  HistogramElement* MyHistogram;
//
//  class MyHistogramElement : public HistogramElement {
//    public:
//      MyHistogramElement(char* name);
//  };
//
//  MyHistogramElement::MyHistogramElement(char* elementName) {
//    _name = elementName;
//
//    if(MyHistogram == NULL)
//      MyHistogram = new Histogram("My Call Counts",100);
//
//    MyHistogram->add_element(this);
//  }
//
//  #define MyCountWrapper(arg) static MyHistogramElement* e = new MyHistogramElement(arg); e->increment_count()
//
// This gives you a simple way to count invocations of specfic functions:
//
// void a_function_that_is_being_counted() {
//   MyCountWrapper("FunctionName");
//   ...
// }
//
// To print the results, invoke print() on your Histogram*.

#endif

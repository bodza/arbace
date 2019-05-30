#ifndef SHARE_VM_UTILITIES_TICKS_HPP
#define SHARE_VM_UTILITIES_TICKS_HPP

#include "jni.h"
#include "memory/allocation.hpp"
#include "utilities/macros.hpp"

// Time sources
class ElapsedCounterSource {
 public:
  typedef jlong Type;
  static uint64_t frequency();
  static Type now();
  static double seconds(Type value);
  static uint64_t milliseconds(Type value);
  static uint64_t microseconds(Type value);
  static uint64_t nanoseconds(Type value);
};

// Not guaranteed to be synchronized across hardware threads and
// therefore software threads, and can be updated asynchronously
// by software. now() can jump backwards as well as jump forward
// when threads query different cores/sockets.
// Very much not recommended for general use. Caveat emptor.
class FastUnorderedElapsedCounterSource {
 public:
  typedef jlong Type;
  static uint64_t frequency();
  static Type now();
  static double seconds(Type value);
  static uint64_t milliseconds(Type value);
  static uint64_t microseconds(Type value);
  static uint64_t nanoseconds(Type value);
};

template <typename T1, typename T2>
class PairRep {
 public:
  T1 val1;
  T2 val2;

  PairRep() : val1((T1)0), val2((T2)0) {}
  void operator+=(const PairRep& rhs) {
    val1 += rhs.val1;
    val2 += rhs.val2;
  }
  void operator-=(const PairRep& rhs) {
    val1 -= rhs.val1;
    val2 -= rhs.val2;
  }
  bool operator==(const PairRep& rhs) const {
    return val1 == rhs.val1;
  }
  bool operator!=(const PairRep& rhs) const {
    return !operator==(rhs);
  }
  bool operator<(const PairRep& rhs) const {
    return val1 < rhs.val1;
  }
  bool operator>(const PairRep& rhs) const {
    return val1 > rhs.val1;
  }
};

template <typename T1, typename T2>
PairRep<T1, T2> operator-(const PairRep<T1, T2>& lhs, const PairRep<T1, T2>& rhs) {
  PairRep<T1, T2> temp(lhs);
  temp -= rhs;
  return temp;
}

typedef PairRep<ElapsedCounterSource::Type, FastUnorderedElapsedCounterSource::Type> CompositeTime;

class CompositeElapsedCounterSource {
 public:
  typedef CompositeTime Type;
  static uint64_t frequency();
  static Type now();
  static double seconds(Type value);
  static uint64_t milliseconds(Type value);
  static uint64_t microseconds(Type value);
  static uint64_t nanoseconds(Type value);
};

template <typename TimeSource>
class Representation {
 public:
  typedef typename TimeSource::Type Type;
 protected:
  Type _rep;
  Representation(const Representation<TimeSource>& end, const Representation<TimeSource>& start) : _rep(end._rep - start._rep) {}
  Representation() : _rep() {}
 public:
  void operator+=(const Representation<TimeSource>& rhs) {
    _rep += rhs._rep;
  }
  void operator-=(const Representation<TimeSource>& rhs) {
    _rep -= rhs._rep;
  }
  bool operator==(const Representation<TimeSource>& rhs) const {
    return _rep == rhs._rep;
  }
  bool operator!=(const Representation<TimeSource>& rhs) const {
    return !operator==(rhs);
  }
  bool operator<(const Representation<TimeSource>& rhs) const {
    return _rep < rhs._rep;
  }
  bool operator>(const Representation<TimeSource>& rhs) const {
    return _rep > rhs._rep;
  }
  bool operator<=(const Representation<TimeSource>& rhs) const {
    return !operator>(rhs);
  }
  bool operator>=(const Representation<TimeSource>& rhs) const {
    return !operator<(rhs);
  }
  double seconds() const {
    return TimeSource::seconds(_rep);
  }
  uint64_t milliseconds() const {
    return TimeSource::milliseconds(_rep);
  }
  uint64_t microseconds() const {
    return TimeSource::microseconds(_rep);
  }
  uint64_t nanoseconds() const {
    return TimeSource::nanoseconds(_rep);
  }
};

template <typename TimeSource>
class CounterRepresentation : public Representation<TimeSource> {
 protected:
  CounterRepresentation(const CounterRepresentation& end, const CounterRepresentation& start) : Representation<TimeSource>(end, start) {}
  explicit CounterRepresentation(jlong value) : Representation<TimeSource>() {
    this->_rep = value;
  }
 public:
  CounterRepresentation() : Representation<TimeSource>() {}
  typename TimeSource::Type value() const { return this->_rep; }
  operator typename TimeSource::Type() { return value(); }
};

template <typename TimeSource>
class CompositeCounterRepresentation : public Representation<TimeSource> {
 protected:
  CompositeCounterRepresentation(const CompositeCounterRepresentation& end, const CompositeCounterRepresentation& start) :
    Representation<TimeSource>(end, start) {}
  explicit CompositeCounterRepresentation(jlong value) : Representation<TimeSource>() {
    this->_rep.val1 = value;
    this->_rep.val2 = value;
  }
 public:
  CompositeCounterRepresentation() : Representation<TimeSource>() {}
  ElapsedCounterSource::Type value() const { return this->_rep.val1; }
  FastUnorderedElapsedCounterSource::Type ft_value() const { return this->_rep.val2; }
};

template <template <typename> class, typename>
class TimeInstant;

template <template <typename> class Rep, typename TimeSource>
class TimeInterval : public Rep<TimeSource> {
  template <template <typename> class, typename>
  friend class TimeInstant;
  TimeInterval(const TimeInstant<Rep, TimeSource>& end, const TimeInstant<Rep, TimeSource>& start) : Rep<TimeSource>(end, start) {}
 public:
  TimeInterval() : Rep<TimeSource>() {}
  TimeInterval<Rep, TimeSource> operator+(const TimeInterval<Rep, TimeSource>& rhs) const {
    TimeInterval<Rep, TimeSource> temp(*this);
    temp += rhs;
    return temp;
  }
  TimeInterval<Rep, TimeSource> operator-(const TimeInterval<Rep, TimeSource>& rhs) const {
    TimeInterval<Rep, TimeSource> temp(*this);
    temp -= rhs;
    return temp;
  }
};

template <template <typename> class Rep, typename TimeSource>
class TimeInstant : public Rep<TimeSource> {
 public:
  TimeInstant() : Rep<TimeSource>() {}
  TimeInstant<Rep, TimeSource>& operator+=(const TimeInterval<Rep, TimeSource>& rhs) {
    Rep<TimeSource>::operator+=(rhs);
    return *this;
  }
  TimeInstant<Rep, TimeSource>& operator-=(const TimeInterval<Rep, TimeSource>& rhs) {
    Rep<TimeSource>::operator-=(rhs);
    return *this;
  }
  TimeInterval<Rep, TimeSource> operator+(const TimeInstant<Rep, TimeSource>& end) const {
    return TimeInterval<Rep, TimeSource>(end, *this);
  }
  TimeInterval<Rep, TimeSource> operator-(const TimeInstant<Rep, TimeSource>& start) const {
    return TimeInterval<Rep, TimeSource>(*this, start);
  }
  void stamp() {
    this->_rep = TimeSource::now();
  }
  static TimeInstant<Rep, TimeSource> now() {
    TimeInstant<Rep, TimeSource> temp;
    temp.stamp();
    return temp;
  }
 private:
  TimeInstant(jlong ticks) : Rep<TimeSource>(ticks) {}
  friend class GranularTimer;
  friend class ObjectSample;
  //  GC VM tests
  friend class TimePartitionPhasesIteratorTest;
  friend class GCTimerTest;
};

typedef TimeInstant<CounterRepresentation, ElapsedCounterSource> Ticks;
typedef TimeInterval<CounterRepresentation, ElapsedCounterSource> Tickspan;

#endif

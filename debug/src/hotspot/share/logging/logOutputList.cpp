#include "precompiled.hpp"
#include "logging/logLevel.hpp"
#include "logging/logOutputList.hpp"
#include "memory/allocation.inline.hpp"
#include "runtime/atomic.hpp"
#include "runtime/orderAccess.hpp"
#include "utilities/globalDefinitions.hpp"

jint LogOutputList::increase_readers() {
  jint result = Atomic::add(1, &_active_readers);
  return result;
}

jint LogOutputList::decrease_readers() {
  jint result = Atomic::add(-1, &_active_readers);
  return result;
}

void LogOutputList::wait_until_no_readers() const {
  OrderAccess::storeload();
  while (_active_readers != 0) {
    // Busy wait
  }
}

void LogOutputList::set_output_level(LogOutput* output, LogLevelType level) {
  LogOutputNode* node = find(output);
  if (level == LogLevel::Off && node != NULL) {
    remove_output(node);
  } else if (level != LogLevel::Off && node == NULL) {
    add_output(output, level);
  } else if (node != NULL) {
    update_output_level(node, level);
  }
}

LogOutputList::LogOutputNode* LogOutputList::find(const LogOutput* output) const {
  for (LogOutputNode* node = _level_start[LogLevel::Last]; node != NULL; node = node->_next) {
    if (output == node->_value) {
      return node;
    }
  }
  return NULL;
}

void LogOutputList::remove_output(LogOutputList::LogOutputNode* node) {

  // Remove node from _level_start first
  bool found = false;
  for (uint level = LogLevel::First; level < LogLevel::Count; level++) {
    if (_level_start[level] == node) {
      found = true;
      _level_start[level] = node->_next;
    }
  }

  // Now remove it from the linked list
  for (LogOutputNode* cur = _level_start[LogLevel::Last]; cur != NULL; cur = cur->_next) {
    if (cur->_next == node) {
      found = true;
      cur->_next = node->_next;
      break;
    }
  }

  wait_until_no_readers();
  delete node;
}

void LogOutputList::add_output(LogOutput* output, LogLevelType level) {
  LogOutputNode* node = new LogOutputNode();
  node->_value = output;
  node->_level = level;

  // Set the next pointer to the first node of a lower level
  for (node->_next = _level_start[level];
       node->_next != NULL && node->_next->_level == level;
       node->_next = node->_next->_next) {
  }

  // Update the _level_start index
  for (int l = LogLevel::Last; l >= level; l--) {
    if (_level_start[l] == NULL || _level_start[l]->_level < level) {
      _level_start[l] = node;
    }
  }

  // Add the node the list
  for (LogOutputNode* cur = _level_start[LogLevel::Last]; cur != NULL; cur = cur->_next) {
    if (cur != node && cur->_next == node->_next) {
      cur->_next = node;
      break;
    }
  }
}

void LogOutputList::update_output_level(LogOutputList::LogOutputNode* node, LogLevelType level) {
  add_output(node->_value, level);
  wait_until_no_readers();
  remove_output(node);
}

#include "precompiled.hpp"

// This file should really contain the code for generating the OptoRuntime
// exception_blob. However that code uses SimpleRuntimeFrame which only
// exists in sharedRuntime_x86_64.cpp. When there is a sharedRuntime_<arch>.hpp
// file and SimpleRuntimeFrame is able to move there then the exception_blob
// code will move here where it belongs.

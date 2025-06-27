
#ifndef ASPECT_GUI_ASP_IMPORT_STL_HPP
#define ASPECT_GUI_ASP_IMPORT_STL_HPP

#if defined(__has_include)
#define ASP_HAS_INCLUDE(X) __has_include(X)
#else
#define ASP_HAS_INCLUDE(X) false
#endif

#if !defined(ASP_NO_STL_MODULE) && __cpp_lib_modules
import std;
#else
#include <algorithm>
#include <array>
#include <cassert>
#include <chrono>
#include <cmath>
#include <concepts>
#include <cstddef>
#include <cstring>
#include <exception>
#include <format>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <ratio>
#include <string>
#include <string_view>
#include <thread>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#ifndef NDEBUG
#include <iostream>
#include <source_location>
#endif

#if __cplusplus > 202002L && ASP_HAS_INCLUDE(<expected>)
#include <expected>
#define ASP_HAS_EXPECTED __cpp_lib_expected >= 202202L
#else
#define ASP_HAS_EXPECTED false
#endif
#endif

#endif

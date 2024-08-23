#ifndef COMPONENT_GUI_STL_EXTEND_HPP
#define COMPONENT_GUI_STL_EXTEND_HPP

#if defined(__has_include)
#define CGUI_HAS_INCLUDE(X) __has_include(X)
#else
#define CGUI_HAS_INCLUDE(X) false
#endif

#include <cassert>
#include <exception>
#include <initializer_list>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

#if CGUI_HAS_INCLUDE(<expected>)
#include <expected>
#endif

#if __cpp_lib_expected < 202202L
#include <cgui/std-backport/expected.hpp>
#endif

#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/limits.hpp>
#include <cgui/std-backport/type_traits.hpp>
#include <cgui/std-backport/utility.hpp>

#if __cpp_lib_constexpr_string >= 201907L
#define CGUI_CONSTEXPR_STRING_F constexpr
#else
#define CGUI_CONSTEXPR_STRING_F
#endif

#include <cgui/std-backport/limits.hpp>
#include <cgui/warnings.hpp>

namespace cgui {
namespace details {
template <typename T, typename TIn>
constexpr auto &&forward_like_const(TIn &&in) {
  if constexpr (std::is_const_v<std::remove_reference_t<T>>) {
    return std::as_const(in);
  } else {
    return in;
  }
}
template <typename T, typename TIn>
constexpr auto &&forward_like_reference(TIn &&in) {
  if constexpr (std::is_rvalue_reference_v<T>) {
    return std::move(in);
  } else {
    return in;
  }
}
} // namespace details

template <typename T, typename TIn> constexpr auto &&forward_like(TIn &&in) {
  return details::forward_like_reference<T>(details::forward_like_const<T>(in));
}

template <typename T, typename... Ts>
concept invocable_for_all = (std::invocable<T, Ts> && ...);

template <typename T, typename... Ts>
using invoke_result_for_all_t =
    std::common_type_t<std::invoke_result_t<T, Ts>...>;

constexpr void tuple_for_each(auto &&cb, auto &&t) {
  std::apply([&cb](auto &&...args) { unused(((cb(args), 0) | ...)); },
             std::forward<decltype(t)>(t));
}

#if __cpp_lib_expected >= 202202L
using std::bad_expected_access;
using std::expected;
using std::unexpect;
using std::unexpect_t;
using std::unexpected;
#else
using bp::bad_expected_access;
using bp::expected;
using bp::unexpect;
using bp::unexpect_t;
using bp::unexpected;
#endif

#if __cpp_lib_unreachable >= 202202L
using std::unreachable;
#else
using bp::unreachable;
#endif

using bp::highest_possible;
using bp::highest_possible_t;
using bp::lowest_possible;
using bp::lowest_possible_t;

} // namespace cgui

#endif // COMPONENT_GUI_STL_EXTEND_HPP

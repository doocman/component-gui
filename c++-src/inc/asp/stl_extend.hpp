#ifndef COMPONENT_GUI_STL_EXTEND_HPP
#define COMPONENT_GUI_STL_EXTEND_HPP

#if defined(__has_include)
#define ASP_HAS_INCLUDE(X) __has_include(X)
#else
#define ASP_HAS_INCLUDE(X) false
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

#if __cplusplus > 202002L && ASP_HAS_INCLUDE(<expected>)
#include <expected>
#define ASP_HAS_EXPECTED __cpp_lib_expected >= 202202L
#else
#define ASP_HAS_EXPECTED false
#endif

#if !ASP_HAS_EXPECTED
#include <asp/std-backport/expected.hpp>
#endif

#include <asp/std-backport/concepts.hpp>
#include <asp/std-backport/limits.hpp>
#include <asp/std-backport/type_traits.hpp>
#include <asp/std-backport/utility.hpp>

#if __cpp_lib_constexpr_string >= 201907L
#define ASP_CONSTEXPR_STRING_F constexpr
#else
#define ASP_CONSTEXPR_STRING_F
#endif

#include <asp/std-backport/limits.hpp>
#include <asp/warnings.hpp>

namespace asp {
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

#if ASP_HAS_EXPECTED
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

} // namespace asp

#endif // COMPONENT_GUI_STL_EXTEND_HPP

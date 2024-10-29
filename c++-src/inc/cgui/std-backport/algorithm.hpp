
#ifndef COMPONENT_GUI_STD_BACKPORT_ALGORITHM_HPP
#define COMPONENT_GUI_STD_BACKPORT_ALGORITHM_HPP

#include <algorithm>
#include <array>
#include <numeric>
#include <type_traits>
#include <utility>

namespace cgui::bp {

/// Checks if all values Ts... are sequential.
///
/// \tparam Ts Types of parameters
/// \tparam T Common type of Ts.
/// \param vs Values to check
/// \return true if all values are sequential (i.e. v[n+1] - v[n] = 1 for all n)
template <typename... Ts,
          typename T = std::remove_cvref_t<std::common_type_t<Ts...>>>
constexpr bool is_sequential(Ts... vs) {
  if constexpr (sizeof...(vs) < 2) {
    return true;
  }
  std::array<T, sizeof...(Ts)> vals{vs...};
  auto b = begin(vals);
  auto b2 = b + 1;
  auto e = end(vals) - 1;
  auto res = std::transform_reduce(
      b, e, b2, std::make_signed_t<std::size_t>{}, std::plus{},
      [](auto const &low, auto const &high) {
        constexpr auto to_int = [](T const &t) {
          if constexpr (requires() {
                          typename std::underlying_type<T>::type;
                        }) {
            return static_cast<std::underlying_type_t<T>>(t);
          } else {
            return t;
          }
        };
        auto diff = to_int(high) - to_int(low);
        return diff == 1 ? 0 : 1;
      });
  return res == 0;
}

/// Returns the first value of a parameter pack
/// \param v first value of pack
/// \return v
constexpr decltype(auto) first_of(auto &&v, auto &&...) {
  return std::forward<decltype(v)>(v);
}

/// Finds the index i in which v == values[i] or sizeof...(values) in case it
/// does not exist.
/// \tparam T Type of template values
/// \tparam values pack to find index of
/// \param v value to find
/// \return index or sizeof...(values)
template <typename T, T... values>
constexpr auto find_template_index(T const &v) {
  constexpr std::array<T, sizeof...(values)> vals{values...};
  auto it = std::ranges::find(vals, v);
  return std::distance(begin(vals), it);
}

/// Return true if all parameter Ts are unique, i.e. their operator== is false
/// for all other parameter
/// \tparam T type of first value
/// \tparam Ts rest of the parameter pack.
/// \param value first value to compare.
/// \param values values to compare.
/// \return true if no element equals another
template <typename T, std::equality_comparable_with<T>... Ts>
constexpr bool is_unique(T &&v1, Ts &&...values) {
  if constexpr (sizeof...(Ts) == 0) {
    return true;
  } else {
    return ((v1 != values) && ...) && is_unique(std::forward<Ts>(values)...);
  }
}

} // namespace cgui::bp

#endif

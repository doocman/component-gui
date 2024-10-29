
#ifndef COMPONENT_GUI_STD_BACKPORT_ALGORITHM_HPP
#define COMPONENT_GUI_STD_BACKPORT_ALGORITHM_HPP

#include <algorithm>
#include <array>
#include <numeric>
#include <type_traits>
#include <utility>

namespace cgui::bp {
template <typename... Ts,
          typename T = std::remove_cvref_t<std::common_type_t<Ts...>>>
constexpr bool is_sequential(Ts... v) {
  if constexpr (sizeof...(v) < 2) {
    return true;
  }
  std::array<T, sizeof...(Ts)> vals{v...};
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

constexpr decltype(auto) first_of(auto &&v, auto &&...) {
  return std::forward<decltype(v)>(v);
}

template <typename T, T... values>
constexpr auto find_template_index(T const &v) {
  constexpr std::array<T, sizeof...(values)> vals{values...};
  auto it = std::ranges::find(vals, v);
  return std::distance(begin(vals), it);
}

template <typename... Ts, typename T = std::remove_cvref_t<std::common_type_t<Ts...>>>
constexpr bool is_unique(Ts&&... values) {
  std::array<T, sizeof...(Ts)> vals{values...};
  return ((std::count(begin(vals), end(vals), values) == 1) && ...);
}

} // namespace cgui::bp

#endif

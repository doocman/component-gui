//
// Created by rvons on 2024-08-20.
//

#ifndef COMPONENT_GUI_TYPE_TRAITS_HPP
#define COMPONENT_GUI_TYPE_TRAITS_HPP

#include <functional>
#include <numeric>
#include <type_traits>

namespace cgui::bp {
template <typename T> using dereferenced_t = decltype(*std::declval<T &&>());

template <typename T, T... tVals>
constexpr bool is_increasing(std::integer_sequence<T, tVals...>) {
  if constexpr (sizeof...(tVals) < 2) {
    return true;
  } else {
    auto constexpr val_arr[] = {tVals...};
    return std::transform_reduce(
        std::begin(val_arr), std::end(val_arr) - 1, std::begin(val_arr) + 1,
        bool{}, std::logical_and{}, [](auto const &l, auto const &r) {
          return r - l == static_cast<T>(1);
        });
  }
}

} // namespace cgui::bp

#endif // COMPONENT_GUI_TYPE_TRAITS_HPP

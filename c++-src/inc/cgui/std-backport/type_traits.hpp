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

/// Compile-time value wrapper. Can simplify non-type template parameter usage
/// as it allows for automatic deduction of its type.
/// \tparam T wrapped type
template <typename T> struct ct_value_wrapper {
  T value_;
  consteval explicit(false) ct_value_wrapper(T v) : value_(v) {}
  constexpr T raw() const { return value_; }

  constexpr auto operator<=>(const ct_value_wrapper &) const = default;
  constexpr bool operator==(const ct_value_wrapper &) const = default;
};
template <typename T> ct_value_wrapper(T) -> ct_value_wrapper<T>;
} // namespace cgui::bp

#endif // COMPONENT_GUI_TYPE_TRAITS_HPP

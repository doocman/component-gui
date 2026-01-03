//
// Created by rvons on 2024-08-20.
//

#ifndef COMPONENT_GUI_TYPE_TRAITS_HPP
#define COMPONENT_GUI_TYPE_TRAITS_HPP

#include <asp/import/stl.hpp>

namespace asp::bp {
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

template <typename T> struct remove_temp_ref {
  using type = T;
};
template <typename T> struct remove_temp_ref<T &&> {
  using type = std::remove_cvref_t<T>;
};
template <typename T> struct remove_temp_ref<T const &&> {
  using type = std::remove_cvref_t<T>;
};

template <typename T>
using remove_temp_ref_t = typename remove_temp_ref<T>::type;

template <typename L, typename R>
concept subtractable_with = requires(L&& lhs, R&& rhs) {
	lhs - rhs;
};
template <typename L, typename R>
concept addable_with = requires(L&& lhs, R&& rhs) {
	lhs + rhs;
};
template <typename L, typename R>
requires (subtractable_with<L, R>)
using subtract_result_t = decltype(std::declval<L&&>() - std::declval<R&&>());
template <typename L, typename R>
requires(addable_with<L, R>)
using addition_result_t = decltype(std::declval<L&&>() + std::declval<R&&>());

template <typename T, typename... Ts>
inline constexpr bool all_are_same_types = (std::is_same_v<T, Ts> && ...);

} // namespace asp::bp

#endif // COMPONENT_GUI_TYPE_TRAITS_HPP

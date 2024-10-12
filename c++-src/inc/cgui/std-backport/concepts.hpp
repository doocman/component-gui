//
// Created by rvons on 2024-07-24.
//

#ifndef COMPONENT_GUI_CONCEPTS_HPP
#define COMPONENT_GUI_CONCEPTS_HPP

#include <concepts>
#include <type_traits>
#include <utility>

namespace cgui::bp {
template <typename T>
concept pure_value = std::is_same_v<T, std::remove_cvref_t<T>>;
template <typename T, typename TRaw>
concept cvref_type =
    std::is_same_v<std::remove_cvref_t<T>, std::remove_cvref_t<TRaw>>;
template <typename T>
concept not_void = !std::is_void_v<T>;
template <typename T>
concept dereferencable = requires(T &&t) {
  { *std::forward<T>(t) } -> not_void;
};
template <typename T>
concept has_pointer_op = requires(T &&t) {
  { std::forward<T>(t).operator->() } -> not_void;
};
template <typename T>
concept pointer_type = std::is_pointer_v<T> && !cvref_type<T, std::nullptr_t>;

template <typename T, typename TFrom>
concept is_mutable_by =
    std::is_lvalue_reference_v<T> && std::is_assignable_v<T, TFrom>;

template <typename T, typename... TArgs>
concept invocable_or_invocable_args =
    std::invocable<T> || std::invocable<T, TArgs...>;
} // namespace cgui::bp

#endif // COMPONENT_GUI_CONCEPTS_HPP

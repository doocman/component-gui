//
// Created by rvons on 2024-07-24.
//

#ifndef COMPONENT_GUI_CONCEPTS_HPP
#define COMPONENT_GUI_CONCEPTS_HPP

#include <concepts>
#include <type_traits>
#include <utility>

namespace cgui::bp {
/// Concept that requires that the type has no reference nor volatile/const
/// qualifier.
/// \tparam T type to check
template <typename T>
concept pure_value = std::is_same_v<T, std::remove_cvref_t<T>>;

/// Concept that requires that a type is a potentially const/volatile qualified
/// reference or value-type of another type.
/// \tparam T Type to check
/// \tparam TRaw Type it should be if all const+volatile+ref is stripped from
/// both types.
template <typename T, typename TRaw>
concept cvref_type =
    std::is_same_v<std::remove_cvref_t<T>, std::remove_cvref_t<TRaw>>;

/// Concept stating that T is not void.
/// \tparam T
template <typename T>
concept not_void = !std::is_void_v<T>;

/// Constraint requiring that operator* is applicable to T.
/// \tparam T
template <typename T>
concept dereferencable = requires(T &&t) {
  { *std::forward<T>(t) } -> not_void;
};

/// Constraint requiring that operator-> is applicable to T.
/// \tparam T
template <typename T>
concept has_pointer_op = requires(T &&t) {
  { std::forward<T>(t).operator->() } -> not_void;
};

/// Concept stating that T is a pointer.
/// \tparam T
template <typename T>
concept pointer_type = std::is_pointer_v<T> && !cvref_type<T, std::nullptr_t>;

/// Concept that applies a simpler version of assignability than the standard
/// concept version.
/// \tparam T
/// \tparam TFrom
template <typename T, typename TFrom>
concept is_mutable_by =
    std::is_lvalue_reference_v<T> && std::is_assignable_v<T, TFrom>;

/// Concept stating that T should either be invocable without any args or
/// invocable with TArgs... types.
/// \tparam T
/// \tparam TArgs The optional args that T should potentially accept.
template <typename T, typename... TArgs>
concept invocable_or_invocable_args =
    std::invocable<T> || std::invocable<T, TArgs...>;

/// Used internally by cgui. Essentially states that given an operator Operator
/// and several operand arguments, requires that Operator(T, EachOperand[0]) &&
/// Operator(T, EachOperand[1]) &&... are all valid.
/// \tparam T Type to check
/// \tparam Operator operator to use
/// \tparam EachOperand right side arguments to apply.
template <typename T, typename Operator, typename... EachOperand>
concept can_be_operand_for_all =
    (std::invocable<Operator, T, EachOperand> && ...);

/// Concept to test if a type is purely stateless, i.e. no side-effects occur on
/// destruction and construction and it has no internal state.
/// \tparam T
template <typename T>
concept stateless =
    std::is_empty_v<T> && std::is_trivially_constructible_v<T> &&
    std::is_trivially_destructible_v<T>;

template <typename T>
concept empty_type = std::is_empty_v<T>;

template <typename T, typename U>
concept weakly_comparable_with_impl = requires(T &&t, U &&u) {
  { t == u } -> std::convertible_to<bool>;
};
template <typename T, typename U>
concept weakly_comparable_with =
    weakly_comparable_with_impl<T, U> && weakly_comparable_with_impl<U, T>;
template <typename T, typename U>
concept weakly_totally_ordered_with_impl = requires(T &&t, U &&u) {
  { t < u } -> std::convertible_to<bool>;
  { t > u } -> std::convertible_to<bool>;
  { t <= u } -> std::convertible_to<bool>;
  { t >= u } -> std::convertible_to<bool>;
};
template <typename T, typename U>
concept weakly_totally_ordered_with = weakly_totally_ordered_with_impl<T, U> &&
                                      weakly_totally_ordered_with_impl<U, T>;

template <typename T, typename... Args>
concept direct_invocable = requires(T &&t, Args &&...args) {
  std::forward<T>(t)(std::forward<Args>(args)...);
};

template <typename T>
concept value_incrementable = requires(T &t) {
  { ++t } -> std::same_as<T &>;
};
template <typename T>
concept value_decrementable = requires(T &t) {
  { --t } -> std::same_as<T &>;
};

template <typename T, typename... Ts>
concept same_as_any = (std::same_as<T, Ts> || ...);
} // namespace cgui::bp

#endif // COMPONENT_GUI_CONCEPTS_HPP


#ifndef COMPONENT_GUI_CGUI_AUTO_REF_HPP
#define COMPONENT_GUI_CGUI_AUTO_REF_HPP

#include <type_traits>

#include <cgui/cgui-call.hpp>
#include <cgui/std-backport/concepts.hpp>

namespace cgui {

/// A wrapper class that disables copy and move semantics for the underlying
/// type, effectively making the object default-constructible and immutable
/// in terms of its copy/move behavior.
///
/// Copy and move constructors are replaced with default constructors, and
/// copy and move assignments become no-operations.
///
/// \tparam T The underlying type to be wrapped. Must be default-constructible.
template <typename T>
  requires(std::is_default_constructible_v<T>)
class ignore_copy {
  T v_{};
  static constexpr bool _is_noexcept =
      std::is_nothrow_default_constructible_v<T>;

public:
  /// Constructs the wrapped type with the provided arguments, forwarding
  /// them to its constructor.
  ///
  /// \tparam Ts The types of the constructor arguments.
  /// \param args The arguments to forward to the constructor of the wrapped
  /// type.
  /// \note This constructor is only enabled if `T` is constructible with the
  /// given arguments.
  template <typename... Ts>
    requires(std::constructible_from<T, Ts...>)
  constexpr explicit ignore_copy(Ts &&...args) noexcept(
      std::is_nothrow_constructible_v<T, Ts...>)
      : v_(std::forward<Ts>(args)...) {}

  constexpr ignore_copy() noexcept(_is_noexcept) = default;
  constexpr ignore_copy(ignore_copy const &) noexcept(_is_noexcept) {}
  constexpr ignore_copy &operator=(ignore_copy const &) noexcept {
    return *this;
  }

  /// Provides mutable access to the wrapped value.
  ///
  /// \return A reference to the wrapped value.
  constexpr T &value() noexcept { return v_; }

  /// Provides read-only access to the wrapped value.
  ///
  /// \return A const reference to the wrapped value.
  constexpr T const &value() const noexcept { return v_; }

  /// No-op swap function.
  ///
  /// This function does nothing, as the object is designed to avoid
  /// modifications via copy or move operations.
  constexpr void swap(ignore_copy &) noexcept {}
};

/// A compile-time reference stack that allows hierarchical storage of
/// references. Each stack instance holds a reference to a current value and a
/// previous stack.
///
/// \tparam TCur Current reference type.
/// \tparam TPrevious Variadic template for the previous stack's references.
template <typename TCur, typename... TPrevious>
  requires(!std::is_reference_v<TCur> &&
           (!std::is_reference_v<TPrevious> && ...))
class reference_stack {
  reference_stack<TPrevious...> const *prev_;
  TCur *ref_;

public:
  /// Constructs a reference stack by appending a new reference to an existing
  /// stack.
  /// \param prev Reference to the previous stack.
  /// \param cur_ref Reference to the current value.
  constexpr reference_stack(reference_stack<TPrevious...> const &prev,
                            TCur &cur_ref) noexcept
      : prev_(&prev), ref_(std::addressof(cur_ref)) {}

  /// Returns the current reference.
  constexpr TCur &ref() const noexcept { return *ref_; }

  /// Returns the previous stack.
  constexpr auto const &previous() const noexcept { return *prev_; }

  /// Creates a new stack with an additional reference.
  /// \tparam T Type of the new reference.
  /// \param v Reference to be added to the stack.
  /// \return A new stack with the current stack and the new reference.
  template <typename T>
  constexpr reference_stack<T, TCur, TPrevious...> push(T &v) const noexcept {
    return {*this, v};
  }
};

/// Specialization of the reference stack for the base case.
/// \tparam TCur Type of the current reference.
template <typename TCur> class reference_stack<TCur> {
  TCur *ref_;

public:
  constexpr explicit reference_stack(TCur &cur_ref) noexcept
      : ref_(std::addressof(cur_ref)) {}

  constexpr TCur &ref() const noexcept { return *ref_; }

  /// Creates a 'new stack' object that holds all current elements + a reference
  /// to v.
  /// \tparam T type of 'v'
  /// \param v new value to have as the current reference.
  /// \return stack with appended 'v'.
  template <typename T>
  constexpr reference_stack<T, TCur> push(T &v) const noexcept {
    return {*this, v};
  }
};

template <typename... Ts>
constexpr std::size_t size(reference_stack<Ts...> const &) noexcept {
  return sizeof...(Ts);
}

template <typename T> reference_stack(T &) -> reference_stack<T>;
template <typename... Ts, typename T>
reference_stack(reference_stack<Ts...> const &,
                T &) -> reference_stack<T, Ts...>;

} // namespace cgui

#endif


#ifndef COMPONENT_GUI_CGUI_AUTO_REF_HPP
#define COMPONENT_GUI_CGUI_AUTO_REF_HPP

#include <type_traits>

#include <cgui/cgui-call.hpp>
#include <cgui/std-backport/concepts.hpp>

namespace cgui {

/// @brief A template for creating a parent-child relationship between object,
///        enabling parent objects to manage and reference child objects
///        dynamically.
/// @tparam CRTP The derived type used for the Curiously Recurring Template
/// Pattern (CRTP).
/// @tparam Inherit The base type this widget inherits from. Can be used with
/// e.g. empty_base_optimiser.
/// @tparam Children Variadic template arguments representing the child object
/// types.
template <typename CRTP, typename Inherit, typename... Children>
class auto_parent_inheriting : public Inherit {

  using this_t = auto_parent_inheriting<CRTP, Inherit, Children...>;

  constexpr CRTP &to_parent() noexcept { return static_cast<CRTP &>(*this); }

public:
  /// @brief A wrapper for managing references to child objects.
  ///
  /// The `child_reference` class ensures proper parent-child relationships
  /// in the context of the `auto_parent_inheriting` class. It allows each
  /// child object to maintain a reference to its parent while safely
  /// handling copy and move operations.
  ///
  /// Key properties:
  /// - The parent reference (`parent_`) is not copied or moved during
  ///   copy or move assignment, ensuring that the child retains its
  ///   original parent reference.
  /// - The value stored in the child reference is independent of the
  ///   parent and can be safely copied or moved.
  /// - Upon destruction of the `child_reference`, the parent is notified
  ///   via the `on_parent_destruct` mechanism to handle cleanup or other
  ///   responsibilities.
  ///
  /// @tparam T The type of the child object.
  template <typename T> class child_reference {
  public:
    /// @brief Alias for the parent type.
    using parent_t = CRTP;

  private:
    friend class auto_parent_inheriting<CRTP, Inherit, Children...>;
    parent_t *parent_ = nullptr;
    void set_parent(parent_t &p) { parent_ = &p; }
    T value_;

  public:
    /// @brief Retrieves the parent object reference.
    /// @return Pointer to the parent object. May be null.
    parent_t *parent() { return parent_; }

    /// @brief Copy assignment operator.
    /// @param other The child reference to copy from.
    /// @return Reference to the current object.
    constexpr child_reference &operator=(
        child_reference const &other) // NOLINT(*-unhandled-self-assignment)
        noexcept(std::is_nothrow_copy_assignable_v<T>) {
      value_ = other.value_;
      return *this;
    }

    /// @brief Move assignment operator.
    /// @param other The child reference to move from.
    /// @return Reference to the current object.
    constexpr child_reference &
    operator=(child_reference &&other) // NOLINT(*-unhandled-self-assignment)
        noexcept(std::is_nothrow_move_assignable_v<T>) {
      value_ = std::move(other.value_);
      return *this;
    }

    /// @brief Default constructor.
    constexpr child_reference() noexcept(
        std::is_nothrow_default_constructible_v<T>) = default;

    /// @brief Constructor with arguments for the child object.
    /// @param args Arguments used to construct the child object.
    template <typename... Ts>
      requires(std::constructible_from<T, Ts...>)
    constexpr explicit(sizeof...(Ts) == 1) child_reference(
        Ts &&...args) noexcept(std::is_nothrow_constructible_v<T, Ts...>)
        : value_(std::forward<Ts>(args)...) {}

    /// @brief Copy constructor.
    constexpr child_reference(child_reference const &other) noexcept(
        std::is_nothrow_copy_constructible_v<T>)
        : value_(other.value_) {}

    /// @brief Move constructor.
    constexpr child_reference(child_reference &&other) noexcept(
        std::is_nothrow_move_constructible_v<T>)
        : value_(std::move(other.value_)) {}

    /// @brief Accesses the value of the child widget.
    /// @return Reference to the value.
    constexpr T &value() noexcept { return value_; }

    /// @brief Accesses the value of the child widget.
    /// @return Const reference to the value.
    constexpr T const &value() const noexcept { return value_; }

    /// @brief Destructor, ensures the parent is notified when the child is
    /// destroyed.
    ~child_reference() {
      if (parent_ != nullptr) {
        parent_->on_parent_destruct(std::move(*this).value_);
      }
    }
  };

  using child_tuple_t = std::tuple<child_reference<Children>...>;

private:
  child_tuple_t children_;

  constexpr void update_children_ref() {
    call::for_each(children_, [this](auto &c) { c.set_parent(to_parent()); });
  }

public:
  /// @brief Constructor with child widget initialization.
  /// @param vars Arguments for constructing the child widgets.
  template <typename... Ts>
    requires(std::constructible_from<child_tuple_t, Ts...>)
  constexpr explicit(sizeof...(Ts) == 1)
      auto_parent_inheriting(Ts &&...vars) noexcept(
          std::is_nothrow_constructible_v<child_tuple_t, Ts...>)
      : children_(std::forward<Ts>(vars)...) {
    update_children_ref();
  }

  /// @brief Default constructor.
  constexpr auto_parent_inheriting() noexcept(
      std::is_nothrow_default_constructible_v<child_tuple_t>) {
    update_children_ref();
  }

  /// @brief Copy constructor.
  constexpr auto_parent_inheriting(
      auto_parent_inheriting const
          &other) noexcept(std::is_nothrow_copy_constructible_v<child_tuple_t>)
      : children_(other.children_) {
    update_children_ref();
  }

  /// @brief Move constructor.
  constexpr auto_parent_inheriting(auto_parent_inheriting &&other) noexcept(
      std::is_nothrow_move_constructible_v<child_tuple_t>)
      : children_(std::move(other).children_) {
    update_children_ref();
  }

  /// @brief Copy assignment operator.
  constexpr auto_parent_inheriting &
  operator=(auto_parent_inheriting const &) noexcept(
      std::is_nothrow_copy_assignable_v<child_tuple_t>) = default;

  /// @brief Move assignment operator.
  constexpr auto_parent_inheriting &
  operator=(auto_parent_inheriting &&) noexcept(
      std::is_nothrow_move_assignable_v<child_tuple_t>) = default;

  /// @brief Access the children. Useful in inheritance context.
  /// @return Tuple-like object that can give structured bindings for the child
  /// references.
  constexpr this_t &children() noexcept { return *this; }

  /// @brief Access the children. Useful in inheritance context.
  /// @return Tuple-like object that can give structured bindings for the child
  /// const references.
  constexpr this_t const &children() const noexcept { return *this; }

  /// @brief Helper for tuple-like access function.
  /// @tparam ti Index of child object to fetch.
  /// @return child no. ti.
  template <std::size_t ti, bp::cvref_type<this_t> T>
  static constexpr auto &&do_get(T &&t) noexcept {
    return std::get<ti>(std::forward<T>(t).children_);
  }
};

/// @brief Tuple-like access function.
/// @tparam ti Index of child object to fetch.
/// @return child no. ti.
template <std::size_t ti, typename T>
  requires(requires(T &&t) {
    typename std::remove_cvref_t<T>::child_tuple_t;
    std::remove_cvref_t<T>::template do_get<ti>(t);
  })
constexpr auto &&get(T &&t) noexcept {
  return std::remove_cvref_t<T>::template do_get<ti>(std::forward<T>(t));
}

/// @brief Shortcut for an auto_parent_inheriting that has no meaningful
/// inheritance.
/// @tparam CRTP The derived type for CRTP.
/// @tparam Children Variadic template arguments representing the child widget
/// types.
template <typename CRTP, typename... Children>
using auto_parent =
    auto_parent_inheriting<CRTP, empty_placeholder_t, Children...>;

} // namespace cgui

namespace std {
template <typename CRTP, typename T2, typename... Cs>
struct tuple_size<::cgui::auto_parent_inheriting<CRTP, T2, Cs...>>
    : tuple_size<std::tuple<Cs...>> {};
template <std::size_t ti, typename CRTP, typename T2, typename... Cs>
struct tuple_element<ti, ::cgui::auto_parent_inheriting<CRTP, T2, Cs...>>
    : tuple_element<ti, typename ::cgui::auto_parent_inheriting<
                            CRTP, T2, Cs...>::child_tuple_t> {};
} // namespace std

#endif

//
// Created by rvons on 2024-07-22.
//

#ifndef COMPONENT_GUI_UTILITY_HPP
#define COMPONENT_GUI_UTILITY_HPP

#include <utility>

#include <cgui/std-backport/concepts.hpp>
#include <cgui/warnings.hpp>

namespace cgui::bp {

[[noreturn]] inline void unreachable() {
#if defined(_MSC_VER) && !defined(__clang__)
  __assume(false);
#elif defined(__GNUC__) || defined(__clang__)
  __builtin_unreachable();
#endif
}

#if __cpp_lib_forward_like >= 202207L
using std::forward_like;
#else
template <class T, class U> constexpr auto &&forward_like(U &&x) noexcept {
  constexpr bool is_adding_const = std::is_const_v<std::remove_reference_t<T>>;
  if constexpr (std::is_lvalue_reference_v<T &&>) {
    if constexpr (is_adding_const)
      return std::as_const(x);
    else
      return static_cast<U &>(x);
  } else {
    if constexpr (is_adding_const)
      return std::move(std::as_const(x));
    else
      return std::move(x);
  }
}
#endif
template <typename T, typename TToCopy>
using copy_cvref_t = decltype(bp::forward_like<TToCopy>(std::declval<T>()));

template <typename T> struct as_forward {
  T &&val_;

  constexpr explicit(false) as_forward(T &&v) : val_(std::forward<T>(v)) {}
  constexpr explicit as_forward(T &v)
    requires(!std::is_lvalue_reference_v<T>)
      : val_(std::forward<T>(v)) {}

  constexpr T &&value() const noexcept { return std::forward<T>(val_); }
  constexpr T &&operator*() const noexcept { return value(); }
};

template <typename T> as_forward(T &&) -> as_forward<T>;

template <typename T, typename TArg, typename TIn>
constexpr auto forward_cast(TIn &&arg)
    -> decltype(bp::forward_like<TArg>(std::declval<T &>())) {
  return static_cast<decltype(bp::forward_like<TArg>(std::declval<T &>()))>(
      arg);
}

template <typename T, typename Type, std::size_t index>
concept get_arg = std::is_same_v<T, std::type_identity<Type>> ||
                  std::is_same_v<T, std::integral_constant<std::size_t, index>>;
template <typename T, typename Tag>
concept member_get = requires(T &&t, Tag tag) { std::forward<T>(t).get(tag); };
template <typename T, typename TQual, typename Tag>
concept static_get =
    requires(TQual &&t, Tag tag) { T::get(std::forward<TQual>(t), tag); };

template <std::size_t tIndex, typename... Ts>
struct empty_structs_optimiser_impl;
template <std::size_t tIndex> struct empty_structs_optimiser_impl<tIndex> {
  static constexpr void get() {}
};
template <std::size_t tIndex, typename T, typename... Ts>
  requires(std::is_empty_v<T>)
struct empty_structs_optimiser_impl<tIndex, T, Ts...>
    : empty_structs_optimiser_impl<tIndex + 1, Ts...> {
  using _base_t = empty_structs_optimiser_impl<tIndex + 1, Ts...>;
  constexpr empty_structs_optimiser_impl() noexcept(
      std::is_nothrow_default_constructible_v<T> &&
      std::is_nothrow_default_constructible_v<_base_t>) = default;
  template <typename TArg, typename... TArgs>
    requires(std::constructible_from<_base_t, TArgs && ...>)
  constexpr explicit empty_structs_optimiser_impl(TArg &&, TArgs &&...to_base)
      : _base_t(std::forward<TArgs>(to_base)...) {}

  template <typename Tag>
    requires(get_arg<Tag, T, tIndex> || member_get<_base_t, Tag>)
  static constexpr T get(Tag) {
    return T{};
  }
  template <bp::cvref_type<empty_structs_optimiser_impl> TSelf, typename Tag>
    requires(get_arg<Tag, T, tIndex> || member_get<_base_t, Tag>)
  static constexpr T get(TSelf &&, Tag) {
    return T{};
  }
  using _base_t::get;
};

template <std::size_t tIndex, typename T, typename... Ts>
  requires(!std::is_empty_v<T>)
struct empty_structs_optimiser_impl<tIndex, T, Ts...>
    : empty_structs_optimiser_impl<tIndex + 1, Ts...> {
  using _base_t = empty_structs_optimiser_impl<tIndex + 1, Ts...>;
  using _this_t = empty_structs_optimiser_impl;
  T val_;
  constexpr empty_structs_optimiser_impl() noexcept(
      std::is_nothrow_default_constructible_v<T> &&
      std::is_nothrow_default_constructible_v<_base_t>) = default;
  template <typename TArg, typename... TArgs>
    requires(std::constructible_from<T, TArg &&> &&
             std::constructible_from<_base_t, TArgs && ...>)
  constexpr explicit empty_structs_optimiser_impl(TArg &&arg,
                                                  TArgs &&...to_base)
      : _base_t(std::forward<TArgs>(to_base)...),
        val_(std::forward<TArg>(arg)) {}

  template <bp::cvref_type<empty_structs_optimiser_impl> TSelf, typename Tag>
    requires(get_arg<Tag, T, tIndex> || static_get<_base_t, _base_t, Tag>)
  static constexpr auto && get(TSelf &&self, Tag tag) {
    auto s =
        bp::as_forward(forward_cast<empty_structs_optimiser_impl, TSelf>(self));
    if constexpr (get_arg<Tag, T, tIndex>) {
      static_assert(!member_get<_base_t, Tag>, "Ambigouos get");
      return (*s).val_;
    } else {
      return _base_t::get(*s, tag);
    }
  }

  template <typename Tag>
    requires(static_get<_this_t, _this_t &&, Tag>)
  constexpr auto &&get(Tag tag) && {
    return _this_t::get(std::move(*this), tag);
  }
  template <typename Tag>
    requires(static_get<_this_t, _this_t &, Tag>)
  constexpr auto &get(Tag tag) & {
    return _this_t::get(*this, tag);
  }
  template <get_arg<T, tIndex> Tag>
    requires(static_get<_this_t, _this_t const &&, Tag>)
  constexpr T const &&get(Tag tag) const && {
    return _this_t::get(std::move(*this), tag);
  }
  template <get_arg<T, tIndex> Tag>
    requires(static_get<_this_t, _this_t const &, Tag>)
  constexpr T const &get(Tag tag) const & {
    return _this_t::get(*this, tag);
  }
};

template <typename... Ts>
using empty_structs_optimiser = empty_structs_optimiser_impl<0u, Ts...>;

template <typename T> class deferred {
  T val_;

public:
  constexpr explicit deferred(T in) : val_(std::move(in)) {}
  constexpr ~deferred() { val_(); }
  deferred(deferred &&) = delete;
  deferred &operator=(deferred &&) = delete;
};

constexpr void run_for_each(auto &&cb, auto &&...vals)
  requires(std::invocable<decltype(cb), decltype(vals)> && ...)
{
  auto cb_return = [&cb]<typename T>(T &&v) {
    cb(std::forward<T>(v));
    return '\0';
  };
  using expander = char const[sizeof...(vals)];
  unused(expander{cb_return(std::forward<decltype(vals)>(vals))...});
}

} // namespace cgui::bp

#endif // COMPONENT_GUI_UTILITY_HPP

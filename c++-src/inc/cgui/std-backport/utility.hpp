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

template <std::size_t tI>
using index_constant = std::integral_constant<std::size_t, tI>;

template <typename T> struct as_forward {
  T &&val_;

  constexpr explicit(false) as_forward(T &&v) : val_(std::forward<T>(v)) {}
  constexpr explicit as_forward(T &v)
    requires(!std::is_lvalue_reference_v<T>)
      : val_(std::forward<T>(v)) {}

  constexpr T &&value() const noexcept { return std::forward<T>(val_); }
  constexpr T &&operator*() const noexcept { return value(); }
  constexpr std::remove_reference_t<T> &as_ref() noexcept { return val_; }
  constexpr std::add_const_t<std::remove_reference_t<T>> &
  as_cref() const noexcept {
    return val_;
  }
};

template <typename T> as_forward(T &&) -> as_forward<T>;

template <typename T, typename TArg, typename TIn>
constexpr auto forward_cast(TIn &&arg)
    -> decltype(bp::forward_like<TArg>(std::declval<T &>())) {
  return static_cast<decltype(bp::forward_like<TArg>(std::declval<T &>()))>(
      arg);
}

template <typename F, typename T1, typename T2>
concept invocable_arg1_or_arg1_2 =
    std::invocable<F, T1> || std::invocable<F, T1, T2>;

template <typename F, typename T1, typename T2>
  requires(invocable_arg1_or_arg1_2<F, T1, T2>)
constexpr decltype(auto) invoke_arg1_or_arg1_2(F &&cb, T1 &&arg1, T2 &&arg2) {
  auto cbf = bp::as_forward<F>(cb);
  auto a1f = bp::as_forward<T1>(arg1);
  auto a2f = bp::as_forward<T2>(arg2);
  if constexpr (std::invocable<F &&, T1 &&, T2 &&>) {
    return std::invoke(*cbf, *a1f, *a2f);
  } else {
    return std::invoke(*cbf, *a1f);
  }
}

namespace impl {
template <typename T, typename Type, std::size_t index>
concept get_arg = std::is_same_v<T, std::type_identity<Type>> ||
                  std::is_same_v<T, index_constant<index>>;
template <typename T, typename Tag>
concept member_get = requires(T &&t, Tag tag) { std::forward<T>(t).get(tag); };
template <typename T, typename Tag>
concept static_get = requires(T &&t, Tag tag) {
  std::remove_cvref_t<T>::get(std::forward<T>(t), tag);
};

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
  using _this_t = empty_structs_optimiser_impl;
  constexpr empty_structs_optimiser_impl() noexcept(
      std::is_nothrow_default_constructible_v<T> &&
      std::is_nothrow_default_constructible_v<_base_t>) = default;
  template <typename TArg, typename... TArgs>
    requires(std::constructible_from<_base_t, TArgs && ...>)
  constexpr explicit(sizeof...(TArgs) == 0)
      empty_structs_optimiser_impl(TArg &&, TArgs &&...to_base)
      : _base_t(std::forward<TArgs>(to_base)...) {}

  template <bp::cvref_type<empty_structs_optimiser_impl> TSelf, typename Tag>
    requires(get_arg<Tag, T, tIndex>)
  static constexpr T get(TSelf &&, Tag) {
    static_assert(!static_get<copy_cvref_t<_base_t, TSelf>, Tag>,
                  "Ambigouos get");
    return T{};
  }
  template <bp::cvref_type<empty_structs_optimiser_impl> TSelf, typename Tag,
            typename TBase = copy_cvref_t<_base_t, TSelf>>
    requires(static_get<TBase, Tag>)
  static constexpr decltype(auto) get(TSelf &&self, Tag tag) {
    return _base_t::get(static_cast<TBase>(self), tag);
  }
  template <typename Tag>
    requires static_get<_this_t, Tag>
  constexpr decltype(auto) get(Tag tag) & {
    return _this_t::get(*this, tag);
  }
  template <typename Tag>
    requires static_get<_this_t, Tag>
  constexpr decltype(auto) get(Tag tag) const & {
    return _this_t::get(*this, tag);
  }
  template <typename Tag>
    requires static_get<_this_t, Tag>
  constexpr decltype(auto) get(Tag tag) && {
    return _this_t::get(std::move(*this), tag);
  }
  template <typename Tag>
    requires static_get<_this_t, Tag>
  constexpr decltype(auto) get(Tag tag) const && {
    return _this_t::get(std::move(*this), tag);
  }

  constexpr decltype(auto) get_first()
    requires(tIndex == 0)
  {
    return _this_t::get(*this, index_constant<tIndex>{});
  }
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
  constexpr explicit(sizeof...(TArgs) == 0)
      empty_structs_optimiser_impl(TArg &&arg, TArgs &&...to_base)
      : _base_t(std::forward<TArgs>(to_base)...),
        val_(std::forward<TArg>(arg)) {}

  template <bp::cvref_type<empty_structs_optimiser_impl> TSelf, typename Tag,
            typename TBase = copy_cvref_t<_base_t, TSelf>>
    requires(static_get<TBase, Tag>)
  static constexpr decltype(auto) get(TSelf &&self, Tag tag) {
    return _base_t::get(static_cast<TBase>(self), tag);
  }

  template <typename TSelf, typename Tag>
    requires(get_arg<Tag, T, tIndex>)
  static constexpr auto &&get(TSelf &&self, Tag) {
    static_assert(!static_get<copy_cvref_t<_base_t, TSelf>, Tag>,
                  "Ambigouos get");
    using this_t = copy_cvref_t<_this_t, TSelf &&>;
    return static_cast<this_t>(self).val_;
  }

  template <typename Tag>
    requires(static_get<_this_t, Tag>)
  constexpr decltype(auto) get(Tag tag) && {
    return _this_t::get(std::move(*this), tag);
  }
  template <typename Tag>
    requires(static_get<_this_t &, Tag>)
  constexpr decltype(auto) get(Tag tag) & {
    return _this_t::get(*this, tag);
  }
  template <typename Tag>
    requires(static_get<_this_t const, Tag>)
  constexpr decltype(auto) get(Tag tag) const && {
    return _this_t::get(std::move(*this), tag);
  }
  template <typename Tag>
    requires(static_get<_this_t const &, Tag>)
  constexpr decltype(auto) get(Tag tag) const & {
    return _this_t::get(*this, tag);
  }

  // double parenthesis due to clang-format bug.
  constexpr decltype(auto) get_first() &&
    requires((tIndex == 0))
  {
    return _this_t::get(std::move(*this), index_constant<tIndex>{});
  }
  constexpr decltype(auto) get_first() const &&
    requires((tIndex == 0))
  {
    return _this_t::get(std::move(*this), index_constant<tIndex>{});
  }
  constexpr decltype(auto) get_first() &
    requires((tIndex == 0))
  {
    return _this_t::get(*this, index_constant<tIndex>{});
  }
  constexpr decltype(auto) get_first() const &
    requires((tIndex == 0))
  {
    return _this_t::get(*this, index_constant<tIndex>{});
  }
};

template <std::size_t tI, typename T>
  requires impl::static_get<T, index_constant<tI>>
constexpr decltype(auto) get(T &&t) {
  return std::remove_cvref_t<T>::get(std::forward<T>(t), index_constant<tI>{});
}
} // namespace impl
} // namespace cgui::bp
namespace std {
template <std::size_t tI, typename... Ts>
struct tuple_size<cgui::bp::impl::empty_structs_optimiser_impl<tI, Ts...>>
    : std::integral_constant<std::size_t, tI + sizeof...(Ts)> {};
template <std::size_t i, std::size_t tI, typename... Ts>
struct tuple_element<i,
                     cgui::bp::impl::empty_structs_optimiser_impl<tI, Ts...>> {
  using type = decltype(get<i>(
      declval<cgui::bp::impl::empty_structs_optimiser_impl<tI, Ts...>>()));
};
template <std::size_t i, std::size_t tI, typename... Ts>
struct tuple_element<
    i, cgui::bp::impl::empty_structs_optimiser_impl<tI, Ts...> const> {
  using type = decltype(get<i>(
      declval<
          cgui::bp::impl::empty_structs_optimiser_impl<tI, Ts...> const>()));
};
} // namespace std
namespace cgui::bp {
namespace impl {

template <typename T, std::size_t... is, typename F>
  requires(std::invocable<F &&, decltype(get<is>(std::declval<T &&>()))...>)
constexpr decltype(auto) do_apply_to(T &&t, F &&f, std::index_sequence<is...>) {
  return std::invoke(std::forward<F>(f), get<is>(std::forward<T>(t))...);
}

template <typename T, typename F>
  requires(requires(T &&t, F &&f) {
    do_apply_to(
        std::forward<T>(t), std::forward<F>(f),
        std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<T>>>());
  })
constexpr decltype(auto) apply_to(T &&t, F &&f) {
  return do_apply_to(
      std::forward<T>(t), std::forward<F>(f),
      std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<T>>>());
}

} // namespace impl
template <typename... Ts>
using empty_structs_optimiser = impl::empty_structs_optimiser_impl<0u, Ts...>;

template <typename T> class deferred {
  T val_;

public:
  constexpr explicit deferred(T in) : val_(std::move(in)) {}
  constexpr ~deferred() { val_(); }
  deferred(deferred &&) = delete;
  deferred &operator=(deferred &&) = delete;
};

constexpr void run_for_each(auto &&cb, auto &&...vals)
  requires(
      invocable_arg1_or_arg1_2<decltype(cb), decltype(vals), std::size_t> &&
      ...)
{
  auto cb_return = [f =
                        [&cb]<typename T>(bp::as_forward<T> t, std::size_t i) {
                          invoke_arg1_or_arg1_2(cb, *t, i);
                        }]<typename... Ts, std::size_t... is>(
                       std::index_sequence<is...>, Ts &&...vs) {
    using expander = char const[sizeof...(vals)];
    auto wrem = [&f]<typename... Us>(Us &&...args) {
      (void)f(std::forward<Us>(args)...);
      return char{};
    };
    unused(expander{wrem(bp::as_forward<Ts>(vs), is)...});
  };
  cb_return(std::make_index_sequence<sizeof...(vals)>{},
            std::forward<decltype(vals)>(vals)...);
  // unused(expander{cb_return(std::forward<decltype(vals)>(vals))...});
}

} // namespace cgui::bp

#endif // COMPONENT_GUI_UTILITY_HPP

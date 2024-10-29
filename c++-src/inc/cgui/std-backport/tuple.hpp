#ifndef COMPONENT_GUI_TUPLE_HPP
#define COMPONENT_GUI_TUPLE_HPP

#include <tuple>

#include <cgui/std-backport/utility.hpp>

namespace cgui::bp {
/// Concept to find if tuple_size_v is valid for T.
/// \tparam T
template <typename T>
concept has_tuple_size =
    requires() { std::tuple_size<std::remove_cvref_t<T>>::value; };
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

  constexpr decltype(auto) get_first() const
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
      requires((tIndex == 0)) {
        return _this_t::get(std::move(*this), index_constant<tIndex>{});
      } constexpr decltype(auto) get_first() const &&
        requires((tIndex == 0))
  {
    return _this_t::get(std::move(*this), index_constant<tIndex>{});
  }
  constexpr decltype(auto) get_first() &
      requires((tIndex == 0)) {
        return _this_t::get(*this, index_constant<tIndex>{});
      } constexpr decltype(auto) get_first() const &
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

template <has_tuple_size T, typename F>
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

/// @brief Struct to remove empty objects by letting them be
/// default-constructible on-the-fly instead.
///
/// You may use get<N>(OBJECT) or get<T>(OBJECT) or
/// OBJECT.get(index_constant<N>{}) to get the object.
///
/// @tparam Ts Objects to store
template <typename... Ts>
using empty_structs_optimiser = impl::empty_structs_optimiser_impl<0u, Ts...>;

template <typename, typename> struct tuple_element_index;

namespace impl {
template <typename T, typename U, typename... Us, std::size_t i,
          std::size_t... is>
consteval std::size_t find_element_index(std::index_sequence<i, is...>) {
  if constexpr (std::is_same_v<T, U>) {
    return i;
  } else {
    return find_element_index<T, Us...>(std::index_sequence<is...>{});
  }
}
} // namespace impl
/// Trait to find the index in which a specific type resides inside a tuple.
/// \tparam T Type to find
/// \tparam Ts Types in tuple
template <typename T, typename... Ts>
  requires((std::is_same_v<T, Ts> || ...) &&
           requires(std::tuple<Ts...> &v) { std::get<T>(v); })
struct tuple_element_index<T, std::tuple<Ts...>> {
  static constexpr auto value = impl::find_element_index<T, Ts...>(
      std::make_index_sequence<sizeof...(Ts)>());
};
/// Value of tuple_element_index<T, U>::value.
/// \tparam T Type to find
/// \tparam U tuple that contains all the types.
template <typename T, typename U>
constexpr auto tuple_element_index_v = tuple_element_index<T, U>::value;

} // namespace cgui::bp

#endif // COMPONENT_GUI_TUPLE_HPP

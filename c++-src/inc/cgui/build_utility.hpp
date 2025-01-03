
#ifndef COMPONENT_GUI_BUILD_UTILITY_HPP
#define COMPONENT_GUI_BUILD_UTILITY_HPP

#include <tuple>

#include <cgui/cgui-call.hpp>
#include <cgui/cgui-types.hpp>
#include <cgui/std-backport/ranges.hpp>

namespace cgui::build {
/// Helper type to retrieve the 'tags' used to get all elements in a tuple-like
/// object. std-types will have index_sequence here for example.
template <typename> struct all_tuple_tags {};
/// Trait-type that can create a new template object based on an instantiated
/// template type and new template types.
template <typename> struct template_base {};
template <typename... Ts> struct all_tuple_tags<std::tuple<Ts...>> {
  using type = std::make_index_sequence<sizeof...(Ts)>;
};
template <typename T, std::size_t sz> struct all_tuple_tags<std::array<T, sz>> {
  using type = std::make_index_sequence<sz>;
};
template <typename T, typename U> struct all_tuple_tags<std::pair<T, U>> {
  using type = std::make_index_sequence<2>;
};
template <typename T>
  requires(!bp::pure_value<T>)
struct all_tuple_tags<T> : all_tuple_tags<std::remove_cvref_t<T>> {};
template <typename... Ts> struct template_base<std::tuple<Ts...>> {
  template <typename... Args>
  static constexpr auto decay_make(Args &&...args)
      -> decltype(std::tuple{std::forward<Args>(args)...}) {
    return std::tuple{std::forward<Args>(args)...};
  }
  template <typename... Args>
  static constexpr auto raw_make(Args &&...args) -> std::tuple<Args...> {
    return std::tuple<Args...>{std::forward<Args>(args)...};
  }
};
template <typename T, std::size_t sz> struct template_base<std::array<T, sz>> {
  template <typename... Args> static constexpr auto decay_make(Args &&...args) {
    return std::array{{std::forward<Args>(args)...}};
  }
  template <typename... Args, typename Res = std::common_type_t<Args...>>
  static constexpr auto raw_make(Args &&...args) {
    return std::array<Res, sizeof...(Args)>{{std::forward<Args>(args)...}};
  }
};

template <typename T> using all_tuple_tags_t = typename all_tuple_tags<T>::type;
template <typename T>
using template_base_t = template_base<std::remove_cvref_t<T>>;

/// Concept that hints that build_tuple is a good function to use to build the
/// type. The bp::has_tuple_size<T> is in general redundant but has helped
/// fighting of an 'internal compiler error' in MSVC-22.
/// \tparam T
template <typename T>
concept usable_in_build_tuples =
    bp::has_tuple_size<T> || requires() { typename all_tuple_tags<T>::type; };

template <typename Constraint, typename B, typename... Args>
  requires(std::invocable<Constraint, B> ||
           std::invocable<Constraint, std::unwrap_ref_decay_t<B>> ||
           (builder<B, Args...> &&
            std::invocable<Constraint, build_result_t<B, Args...>>))
constexpr decltype(auto) return_or_build(B &&b, Args &&...args) {
  if constexpr (std::invocable<Constraint, B>) {
    return std::forward<B>(b);
  } else if constexpr (std::invocable<Constraint, std::unwrap_ref_decay_t<B>>) {
    return static_cast<std::unwrap_ref_decay_t<B>>(b);
  } else {
    return call::build(std::forward<B>(b), std::forward<Args>(args)...);
  }
}

template <typename T, typename Constraint, typename... Args>
concept fulfill_or_after_build =
    requires(bp::as_forward<T> t, bp::as_forward<Args>... args) {
      return_or_build<Constraint>(*t, *args...);
    };

namespace impl {
inline namespace {
template <typename Constraint, typename... Args>
constexpr auto optional_builder(Args &&...args) {
  return [at = std::tuple(bp::as_forward<Args>(args)...)]<typename B>(
             B &&b) -> decltype(auto)
           requires(std::invocable<Constraint, B> || builder<B, Args...>)
  {
    return std::apply(
        [&b](auto &&...afs) -> decltype(auto) {
          return return_or_build<Constraint>(std::forward<B>(b), *afs...);
        },
        at);
  };
}
} // namespace
} // namespace impl

/// Builds the tuple by going through each element and either builds them or
/// return them as is depending on what Builder does.
/// \tparam Tup tuple to use
/// \tparam Builder invocable that may build or return the value as-is.
/// \tparam tIs indices of the tuple.
/// \param t tuple to use
/// \param b invocable that may build or return the value as-is.
/// \return a new tuple-like object (i.e. if Tup is a std::tuple, the returned
/// value is a tuple, if Tup is a std::array, the returned value will be a new
/// array etc.)
template <typename Tup, typename Builder, std::size_t... tIs>
constexpr auto build_tuple(Tup &&t, std::index_sequence<tIs...>,
                           Builder const &b) {
  return template_base_t<Tup>::raw_make(
      b(std::get<tIs>(std::forward<Tup>(t)))...);
}

/// Combines several arguments to a std::tuple. Requires that TElementConstraint
/// can be invoked for each TD2, which is used to inject a constraint to the
/// function.
/// \tparam TElementConstraint function-like object whose purpose is only to act
/// as a constraint for the function.
/// \tparam TD2 Arguments to combine. Use std::ref to get keep references.
/// \tparam TTuple Resulting type.
/// \param d2 Arguments to combine. Use std::ref to get keep references.
/// \return TTuple-type of all d2.
template <typename TElementConstraint, typename... TD2,
          typename TTuple = std::tuple<std::unwrap_ref_decay_t<TD2>...>>
  requires((std::invocable<TElementConstraint, std::unwrap_ref_decay_t<TD2>> &&
            ...))
TTuple args_to_group(TElementConstraint, TD2 &&...d2) {
  return TTuple(std::forward<TD2>(d2)...);
}

/// Returns the object essentially unmodified with the exception of a unwrap.
/// Use std::ref to keep a reference to the underlying group.
/// \tparam TElementConstraint Constraint to apply to all elements inside the
/// group.
/// \tparam TGroup2 Group-type. Requires that call::for_each(TGroup2, FUNCTION)
/// is valid for any function that accepts all elements contained in TGroup2.
/// \tparam TG2UW Resulting type
/// \param g Group to unwrap and return.
/// \return the incoming group.
template <typename TElementConstraint, typename TGroup2,
          typename TG2UW = std::unwrap_ref_decay_t<TGroup2>>
  requires each_constraint<TG2UW, TElementConstraint>
TG2UW args_to_group(TElementConstraint, TGroup2 &&g) {
  return TG2UW(std::forward<TGroup2>(g));
}

#if CGUI_HAS_NAMED_ARGS
template <typename... Ts, dooc::template_string... tags>
struct all_tuple_tags<dooc::named_tuple<dooc::named_arg_t<tags, Ts>...>> {
  using type = dooc::template_string_list_t<tags...>;
};

/// dooc-np overload of args_to_group. Uses named_args to create a named_tuple.
/// \tparam TElementConstraint
/// \tparam TArgs
/// \param args named_arg_t:s to combine into a named_tuple.
/// return a named_tuple containing all args.
template <typename TElementConstraint, typename... TArgs>
  requires(((dooc::arg_with_any_name<std::unwrap_ref_decay_t<TArgs>> &&
             std::invocable<
                 TElementConstraint,
                 std::unwrap_ref_decay_t<
                     typename dooc::named_arg_properties<TArgs>::type> &>) &&
            ...))
constexpr auto args_to_group(TElementConstraint, TArgs &&...args) {
  using tuple_t = dooc::named_tuple<std::remove_cvref_t<TArgs>...>;
  return tuple_t(std::forward<TArgs>(args)...);
}

/// Wraps t with any reference and cv-qualifier into a named_arg_t.
/// \tparam tName tag to give the resulting argument
/// \tparam T type of argument
/// \param t value of argument
/// \return named argument containing t.
template <dooc::template_string tName, typename T>
constexpr dooc::named_arg_t<tName, T> forward_named_arg(T &&t) {
  return dooc::named_arg_t<tName, T>{std::forward<T>(t)};
}

/// Overload for dooc-np tuple.
/// \tparam Tup named_tuple to build.
/// \tparam Builder Function-like object that handles the build-or-forward
/// logic.
/// \tparam tags all tags in Tup.
/// \param t named_tuple to build.
/// \param b Function-like object that handles the build-or-forward logic.
/// \return new named tuple where all elements are either forwarded from or
/// built from b.
template <typename Tup, typename Builder, dooc::template_string... tags>
constexpr auto build_tuple(Tup &&t, dooc::template_string_list_t<tags...>,
                           Builder const &b) {
  return dooc::named_tuple(
      forward_named_arg<tags>(b(get<tags>(std::forward<Tup>(t))))...);
}
#endif

/// Takes a group (anything that satisfies call::for_each or call::apply_to) and
/// either builds or forwards all their elements to a new group, depending on
/// Constraint.
/// \tparam Constraint function-like object that is used to inject a constraint
/// to determine whether a type should be forwarded as-is or built.
/// \tparam T type of the group
/// \tparam TArgs arguments needed for any potential builds.
/// \param g group to build-or-forward from.
/// \param args arguments needed for any potential builds.
/// \return new group in which all elements satisfies Constraint.
template <typename Constraint, typename T, typename... TArgs>
constexpr auto build_group(Constraint &&, T &&g, TArgs &&...args) {
  auto gf = bp::as_forward<T>(g);
  auto b_args = impl::optional_builder<Constraint, TArgs &&...>(
      std::forward<TArgs>(args)...);
  if constexpr (std::invocable<decltype(b_args), T>) {
    return b_args(*gf);
  } else {
    if constexpr (std::ranges::range<std::remove_cvref_t<T>>) {
      return bp::transform_range(std::forward<T>(g), std::move(b_args));
    } else if constexpr (usable_in_build_tuples<T>) {
      return build_tuple(*gf, all_tuple_tags_t<T>{}, std::move(b_args));
    } else {
      static_assert(std::is_void_v<T>, "Type does not satisfy constraint");
    }
  }
}

template <typename TConstraint, typename... TArgs>
using args_to_group_t =
    decltype(args_to_group(TConstraint{}, std::declval<TArgs &&>()...));

template <typename Constraint, typename Group, typename... Args>
using build_group_t =
    decltype(build_group(std::declval<Constraint>(), std::declval<Group>(),
                         std::declval<Args>()...));

} // namespace cgui::build

#endif // COMPONENT_GUI_BUILD_UTILITY_HPP

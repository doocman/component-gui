
#ifndef COMPONENT_GUI_BUILD_UTILITY_HPP
#define COMPONENT_GUI_BUILD_UTILITY_HPP

#include <tuple>

#include <cgui/cgui-call.hpp>

namespace cgui::build {
template <typename> struct all_tuple_tags {};
template <typename> struct template_base {};
template <typename T>
  requires(bp::has_tuple_size<T> && bp::pure_value<T>)
struct all_tuple_tags<T> {
  using type = std::make_index_sequence<std::tuple_size_v<T>>;
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
  static constexpr auto raw_make(Args &&...args)
      -> std::tuple<Args...> {
    return std::tuple<Args...>{std::forward<Args>(args)...};
  }
};
template <typename T, std::size_t sz> struct template_base<std::array<T, sz>> {
  template <typename... Args> static constexpr auto decay_make(Args &&...args) {
    return std::array{{std::forward<Args>(args)...}};
  }
  template <typename... Args, typename Res = std::common_type_t<Args...>> static constexpr auto raw_make(Args &&...args) {
    return std::array<Res, sizeof...(Args)>{{std::forward<Args>(args)...}};
  }
};

template <typename T> using all_tuple_tags_t = typename all_tuple_tags<T>::type;
template <typename T>
using template_base_t = template_base<std::remove_cvref_t<T>>;

template <typename T>
concept usable_in_build_tuples =
    requires() { typename all_tuple_tags<T>::type; };

constexpr decltype(auto) widget_build_or_forward(auto &&v, auto const &states) {
  if constexpr (display_component<decltype(v)>) {
    return std::forward<decltype(v)>(v);
  } else {
    return call::build(std::forward<decltype(v)>(v), states);
  }
}

namespace impl {
inline namespace {
template <typename Constraint, typename... Args>
constexpr auto optional_builder(Args &&...args) {
  return [at = std::tuple(bp::as_forward<Args>(args)...)]<typename B>(
             B &&b) -> decltype(auto) requires(std::invocable<Constraint, B> || builder<B, Args...>) {
    if constexpr (std::invocable<Constraint, B>) {
      return std::forward<B>(b);
    } else {
      return std::apply(
          [&b](auto &&...afs) {
            return call::build(std::forward<B>(b), *afs...);
          },
          at);
    }
  };
}
} // namespace
} // namespace impl

template <typename Tup, typename Builder,
          std::size_t... tIs>
constexpr auto build_tuple(Tup &&t, std::index_sequence<tIs...>,
                           Builder const &b) {
  return template_base_t<Tup>::raw_make(
      b(std::get<tIs>(std::forward<Tup>(t)))...);
}

template <typename TElementConstraint, typename... TD2,
          typename TTuple = std::tuple<std::unwrap_ref_decay_t<TD2>...>>
  requires((std::invocable<TElementConstraint, std::unwrap_ref_decay_t<TD2>> &&
            ...))
TTuple args_to_group(TElementConstraint, TD2 &&...d2) {
  return TTuple(std::forward<TD2>(d2)...);
}

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
template <typename... Ts, dooc::template_string... tags>
struct template_base<dooc::named_tuple<dooc::named_arg_t<tags, Ts>...>> {
  template <typename... Args> static constexpr auto decay_make(Args &&...args) {
    return dooc::named_tuple(std::forward<Args>(args)...);
  }
};

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


template <dooc::template_string tName, typename T>
constexpr dooc::named_arg_t<tName, T> forward_named_arg(T &&t) {
  return dooc::named_arg_t<tName, T>{std::forward<T>(t)};
}

template <typename Tup, typename Builder,
          dooc::template_string... tags>
constexpr auto build_tuple(Tup &&t, dooc::template_string_list_t<tags...>,
                           Builder const &b) {
  return dooc::named_tuple(
      forward_named_arg<tags>(b(get<tags>(std::forward<Tup>(t))))...);
}
#endif

template <typename Constraint, typename... Args> struct maybe_build_constraint {
  template <typename T>
    requires(std::invocable<Constraint, T &&> ||
             (builder<T, Args...> &&
              std::invocable<Constraint, build_result_t<T, Args...>>))
  constexpr void operator()(T &&) const {}
};

template <typename Constraint>
constexpr auto optional_build =
    []<typename T, typename... Args>(T &&e, Args &&...args) {
      if constexpr (!std::invocable<Constraint, T>) {
        static_assert(
            builder<T, Args...>,
            "Element did not satisfy constraint and it could not be built");
        static_assert(std::invocable<Constraint, build_result_t<T, Args...>>,
                      "Built element did not satisfy constraint");
        return call::build(std::forward<T>(e), std::forward<Args>(args)...);
      } else {
        unused(args...);
        return std::forward<T>(e);
      }
    };

template <typename Constraint,
          typename T,
          typename... TArgs>
constexpr auto build_group(Constraint &&, T &&g,
                           TArgs &&...args) {
  auto gf = bp::as_forward<T>(g);
  auto b_args = impl::optional_builder<Constraint, TArgs&&...>(std::forward<TArgs>(args)...);
  if constexpr (std::invocable<decltype(b_args), T>) {
    return b_args(*gf);
  } /* else if constexpr (std::invocable<decltype(call::apply_to), T,
                                       bp::no_op_t>) {
     return call::apply_to(
         *gf, [do_build = bp::trailing_curried<Builder, TArgs...>(
                   b, std::forward<TArgs>(args)...)]<typename... Ts>(
                  Ts &&...elements) {
           return std::tuple(do_build(std::forward<Ts>(elements))...);
         });
   }
   */
  else {
    if constexpr (std::ranges::range<std::remove_cvref_t<T>>) {
      return bp::transform_range(
          std::forward<T>(g),
          std::move(b_args));
    } else if constexpr (usable_in_build_tuples<T>) {
      return build_tuple(*gf, all_tuple_tags_t<T>{},
                                     std::move(b_args));
    } else {
      static_assert(std::is_void_v<T>, "Type does not satisfy constraint");
    }
  }
}

template <typename TConstraint, typename... TArgs>
using args_to_group_t =
    decltype(args_to_group(TConstraint{}, std::declval<TArgs &&>()...));

} // namespace cgui::build

#endif // COMPONENT_GUI_BUILD_UTILITY_HPP

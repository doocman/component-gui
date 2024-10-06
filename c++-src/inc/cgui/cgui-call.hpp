
#ifndef COMPONENT_GUI_CGUI_CALL_HPP
#define COMPONENT_GUI_CGUI_CALL_HPP

#include <concepts>
#include <utility>
#include <type_traits>

#define CGUI_CALL_CONCEPT(NAME)                                                \
  [[maybe_unused]] inline void NAME() {}                                       \
  template <typename T, typename... Ts>                                        \
  concept member_##NAME =                                                      \
      requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {              \
        (*t).NAME(*args...);                                                   \
      };                                                                       \
  template <typename T, typename... Ts>                                        \
  concept static_##NAME =                                                      \
      requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {              \
        std::remove_cvref_t<T>::NAME(*t, *args...);                            \
      };                                                                       \
  template <typename T, typename... Ts>                                        \
  concept extend_##NAME =                                                      \
      requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {              \
        extend_api_t<T>::NAME(*t, *args...);                                   \
      };                                                                       \
  template <typename T, typename... Ts>                                        \
  concept free_##NAME =                                                        \
      requires(bp::as_forward<T> t, bp::as_forward<Ts>... args) {              \
        NAME(*t, *args...);                                                    \
      };                                                                       \
  template <typename T, typename... Ts>                                        \
  concept has_##NAME = member_##NAME<T, Ts...> || static_##NAME<T, Ts...> ||   \
                       extend_##NAME<T, Ts...> || free_##NAME<T, Ts...>;       \
  struct _do_##NAME {                                                          \
                                                                               \
    template <typename... Ts, has_##NAME<Ts...> T>                             \
    static constexpr decltype(auto) call(T &&torg, Ts &&...args) {             \
      auto t = bp::as_forward<T>(torg);                                        \
      if constexpr (member_##NAME<T, Ts...>) {                                 \
        return (*t).NAME(std::forward<Ts>(args)...);                           \
      } else if constexpr (static_##NAME<T, Ts...>) {                          \
        return std::remove_cvref_t<T>::NAME(*t, std::forward<Ts>(args)...);    \
      } else if constexpr (extend_##NAME<T, Ts...>) {                          \
        return extend_api_t<T>::NAME(*t, std::forward<Ts>(args)...);           \
      } else {                                                                 \
        static_assert(free_##NAME<T, Ts...>);                                  \
        return NAME(*t, std::forward<Ts>(args)...);                            \
      }                                                                        \
    }                                                                          \
    template <typename... Ts, has_##NAME<Ts...> T>                             \
    constexpr decltype(auto) operator()(T && t, Ts &&...args) const {          \
      return call(std::forward<T>(t), std::forward<Ts>(args)...);              \
    }                                                                          \
  };

#define CGUI_CALL_BBOX_MEMBER(NAME, CONCEPT, MUTCONCEPT)                       \
  static constexpr decltype(auto) _fallback(auto const &b);                    \
  static constexpr decltype(auto) _fallback_mut(auto &&b, auto &&v);           \
  static constexpr decltype(auto) call(CONCEPT auto &&b) {                     \
    using fwd_t = decltype(b);                                                 \
    auto bf = bp::as_forward<fwd_t>(std::forward<fwd_t>(b));                   \
    if constexpr (has_##NAME<fwd_t>) {                                         \
      return _do_##NAME::call(*bf);                                            \
    } else {                                                                   \
      return _fallback(bf);                                                    \
    }                                                                          \
  }                                                                            \
  template <typename TVal, MUTCONCEPT<TVal> T>                                 \
  static constexpr decltype(auto) call(T &&b, TVal &&v) {                      \
    auto bf = bp::as_forward<T>(std::forward<T>(b));                           \
    auto vf = bp::as_forward<TVal>(std::forward<TVal>(v));                     \
    if constexpr (has_##NAME<T, TVal>) {                                       \
      return _do_##NAME::call(*bf, *vf);                                       \
    } else if constexpr (has_assignable_get<T, _do_##NAME, TVal>) {            \
      return _do_##NAME::call(*bf) = *vf;                                      \
    } else {                                                                   \
      return _fallback_mut(bf, vf);                                            \
    }                                                                          \
  }                                                                            \
  constexpr decltype(auto) operator()(auto &&b, auto &&...vs) const            \
    requires(requires() {                                                      \
      call(std::forward<decltype(b)>(b), std::forward<decltype(vs)>(vs)...);   \
    })                                                                         \
  {                                                                            \
    return call(std::forward<decltype(b)>(b),                                  \
                std::forward<decltype(vs)>(vs)...);                            \
  }

#define CGUI_EXTRA_PARAMS
#define CGUI_EXTRA_ARGS
#define CGUI_EXTRA_ARGS_COMMA
#define CGUI_PIX_FUNC_IMPL(NAME, CONCEPT)                                      \
  namespace ns_lookup {                                                        \
  inline void NAME() {}                                                        \
                                                                               \
  template <typename T>                                                        \
  concept static_##NAME = requires(T &&tc CGUI_EXTRA_PARAMS) {                 \
    {                                                                          \
      std::remove_cvref_t<T>::NAME(tc CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS)   \
    } -> CONCEPT;                                                              \
  };                                                                           \
  template <typename T>                                                        \
  concept member_##NAME = requires(T &&tc CGUI_EXTRA_PARAMS) {                 \
    { tc.NAME(CGUI_EXTRA_ARGS) } -> CONCEPT;                                   \
  };                                                                           \
  template <typename T>                                                        \
  concept free_##NAME = requires(T &&tc CGUI_EXTRA_PARAMS) {                   \
    { NAME(tc CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS) } -> CONCEPT;             \
  };                                                                           \
  template <typename TExt, typename T>                                         \
  concept extend_##NAME = requires(T &&tc CGUI_EXTRA_PARAMS) {                 \
    { TExt::NAME(tc CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS) } -> CONCEPT;       \
  };                                                                           \
  template <typename T>                                                        \
  concept has_##NAME = member_##NAME<T> || static_##NAME<T> ||                 \
                       free_##NAME<T> || extend_##NAME<extend_api_t<T>, T>;    \
  constexpr decltype(auto) do_##NAME(has_##NAME auto &&t CGUI_EXTRA_PARAMS) {  \
    using type = decltype(t);                                                  \
    if constexpr (member_##NAME<type>) {                                       \
      return std::forward<type>(t).NAME(CGUI_EXTRA_ARGS);                      \
    } else if constexpr (static_##NAME<type>) {                                \
      return std::remove_cvref_t<type>::NAME(                                  \
          std::forward<type>(t) CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS);        \
    } else if constexpr (extend_##NAME<extend_api_t<type>, type>) {            \
      return extend_api_t<type>::NAME(                                         \
          std::forward<type>(t) CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS);        \
    } else {                                                                   \
      static_assert(free_##NAME<type>);                                        \
      return NAME(std::forward<type>(t)                                        \
                      CGUI_EXTRA_ARGS_COMMA CGUI_EXTRA_ARGS);                  \
    }                                                                          \
  }                                                                            \
  }                                                                            \
  constexpr auto NAME(auto &&t)                                                \
      -> decltype(ns_lookup::do_##NAME(std::forward<decltype(t)>(t)))          \
    requires(                                                                  \
        requires() { ns_lookup::do_##NAME(std::forward<decltype(t)>(t)); })    \
  {                                                                            \
    return ns_lookup::do_##NAME(std::forward<decltype(t)>(t));                 \
  }

#define CGUI_PIX_FUNC_IMPL_GETSET(NAME, CONCEPT)                               \
  CGUI_PIX_FUNC_IMPL(NAME, CONCEPT)                                            \
  namespace ns_lookup {                                                        \
                                                                               \
  template <typename T, typename TVal>                                         \
  concept member_set_##NAME = requires(                                        \
      bp::as_forward<T> tc, bp::as_forward<TVal> v) { (*tc).NAME(*v); };       \
  template <typename T, typename TVal>                                         \
  concept static_set_##NAME =                                                  \
      requires(bp::as_forward<T> tc, bp::as_forward<TVal> v) {                 \
        std::remove_cvref_t<T>::NAME(*tc, *v);                                 \
      };                                                                       \
  template <typename T, typename TVal>                                         \
  concept free_set_##NAME = requires(                                          \
      bp::as_forward<T> tc, bp::as_forward<TVal> v) { NAME(*tc, *v); };        \
  template <typename TExt, typename T, typename TVal>                          \
  concept extend_set_##NAME = requires(                                        \
      bp::as_forward<T> tc, bp::as_forward<TVal> v) { TExt::NAME(*tc, *v); };  \
  template <typename T, typename TVal>                                         \
  concept mut_get_##NAME = has_##NAME<T> && requires(bp::as_forward<T> t) {    \
    { ns_lookup::do_##NAME(*t) } -> std::assignable_from<TVal>;                \
  };                                                                           \
  template <typename T, typename TV>                                           \
  concept has_set_##NAME =                                                     \
      member_set_##NAME<T, TV> || static_set_##NAME<T, TV> ||                  \
      free_set_##NAME<T, TV> || extend_set_##NAME<extend_api_t<T>, T, TV> ||   \
      mut_get_##NAME<T, TV>;                                                   \
                                                                               \
  template <typename TV, has_set_##NAME<TV> T>                                 \
  constexpr auto do_set_##NAME(T &&vorg, TV &&valorg) {                        \
    auto v = bp::as_forward<T>(std::forward<T>(vorg));                         \
    auto val = bp::as_forward<TV>(std::forward<TV>(valorg));                   \
    if constexpr (member_set_##NAME<T, TV>) {                                  \
      return (*v).NAME(*val);                                                  \
    } else if constexpr (static_set_##NAME<T, TV>) {                           \
      return std::remove_cvref_t<T>::NAME(*v, *val);                           \
    } else if constexpr (extend_set_##NAME<extend_api_t<T>, T, TV>) {          \
      return extend_api_t<T>::NAME(*v, *val);                                  \
    } else if constexpr (free_set_##NAME<T, TV>) {                             \
      return NAME(*v, *val);                                                   \
    } else {                                                                   \
      static_assert(mut_get_##NAME<T, TV>);                                    \
      return do_##NAME(*v) = *val;                                             \
    }                                                                          \
  }                                                                            \
  }                                                                            \
  constexpr auto NAME(auto &&t, auto &&v)                                      \
    requires(requires() {                                                      \
      ns_lookup::do_set_##NAME(std::forward<decltype(t)>(t),                   \
                               std::forward<decltype(v)>(v));                  \
    })                                                                         \
  {                                                                            \
    return ns_lookup::do_set_##NAME(std::forward<decltype(t)>(t),              \
                                    std::forward<decltype(v)>(v));             \
  }


namespace cgui {
template <typename> struct extend_api;
template <typename T> using extend_api_t = extend_api<std::remove_cvref_t<T>>;

namespace call {
namespace impl {
CGUI_CALL_CONCEPT(apply_to);
CGUI_CALL_CONCEPT(for_each);
CGUI_CALL_CONCEPT(build);

template <typename T>
concept is_tuple_like_hack =
    bp::has_tuple_size<T> && (std::tuple_size_v<std::remove_cvref_t<T>> == 0 ||
                              requires(T &&t) { std::get<0>(t); });

struct do_apply_to {
  template <typename T, typename TCB>
    requires(has_apply_to<T, TCB> ||
             (is_tuple_like_hack<T> &&
              requires(bp::as_forward<T> t, bp::as_forward<TCB> cb) {
                std::apply(*cb, *t);
              }))
  constexpr decltype(auto) operator()(T && t, TCB && cb) const {
    auto tf = bp::as_forward<T>(t);
    auto cf = bp::as_forward<TCB>(cb);
    if constexpr (has_apply_to<T, TCB>) {
      return _do_apply_to{}(*tf, *cf);
    } else {
      return std::apply(*cf, *tf);
    }
  }
};

struct do_for_each {
  template <typename T, typename TCB>
    requires(has_for_each<T, TCB> || std::ranges::input_range<T> ||
             requires(bp::as_forward<T> t) { do_apply_to{}(*t, bp::no_op); }
#if defined(CGUI_HAS_NAMED_ARGS)
             || dooc::named_tuple_like<T>
#endif
             )
  constexpr void operator()(T &&t, TCB &&cb) const {
    auto tf = bp::as_forward<T>(t);
    auto cbf = bp::as_forward<TCB>(cb);
    if constexpr (has_for_each<T, TCB>) {
      _do_for_each{}(*tf, *cbf);
    } else if constexpr (std::ranges::input_range<T>) {
      std::ranges::for_each(*tf, *cbf);
      // TODO: See if we can get rid of this ifdef...
#ifdef CGUI_HAS_NAMED_ARGS
    } else if constexpr (dooc::named_tuple_like<T>) {
      dooc::tuple_for_each(
          [&cb](auto &&, auto &&v) { cb(std::forward<decltype(v)>(v)); },
          std::forward<T>(t));
#endif
    } else {
      do_apply_to{}(*tf, [&cbf](auto &&...vals) {
        bp::run_for_each(*cbf, std::forward<decltype(vals)>(vals)...);
      });
    }
  }
};
} // namespace impl
inline constexpr impl::do_apply_to apply_to;
inline constexpr impl::do_for_each for_each;
inline constexpr impl::_do_build build;
} // namespace call
}

#undef CGUI_PIX_FUNC_IMPL
#undef CGUI_EXTRA_PARAMS
#undef CGUI_EXTRA_ARGS
#undef CGUI_EXTRA_ARGS_COMMA
#undef CGUI_CALL_CONCEPT
#undef CGUI_CALL_BBOX_MEMBER
#endif

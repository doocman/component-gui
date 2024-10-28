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
    if constexpr(sizeof...(vals) > 0) {
      using expander = char const[sizeof...(vals)];
      auto wrem = [&f]<typename... Us>(Us &&...args) {
        (void)f(std::forward<Us>(args)...);
        return char{};
      };
      unused(expander{wrem(bp::as_forward<Ts>(vs), is)...});
    } else {
      unused(f);
    }
  };
  cb_return(std::make_index_sequence<sizeof...(vals)>{},
            std::forward<decltype(vals)>(vals)...);
  // unused(expander{cb_return(std::forward<decltype(vals)>(vals))...});
}

} // namespace cgui::bp

#endif // COMPONENT_GUI_UTILITY_HPP

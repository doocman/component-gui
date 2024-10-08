//
// Created by rvons on 2024-07-22.
//

#ifndef COMPONENT_GUI_UTILITY_HPP
#define COMPONENT_GUI_UTILITY_HPP

#include <utility>

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

template <typename T, typename TArg>
constexpr auto forward_cast(TArg &&arg)
    -> decltype(bp::forward_like<TArg>(std::declval<T &>())) {
  return static_cast<decltype(bp::forward_like<TArg>(std::declval<T &>()))>(
      arg);
}

template <typename... Ts> struct empty_structs_optimiser;
template <> struct empty_structs_optimiser<> {
  static constexpr void get() {}
};
template <typename T, typename... Ts>
  requires(std::is_empty_v<T>)
struct empty_structs_optimiser<T, Ts...> : empty_structs_optimiser<Ts...> {
  using _base_t = empty_structs_optimiser<Ts...>;
  constexpr empty_structs_optimiser() noexcept(
      std::is_nothrow_default_constructible_v<T> &&
      std::is_nothrow_default_constructible_v<_base_t>) = default;
  template <typename TArg, typename... TArgs>
    requires(std::constructible_from<_base_t, TArgs && ...>)
  constexpr explicit empty_structs_optimiser(TArg &&, TArgs &&...to_base)
      : _base_t(std::forward<TArgs>(to_base)...) {}
  static constexpr T get(auto &&, std::type_identity<T>) { return T{}; }
  using _base_t::get;
};
template <typename T, typename... Ts>
  requires(!std::is_empty_v<T>)
struct empty_structs_optimiser<T, Ts...> : empty_structs_optimiser<Ts...> {
  using _base_t = empty_structs_optimiser<Ts...>;
  T val_;
  constexpr empty_structs_optimiser() noexcept(
      std::is_nothrow_default_constructible_v<T> &&
      std::is_nothrow_default_constructible_v<_base_t>) = default;
  template <typename TArg, typename... TArgs>
    requires(std::constructible_from<T, TArg &&> &&
             std::constructible_from<_base_t, TArgs && ...>)
  constexpr explicit empty_structs_optimiser(TArg &&arg, TArgs &&...to_base)
      : _base_t(std::forward<TArgs>(to_base)...),
        val_(std::forward<TArg>(arg)) {}
  static constexpr auto &&get(auto &&self, std::type_identity<T>) {
    using this_t = decltype(bp::forward_like<decltype(self)>(
        std::declval<empty_structs_optimiser &>()));
    return static_cast<this_t>(self).val_;
    // return bp::forward_cast<empty_structs_optimiser>(self).val_;
    // //bp::forward_like<decltype(self)>();
  }
  using _base_t::get;
};

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

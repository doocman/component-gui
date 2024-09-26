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

template <typename... Ts>
struct empty_structs_optimiser;
template <>
struct empty_structs_optimiser<> {
  static constexpr void get() {}
};
template <typename T, typename... Ts>
requires(std::is_empty_v<T>)
struct empty_structs_optimiser<T, Ts...> : empty_structs_optimiser<Ts...> {
  using _base_t = empty_structs_optimiser<Ts...>;
  static constexpr T get(auto&&, std::type_identity<T>) {
    return T{};
  }
  using _base_t::get;
};
template <typename T, typename... Ts>
requires(!std::is_empty_v<T>)
struct empty_structs_optimiser<T, Ts...> : empty_structs_optimiser<Ts...> {
  using _base_t = empty_structs_optimiser<Ts...>;
  T val_;
  static constexpr auto&& get(auto&& self, std::type_identity<T>) {
    return std::forward<decltype(self)>(self);
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

constexpr void run_for_each(auto&& cb, auto&&... vals) requires(std::invocable<decltype(cb), decltype(vals)> && ...) {
  auto cb_return = [&cb] <typename T> (T&& v) {
    cb(std::forward<T>(v));
    return 0;
  };
  unused((... + cb_return(std::forward<decltype(vals)>(vals))));
}

} // namespace cgui::bp

#endif // COMPONENT_GUI_UTILITY_HPP

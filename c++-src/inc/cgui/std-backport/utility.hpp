//
// Created by rvons on 2024-07-22.
//

#ifndef COMPONENT_GUI_UTILITY_HPP
#define COMPONENT_GUI_UTILITY_HPP

#include <utility>

namespace cgui::bp {

[[noreturn]] inline void unreachable() {
#if defined(_MSC_VER) && !defined(__clang__)
  __assume(false);
#elif defined(__GNUC__) || defined(__clang__)
  __builtin_unreachable();
#endif
}

template <typename T>
struct as_forward {
  T&& val_;

  constexpr explicit(false) as_forward(T&& v) : val_(std::forward<T>(v)) {}

  constexpr T&& value() const noexcept {
    return std::forward<T>(val_);
  }
  constexpr T&& operator*() const noexcept {
    return value();
  }
};

template <typename T>
class deferred {
  T val_;

public:
  constexpr explicit deferred(T in) : val_(std::move(in)) {}
  constexpr ~deferred() {
    val_();
  }
  deferred(deferred&&) = delete;
  deferred& operator=(deferred&&) = delete;
};

}

#endif // COMPONENT_GUI_UTILITY_HPP

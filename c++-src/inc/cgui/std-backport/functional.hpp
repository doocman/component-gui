//
// Created by rvons on 2024-09-21.
//

#ifndef COMPONENT_GUI_FUNCTIONAL_HPP
#define COMPONENT_GUI_FUNCTIONAL_HPP

#include <functional>

#include <cgui/std-backport/utility.hpp>

namespace cgui::bp {
struct no_op_t {
  template <typename T> using function = T;

  template <typename... Ts> static constexpr void call(Ts...) noexcept {}
  constexpr void operator()(auto &&...) const noexcept {}

  template <typename... Ts>
  constexpr explicit(false) operator function<void(Ts...)> *() const noexcept {
    return &call<Ts...>;
  }
};
template <typename T, T result_v = T{}> struct return_constant_t {
  template <typename U> using function = U;

  template <typename... Ts> static constexpr T call(Ts...) noexcept {
    return result_v;
  }
  constexpr T operator()(auto &&...) const noexcept { return result_v; }
  template <typename... Ts>
  constexpr explicit(false) operator function<T(Ts...)> *() const noexcept {
    return &call<Ts...>;
  }
};
template <bool result_v>
using pretend_predicate_t = return_constant_t<bool, result_v>;

inline constexpr no_op_t no_op;
template <typename T, T r>
inline constexpr return_constant_t<T, r> return_constant;
template <bool r> inline constexpr pretend_predicate_t<r> pretend_predicate;
inline constexpr auto false_predicate = pretend_predicate<false>;

template <typename TF, typename... TVals> class trailing_curried {
  TF f_;
  std::tuple<TVals...> values_;

public:
  template <typename TF2, typename... TArgs>
    requires(std::constructible_from<TF, TF2> &&
             (std::constructible_from<TVals, TArgs> && ...))
  constexpr explicit trailing_curried(TF2 &&f, TArgs &&...args)
      : f_(std::forward<TF2>(f)), values_(std::forward<TArgs>(args)...) {}

  constexpr decltype(auto) operator()(auto &&...args) const {
    return std::apply(
        [this, &args...](auto &&...vals) -> decltype(auto) {
          return f_(std::forward<decltype(args)>(args)...,
                    std::forward<decltype(vals)>(vals)...);
        },
        values_);
  }
};

template <typename TF, typename... TArgs>
trailing_curried(TF &&, TArgs &&...)
    -> trailing_curried<std::unwrap_ref_decay_t<TF>,
                        std::unwrap_ref_decay_t<TArgs>...>;

template <typename T, typename... Ts>
class invoke_if_applicable : empty_structs_optimiser<T> {
  using base_t = empty_structs_optimiser<T>;
  static constexpr decltype(auto) get_f(auto &&self) {
    using self_t = decltype(self);
    if constexpr (std::is_const_v<std::remove_reference_t<self_t>>) {
      return bp::forward_like<self_t>(static_cast<base_t const &>(self));
    } else {
      return bp::forward_like<self_t>(static_cast<base_t &>(self));
    }
  }
  std::tuple<Ts...> args_;

public:
  constexpr explicit invoke_if_applicable(T t, Ts &&...args)
      : base_t(std::move(t)), args_(std::forward<Ts>(args)...) {}

  // disable copy/move semantics, because this class should not be "saved".
  invoke_if_applicable(invoke_if_applicable const &) = delete;
  invoke_if_applicable &operator=(invoke_if_applicable const &) = delete;

  constexpr void operator()(auto &&cur_sub, auto &&...subjects) && {
    if constexpr (std::invocable<T, decltype(cur_sub),
                                 std::remove_reference_t<Ts> &...>) {
      std::apply(
          [this, &cur_sub](auto &&...args) {
            std::invoke(get_f(std::move(*this)),
                        std::forward<decltype(cur_sub)>(cur_sub), args...);
          },
          args_);
    }
    if constexpr (sizeof...(subjects) > 0) {
      std::move (*this)(std::forward<decltype(subjects)>(subjects)...);
    }
  }
};
} // namespace cgui::bp

#endif // COMPONENT_GUI_FUNCTIONAL_HPP

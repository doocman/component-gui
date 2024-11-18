//
// Created by rvons on 2024-09-21.
//

#ifndef COMPONENT_GUI_FUNCTIONAL_HPP
#define COMPONENT_GUI_FUNCTIONAL_HPP

#include <functional>
#include <variant>

#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/tuple.hpp>
#include <cgui/std-backport/utility.hpp>

namespace cgui::bp {

/// Function-like object that does nothing.
struct no_op_t {
  template <typename T> using function = T;

  /// Static no-op function used to return a function pointer.
  /// \tparam Ts Types of the arguments
  /// \param ... ignored arguments
  /// \return nothing
  template <typename... Ts> static constexpr void call(Ts...) noexcept {}

  /// No-op call operator.
  /// \return nothing.
  constexpr void operator()(auto &&...) const noexcept {}

  /// Implicit to-function-pointer conversion operator
  /// \tparam Ts Type of the arguments
  /// \return function pointer to no op function.
  template <typename... Ts>
  constexpr explicit(false) operator function<void(Ts...)> *() const noexcept {
    return &call<Ts...>;
  }
};
/// Function-like object that always returns result_v
/// \tparam T Type of result
/// \tparam result_v result to return.
template <typename T, T result_v = T{}> struct return_constant_t {
  template <typename U> using function = U;

  /// Static function to be used in the function pointer conversion operator.
  /// \tparam Ts Type of arguments.
  /// \param ... Ignored arguments
  /// \return result_v
  template <typename... Ts> static constexpr T call(Ts...) noexcept {
    return result_v;
  }

  /// Calling operator
  /// \return result_v
  constexpr T operator()(auto &&...) const noexcept { return result_v; }

  /// Implicit conversion to function pointer
  /// \tparam Ts Argument types.
  /// \return result_v
  template <typename... Ts>
  constexpr explicit(false) operator function<T(Ts...)> *() const noexcept {
    return &call<Ts...>;
  }
};

/// Function-like object that throws a 'bad_function_call' exception on
/// execution.
struct throw_bad_call_f_t {
  template <typename T> using function = T;

  /// Static call function that throws a 'bad_function_call'.
  ///
  /// \tparam R Return type
  /// \tparam Args Argument types
  /// \param ...
  /// \return always throws.
  template <typename R = void, typename... Args> static R call(Args...) {
    throw std::bad_function_call();
  }

  /// Member call operator function that throws a 'bad_function_call'.
  ///
  /// \tparam R Return type
  /// \param ...
  /// \return always throws.
  template <typename R = void> R operator()(auto &&...) const {
    return call<R>();
  }

  /// Implicit to-function-pointer conversion operator. Note: this function
  /// itself is noexcept, even though the pointer returned is not.
  /// \tparam Ts Type of the arguments
  /// \return function pointer to bad call throwing function.
  template <typename R, typename... Ts>
  constexpr explicit(false) operator function<R(Ts...)> *() const noexcept {
    return &call<R, Ts...>;
  }
};

/// Predicate that always returns result_v.
template <bool result_v>
using pretend_predicate_t = return_constant_t<bool, result_v>;

inline constexpr no_op_t no_op;
inline constexpr throw_bad_call_f_t throw_bad_call_f;
template <typename T, T r>
inline constexpr return_constant_t<T, r> return_constant;
template <bool r> inline constexpr pretend_predicate_t<r> pretend_predicate;

/// Predicate that always returns false.
inline constexpr auto false_predicate = pretend_predicate<false>;

/// "Curries" a function-like object by adding arguments to the end of the
/// argument list.
/// \tparam TF Function-like object.
/// \tparam TVals Arguments to append.
template <typename TF, typename... TVals> class trailing_curried {
  TF f_;
  std::tuple<TVals...> values_;

public:
  /// Constructor.
  /// \tparam TF2 Function initiation argument.
  /// \tparam TArgs Trailing arguments.
  /// \param f Function initiation argument.
  /// \param args Trailing arguments.
  template <typename TF2, typename... TArgs>
    requires(std::constructible_from<TF, TF2> &&
             (std::constructible_from<TVals, TArgs> && ...))
  constexpr explicit trailing_curried(TF2 &&f, TArgs &&...args)
      : f_(std::forward<TF2>(f)), values_(std::forward<TArgs>(args)...) {}

  /// Calls the underlying function with args and then TVals.
  /// \param args First args to function
  /// \return result of function TF
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

} // namespace cgui::bp

#endif // COMPONENT_GUI_FUNCTIONAL_HPP

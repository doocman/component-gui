//
// Created by rvons on 2024-07-22.
//

#ifndef COMPONENT_GUI_UTILITY_HPP
#define COMPONENT_GUI_UTILITY_HPP

#include <utility>

#include <cgui/std-backport/concepts.hpp>
#include <cgui/warnings.hpp>

namespace cgui::bp {

/// Same as std::unreachable
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
/// Same as std::forward_like
/// \tparam T Source to copy qualifiers from
/// \tparam U Type to apply qualifiers to
/// \param x argument to forward
/// \return forwarded x.
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
      return std::move(x); // NOLINT(*-move-forwarding-reference)
  }
}
#endif
/// Type-alias to copy const-volatile reference qualifiers from TToCopy to T.
/// @tparam T type to apply qualifiers to
/// @tparam TToCopy type to copy qualifiers from.
template <typename T, typename TToCopy>
using copy_cvref_t = decltype(bp::forward_like<TToCopy>(std::declval<T>()));

template <std::size_t tI>
using index_constant = std::integral_constant<std::size_t, tI>;

/// Convenience function that can be used to automatically forward a value.
/// Note, however, that the *-operator or value() should only ever be used once,
/// as it could potentially trigger a move. Consider using 'as_ref' or 'as_cref'
/// if you need to access the variable multiple times.
/// \tparam T type to forward.
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

/// Constraint that requires a function-like object to either support F(T1) or
/// F(T1, T2).
/// \tparam F function-like object
/// \tparam T1 Mandatory argument
/// \tparam T2 Optional argument
template <typename F, typename T1, typename T2>
concept invocable_arg1_or_arg1_2 =
    std::invocable<F, T1> || std::invocable<F, T1, T2>;

/// Invokes F with either T1 or T1 and T2 depending on which one F allows. It is
/// a convenience function as to allow extending users to simplify their API:s.
/// If both signatures are valid, the one containing both arguments will be
/// prioritised.
/// \tparam F Function-like object
/// \tparam T1 Mandatory argument
/// \tparam T2 Optional argument
/// \param cb Function-like object
/// \param arg1 Mandatory argument
/// \param arg2 Optional argument
/// \return result of calling cb.
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

/// Defers callable T to be called at the end-of-life of any instance of this
/// object.
/// \tparam T function-like object to be invoked at the end of life of the
/// instance.
template <typename T> class deferred {
  T val_;

public:
  constexpr explicit deferred(T in) : val_(std::move(in)) {}
  constexpr ~deferred() { val_(); }
  deferred(deferred &&) = delete;
  deferred &operator=(deferred &&) = delete;
};

/// Invokes cb with each argument vals with an optional std::size_t which tells
/// the index of the relevant argument.
/// \param cb function-like object that will be called once for each of vals.
/// \param vals all values to be used with cb.
/// \return nothing
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
    if constexpr (sizeof...(vals) > 0) {
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
}

} // namespace cgui::bp

#endif // COMPONENT_GUI_UTILITY_HPP

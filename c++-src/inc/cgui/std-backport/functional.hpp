//
// Created by rvons on 2024-09-21.
//

#ifndef COMPONENT_GUI_FUNCTIONAL_HPP
#define COMPONENT_GUI_FUNCTIONAL_HPP

#include <algorithm>
#include <cstring>
#include <functional>
#include <type_traits>
#include <variant>

#include <cgui/std-backport/concepts.hpp>
#include <cgui/std-backport/math.hpp>
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
struct terminate_f_t {
  template <typename T> using function = T;

  /// Static call function that throws a 'bad_function_call'.
  ///
  /// \tparam R Return type
  /// \tparam Args Argument types
  /// \param ...
  /// \return always throws.
  template <typename R = void, typename... Args> static R call(Args...) {
    std::terminate();
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
inline constexpr terminate_f_t terminate_f;
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

/// std::function replacement type that only accepts trivially
/// copyable+destructible objects and stores them unconditionally on the stack.
/// \tparam sz Size of stack-aligned data.
/// \tparam al
template <typename, std::size_t sz = sizeof(void *),
          std::size_t al = alignof(void *)>
class trivial_function;

// Dummy bool to help choosing the correct constructor for trivial_function:s.
template <typename> constexpr bool _is_trivial_function = false;
template <typename T, std::size_t sz, std::size_t al>
constexpr bool _is_trivial_function<trivial_function<T, sz, al>> = true;

/// Actual implementation of trivial function.
/// \tparam R return value type of the function.
/// \tparam Args arguments to pass into the function.
/// \tparam sz Minimum size (actual size may be increases due to alignment).
/// \tparam align Minimum alignment (alignment will at least be that of a
/// function pointer).
template <typename R, typename... Args, std::size_t sz, std::size_t align>
  requires(is_power_of_2(align))
class trivial_function<R(Args...), sz, align> {
  using _dummy_f_t = std::add_pointer_t<R(Args...)>;

  static consteval auto actual_align() {
    return std::max(alignof(_dummy_f_t), align);
  }
  static consteval auto actual_size() {
    auto align_multiple = (sz + actual_align() - 1) / actual_align();
    return align_multiple * actual_align();
  }
  using buffer_t = unsigned char[sz];
  using f_type = std::add_pointer_t<R(buffer_t &, Args...)>;
  static_assert(alignof(_dummy_f_t) == alignof(f_type),
                "What kind of a platform are you on to fail this assert??? "
                "Please report to component-gui Github repository");

  f_type f_ = static_cast<f_type>(terminate_f);
  alignas(align) buffer_t mutable data_{};

  template <typename T2, std::size_t sz2, std::size_t al2>
  friend class trivial_function;

  template <typename TRaw> static consteval bool _eligible_for_construction() {
    // clang appears to need this branch as it fails to substitute the
    // expression when checking move/copy semantics otherwise.
    if constexpr (!_is_trivial_function<TRaw>) {
      return alignof(TRaw) <= actual_align() && sizeof(TRaw) <= actual_size() &&
             std::is_trivially_copy_constructible_v<TRaw> &&
             std::is_trivially_destructible_v<TRaw> &&
             std::invocable<TRaw &, Args &&...>;
    } else {
      return false;
    }
  }

  constexpr void _ct_data() {
    if (std::is_constant_evaluated()) {
      std::ranges::fill(data_, char{});
    }
  }

public:
  template <typename T, typename TRaw = std::remove_cvref_t<T>>
    requires(_eligible_for_construction<TRaw>())
  constexpr explicit(false) trivial_function(T &&t) noexcept
      : f_([](buffer_t &b, Args... args) {
          return std::invoke(reinterpret_cast<TRaw &>(b),
                             std::forward<Args>(args)...);
        }) {
    _ct_data();
    new (&data_) TRaw(std::forward<T>(t));
  }
  template <typename T, typename TRaw = std::remove_cvref_t<T>>
    requires(_eligible_for_construction<TRaw>())
  constexpr trivial_function &operator=(T &&t) noexcept {
    f_ = [](buffer_t &b, Args... args) {
      return std::invoke(reinterpret_cast<TRaw &>(b),
                         std::forward<Args>(args)...);
    };
    new (&data_) TRaw(std::forward<T>(t));
    return *this;
  }

  constexpr trivial_function() noexcept = default;
  template <std::size_t sz2, std::size_t al2>
    requires(sz2 <= sz && al2 <= align && (sz2 != sz || al2 != align))
  constexpr explicit(false) trivial_function(
      trivial_function<R(Args...), sz2, al2> const &f2) noexcept
      : f_(f2.f_) {
    if (std::is_constant_evaluated()) {
      auto res = std::ranges::copy(f2.data_, std::begin(data_));
      // Necessary to satisfy constexpr.
      std::ranges::fill(res.out, std::ranges::end(data_), char{});
    } else {
      std::memcpy(data_, f2.data_, f2.actual_size());
    }
  }

  template <std::convertible_to<Args>... Ts>
  constexpr R operator()(Ts &&...args) const {
    return f_(data_, std::forward<Ts>(args)...);
  }

  constexpr void swap(trivial_function &other) noexcept {
    std::ranges::swap(other.data_, data_);
    std::swap(other.f_, f_);
  }

  constexpr explicit operator bool() const noexcept {
    return f_ != static_cast<f_type>(terminate_f);
  }
};

} // namespace cgui::bp

#endif // COMPONENT_GUI_FUNCTIONAL_HPP

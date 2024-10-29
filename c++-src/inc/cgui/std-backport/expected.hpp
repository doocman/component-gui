/// @file
/// This file implements std::expected for C++ pre-23.
///


#ifndef CGUI_CGUI_STD_BACKPORT_EXPECTED
#define CGUI_CGUI_STD_BACKPORT_EXPECTED

#include <cassert>
#include <exception>
#include <functional>
#include <initializer_list>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

#include <cgui/warnings.hpp>

namespace cgui::bp {

struct unexpect_t {};
inline constexpr unexpect_t unexpect;

inline constexpr bool _is_noexcept_exception_what =
    noexcept(std::exception{}.what());

template <typename T> class bad_expected_access;
template <> class bad_expected_access<void> : public std::exception {
protected:
  bad_expected_access() = default;
  bad_expected_access(bad_expected_access const &) = default;
  bad_expected_access(bad_expected_access &&) noexcept = default;
  bad_expected_access &operator=(bad_expected_access const &) = default;
  bad_expected_access &operator=(bad_expected_access &&) noexcept = default;
  ~bad_expected_access() override = default;

public:
  [[nodiscard]] char const *what() const
      noexcept(_is_noexcept_exception_what) override {
    return "Bad expected access";
  }
};
template <typename T>
class bad_expected_access : public bad_expected_access<void> {
  T e_;

public:
  explicit bad_expected_access(T e) : e_(e) {}
  T &error() & noexcept { return e_; }
  T const &error() const & noexcept { return e_; }
  T &&error() && noexcept { return std::move(e_); }
  T const &&error() const && noexcept { return std::move(e_); }

  [[nodiscard]] char const *what() const
      noexcept(_is_noexcept_exception_what) override {
    if constexpr (std::convertible_to<T, char const *>) {
      return static_cast<char const *>(e_);
    } else if constexpr (requires() {
                           { e_.c_str() } -> std::convertible_to<char const *>;
                         }) {
      return e_.c_str();
    } else {
      return bad_expected_access<void>::what();
    }
  }
};

template <typename TErr> class unexpected {
  TErr err_;

public:
  constexpr unexpected(unexpected const &) = default;
  constexpr unexpected(unexpected &&) noexcept(
      std::is_nothrow_move_constructible_v<TErr>) = default;
  template <typename TE = TErr>
  constexpr explicit unexpected(TE &&e) : err_(std::forward<TE>(e)) {}
  template <typename... Ts>
  constexpr explicit unexpected(std::in_place_t, Ts &&...args)
      : err_(std::forward<Ts>(args)...) {}
  template <typename TU, typename... Ts>
  constexpr explicit unexpected(std::in_place_t, std::initializer_list<TU> il,
                                Ts &&...args)
      : err_(il, std::forward<Ts>(args)...) {}

  constexpr TErr &error() & noexcept { return err_; }
  constexpr TErr &&error() && noexcept { return std::move(err_); }
  constexpr TErr const &error() const & noexcept { return err_; }
  constexpr TErr const &&error() const && noexcept { return std::move(err_); }
  constexpr void
  swap(unexpected<TErr> &r) noexcept(std::is_nothrow_swappable_v<TErr>) {
    std::swap(error(), r.error());
  }
};

template <typename T>
constexpr void swap(unexpected<T> &l,
                    unexpected<T> &r) noexcept(std::is_nothrow_swappable_v<T>) {
  l.swap(r);
}
template <typename T1, typename T2>
  requires(std::equality_comparable_with<T1, T2>)
constexpr bool operator==(unexpected<T1> const &l, unexpected<T2> const &r) {
  return l.error() == r.error();
}
template <typename E> unexpected(E) -> unexpected<E>;

namespace details {
template <typename T, typename TErr> struct expected_member {
  std::variant<T, TErr> data_;
  constexpr expected_member() = default;
  constexpr explicit expected_member(std::in_place_t, auto &&...vs)
      : data_(T(std::forward<decltype(vs)>(vs)...)) {}
  constexpr explicit expected_member(unexpect_t, auto &&...vs)
      : data_(TErr(std::forward<decltype(vs)>(vs)...)) {}
  template <typename TI, typename... Ts>
  constexpr explicit expected_member(std::in_place_t,
                                     std::initializer_list<TI> il, Ts &&...vs)
      : data_(T(il, std::forward<decltype(vs)>(vs)...)) {}
  template <typename TI, typename... Ts>
  constexpr explicit expected_member(unexpect_t, std::initializer_list<TI> il,
                                     Ts &&...vs)
      : data_(TErr(il, std::forward<decltype(vs)>(vs)...)) {}

  [[nodiscard]] constexpr bool has_value() const noexcept {
    return std::holds_alternative<T>(data_);
  }
  static constexpr auto &&exp(auto &&v) noexcept {
    assert(v.has_value());
    return std::get<0>(std::forward<decltype(v)>(v).data_);
  }
  static constexpr auto &&err(auto &&v) noexcept {
    assert(!v.has_value());
    return std::get<1>(std::forward<decltype(v)>(v).data_);
  }
  constexpr void
  swap(expected_member &r) noexcept(std::is_nothrow_swappable_v<T>) {
    std::swap(data_, r.data_);
  }
};

template <typename T> struct expected_member<T, T> {
  T data_{};
  bool has_value_{true};
  constexpr expected_member() = default;
  constexpr explicit expected_member(std::in_place_t, auto &&...vs)
      : data_(std::forward<decltype(vs)>(vs)...), has_value_(true) {}
  constexpr explicit expected_member(unexpect_t, auto &&...vs)
      : data_(std::forward<decltype(vs)>(vs)...), has_value_(false) {}

  template <typename TI, typename... Ts>
  constexpr explicit expected_member(std::in_place_t,
                                     std::initializer_list<TI> il, Ts &&...vs)
      : data_(il, std::forward<decltype(vs)>(vs)...), has_value_(true) {}
  template <typename TI, typename... Ts>
  constexpr explicit expected_member(unexpect_t, std::initializer_list<TI> il,
                                     Ts &&...vs)
      : data_(il, std::forward<decltype(vs)>(vs)...), has_value_(false) {}

  [[nodiscard]] constexpr bool has_value() const noexcept { return has_value_; }
  static constexpr auto &&exp(auto &&v) noexcept {
    assert(v.has_value());
    return std::forward<decltype(v)>(v).data_;
  }
  static constexpr auto &&err(auto &&v) noexcept {
    assert(!v.has_value());
    return std::forward<decltype(v)>(v).data_;
  }

  constexpr void
  swap(expected_member &r) noexcept(std::is_nothrow_swappable_v<T>) {
    std::swap(data_, r.data_);
    std::swap(has_value_, r.has_value_);
  }
};
template <typename T> struct expected_member<void, T> {
  std::optional<T> data_{};
  constexpr expected_member() = default;
  constexpr explicit expected_member(std::in_place_t) {}
  constexpr explicit expected_member(unexpect_t, auto &&...vs)
      : data_(std::in_place, std::forward<decltype(vs)>(vs)...) {}

  template <typename TI, typename... Ts>
  constexpr explicit expected_member(unexpect_t, std::initializer_list<TI> il,
                                     Ts &&...vs)
      : data_(std::in_place, il, std::forward<decltype(vs)>(vs)...) {}

  [[nodiscard]] constexpr bool has_value() const noexcept {
    return !data_.has_value();
  }
  static constexpr void exp(auto &&v) noexcept {
    assert(v.has_value());
    unused(v);
  }
  static constexpr auto &&err(auto &&v) noexcept {
    assert(!v.has_value());
    return *std::forward<decltype(v)>(v).data_;
  }

  constexpr void
  swap(expected_member &r) noexcept(std::is_nothrow_swappable_v<T>) {
    std::swap(data_, r.data_);
  }
};
} // namespace details

template <typename TExp, typename TErr> class expected {
  using member_t = details::expected_member<TExp, TErr>;
  member_t data_;

  static constexpr member_t copy_expected(auto &&exp) {
    if (exp.has_value()) {
      return member_t(std::in_place, *std::forward<decltype(exp)>(exp));
    } else {
      return member_t(unexpect, std::forward<decltype(exp)>(exp).error());
    }
  }

  constexpr void throw_without_value() const {
    if (!has_value()) {
      throw bad_expected_access<std::decay_t<TErr>>(member_t::err(data_));
    }
  }

  static constexpr auto invoke_on_self(auto &&exp, auto &&f) {
    if constexpr (std::is_void_v<TExp>) {
      return std::invoke(std::forward<decltype(f)>(f));
    } else {
      return std::invoke(std::forward<decltype(f)>(f),
                         *std::forward<decltype(exp)>(exp));
    }
  }

  template <typename TE, typename TF>
  using invoke_result_exp =
      decltype(invoke_on_self(std::declval<TE &&>(), std::declval<TF &&>()));
  template <typename TE, typename TF>
  using invoke_result_err = decltype(invoke_on_self(
      std::declval<TE &&>().error(), std::declval<TF &&>()));

  template <typename TE, typename TF,
            typename TU = std::remove_cvref_t<invoke_result_exp<TE, TF>>>
  static constexpr auto do_and_then(TE &&exp, TF &&f) -> TU {
    if (exp.has_value()) {
      return invoke_on_self(std::forward<TE>(exp), std::forward<TF>(f));
    } else {
      return TU(unexpect, std::forward<TE>(exp).error());
    }
  }
  template <typename TE, typename TF,
            typename TU = std::remove_cvref_t<invoke_result_err<TE, TF>>>
  static constexpr auto do_or_else(TE &&exp, TF &&f) -> TU {
    if (!exp.has_value()) {
      return std::invoke(std::forward<TF>(f), std::forward<TE>(exp).error());
    } else {
      if constexpr (std::is_void_v<TExp>) {
        return TU();
      } else {
        return TU(std::in_place, *std::forward<TE>(exp));
      }
    }
  }
  template <typename TE, typename TF,
            typename TU =
                expected<std::remove_cvref_t<invoke_result_exp<TE, TF>>, TErr>>
  static constexpr auto do_transform(TE &&exp, TF &&f) -> TU {
    if (exp.has_value()) {
      if constexpr (std::is_void_v<TExp>) {
        invoke_on_self(std::forward<TE>(exp), std::forward<TF>(f));
        return TU();
      } else {
        return TU(std::in_place,
                  invoke_on_self(std::forward<TE>(exp), std::forward<TF>(f)));
      }
    } else {
      return TU(unexpect, std::forward<TE>(exp).error());
    }
  }
  template <typename TE, typename TF,
            typename TU =
                expected<TExp, std::remove_cvref_t<invoke_result_err<TE, TF>>>>
  static constexpr auto do_transform_error(TE &&exp, TF &&f) -> TU {
    if (!exp.has_value()) {
      return TU(unexpect, std::invoke(std::forward<TF>(f),
                                      std::forward<TE>(exp).error()));
    } else {
      if constexpr (std::is_void_v<TExp>) {
        return TU();
      } else {
        return TU(std::in_place, *std::forward<TE>(exp));
      }
    }
  }

public:
  constexpr expected() = default;
  constexpr expected(expected const &) = default;
  constexpr expected(expected &&) noexcept(
      std::is_nothrow_move_constructible_v<TExp> &&
      std::is_nothrow_move_constructible_v<TErr>) = default;

  template <typename TV2, typename TE2>
  constexpr explicit(!std::is_convertible_v<TV2 const &, TExp> ||
                     !std::is_convertible_v<TE2 const &, TErr>)
      expected(expected<TV2, TE2> const &other)
      : data_(copy_expected(other)) {}

  template <typename TV2, typename TE2>
  constexpr explicit(!std::is_convertible_v<TV2, TExp> ||
                     !std::is_convertible_v<TE2, TErr>)
      expected(expected<TV2, TE2> &&other)
      : data_(copy_expected(std::move(other))) {}

  template <typename TV2>
  constexpr explicit(!std::is_convertible_v<TV2, TExp> &&
                     !std::is_same_v<TExp, std::remove_cvref_t<TV2>>)
      expected(TV2 &&v)
      : data_(std::in_place, std::forward<TV2>(v)) {}

  template <typename TE2>
  constexpr explicit(!std::is_convertible_v<TE2 const &, TErr>)
      expected(unexpected<TE2> const &v)
      : data_(unexpect, v.error()) {}
  template <typename TE2>
  constexpr explicit(!std::is_convertible_v<TE2, TErr>)
      expected(unexpected<TE2> &&v)
      : data_(unexpect, std::move(v.error())) {}

  template <typename... Ts>
  constexpr explicit expected(std::in_place_t, Ts &&...args)
      : data_(std::in_place, std::forward<Ts>(args)...) {}
  template <typename TU, typename... Ts>
  constexpr explicit expected(std::in_place_t, std::initializer_list<TU> il,
                              Ts &&...args)
      : data_(std::in_place, il, std::forward<Ts>(args)...) {}
  template <typename... Ts>
  constexpr explicit expected(unexpect_t, Ts &&...args)
      : data_(unexpect, std::forward<Ts>(args)...) {}
  template <typename TU, typename... Ts>
  constexpr explicit expected(unexpect_t, std::initializer_list<TU> il,
                              Ts &&...args)
      : data_(unexpect, il, std::forward<Ts>(args)...) {}

  constexpr expected &operator=(expected const &) = default;
  constexpr expected &operator=(expected &&) noexcept(
      std::is_nothrow_move_assignable_v<TExp> &&
      std::is_nothrow_move_assignable_v<TErr>) = default;
  template <typename T = TExp> constexpr expected &operator=(T &&v) {
    data_ = member_t(std::in_place, std::forward<T>(v));
    return *this;
  }
  template <typename T> constexpr expected &operator=(unexpected<T> const &v) {
    data_ = member_t(unexpect, v.error());
    return *this;
  }
  template <typename T> constexpr expected &operator=(unexpected<T> &&v) {
    data_ = member_t(unexpect, std::move(v).error());
    return *this;
  }

  [[nodiscard]] constexpr bool has_value() const noexcept {
    return data_.has_value();
  }
  [[nodiscard]] constexpr explicit operator bool() const noexcept {
    return has_value();
  }
  template <typename TRet = TExp> [[nodiscard]] constexpr TRet &operator*() & {
    return member_t::exp(data_);
  }
  template <typename TRet = TExp>
  [[nodiscard]] constexpr TRet &&operator*() && {
    return member_t::exp(std::move(data_));
  }
  template <typename TRet = TExp>
  [[nodiscard]] constexpr TRet const &operator*() const & {
    return member_t::exp(data_);
  }
  template <typename TRet = TExp>
  [[nodiscard]] constexpr TRet const &&operator*() const && {
    return member_t::exp(std::move(data_));
  }
  template <typename TRet = TExp> [[nodiscard]] constexpr TRet *operator->() {
    return &member_t::exp(data_);
  }
  template <typename TRet = TExp>
  [[nodiscard]] constexpr TRet const *operator->() const {
    return &member_t::exp(data_);
  }

  template <typename TRet = TExp> [[nodiscard]] constexpr TRet &value() & {
    throw_without_value();
    return member_t::exp(data_);
  }
  template <typename TRet = TExp> [[nodiscard]] constexpr TRet &&value() && {
    throw_without_value();
    return member_t::exp(std::move(data_));
  }

  template <typename TRet = TExp>
  [[nodiscard]] constexpr TRet const &value() const & {
    throw_without_value();
    return member_t::exp(data_);
  }

  template <typename TRet = TExp>
  [[nodiscard]] constexpr TRet const &&value() const && {
    throw_without_value();
    return member_t::exp(std::move(data_));
  }

  [[nodiscard]] constexpr TErr &error() & { return member_t::err(data_); }
  [[nodiscard]] constexpr TErr &&error() && {
    return member_t::err(std::move(data_));
  }
  [[nodiscard]] constexpr TErr const &error() const & {
    return member_t::err(data_);
  }
  [[nodiscard]] constexpr TErr const &&error() const && {
    return member_t::err(std::move(data_));
  }

  template <typename T = TExp>
    requires(!std::is_void_v<TExp>)
  [[nodiscard]] constexpr TExp value_or(T &&v) && {
    if (has_value()) {
      return std::move(value());
    } else {
      return TExp(std::forward<decltype(v)>(v));
    }
  }
  template <typename T = TExp>
    requires(!std::is_void_v<TExp>)
  [[nodiscard]] constexpr TExp value_or(T &&v) const & {
    if (has_value()) {
      return value();
    } else {
      return TExp(std::forward<decltype(v)>(v));
    }
  }

  template <typename T = TErr> [[nodiscard]] constexpr TErr error_or(T &&v) && {
    if (!has_value()) {
      return std::move(error());
    } else {
      return TErr(std::forward<decltype(v)>(v));
    }
  }
  template <typename T = TErr>
  [[nodiscard]] constexpr TErr error_or(T &&v) const & {
    if (!has_value()) {
      return error();
    } else {
      return TErr(std::forward<decltype(v)>(v));
    }
  }
  constexpr auto and_then(auto &&f) && {
    return do_and_then(std::move(*this), std::forward<decltype(f)>(f));
  }
  constexpr auto and_then(auto &&f) const && {
    return do_and_then(std::move(*this), std::forward<decltype(f)>(f));
  }
  constexpr auto and_then(auto &&f) & {
    return do_and_then(*this, std::forward<decltype(f)>(f));
  }
  constexpr auto and_then(auto &&f) const & {
    return do_and_then(*this, std::forward<decltype(f)>(f));
  }

  constexpr auto transform(auto &&f) && {
    return do_transform(std::move(*this), std::forward<decltype(f)>(f));
  }
  constexpr auto transform(auto &&f) const && {
    return do_transform(std::move(*this), std::forward<decltype(f)>(f));
  }
  constexpr auto transform(auto &&f) & {
    return do_transform(*this, std::forward<decltype(f)>(f));
  }
  constexpr auto transform(auto &&f) const & {
    return do_transform(*this, std::forward<decltype(f)>(f));
  }

  constexpr auto or_else(auto &&f) && {
    return do_or_else(std::move(*this), std::forward<decltype(f)>(f));
  }
  constexpr auto or_else(auto &&f) const && {
    return do_or_else(std::move(*this), std::forward<decltype(f)>(f));
  }
  constexpr auto or_else(auto &&f) & {
    return do_or_else(*this, std::forward<decltype(f)>(f));
  }
  constexpr auto or_else(auto &&f) const & {
    return do_or_else(*this, std::forward<decltype(f)>(f));
  }

  template <typename... Ts, typename TRet = TExp>
    requires(std::is_nothrow_constructible_v<TExp, Ts...>)
  constexpr TRet &emplace(auto &&...args) {
    data_ = member_t(std::in_place, std::forward<decltype(args)>(args)...);
    return member_t::exp(data_);
  }
  template <typename TI, typename... Ts, typename TRet = TExp>
    requires(
        std::is_nothrow_constructible_v<TExp, std::initializer_list<TI>, Ts...>)
  constexpr TRet &emplace(std::initializer_list<TI> il, Ts &&...args) {
    data_ = member_t(std::in_place, il, std::forward<decltype(args)>(args)...);
    return member_t::exp(data_);
  }

  constexpr void
  swap(expected &other) noexcept(std::is_nothrow_swappable_v<TExp> &&
                                 std::is_nothrow_swappable_v<TErr>) {
    data_.swap(other.data_);
  }
  template <typename TV2, typename TE2>
  friend constexpr bool operator==(expected const &l,
                                   expected<TV2, TE2> const &r) {
    if (l.has_value() == r.has_value()) {
      if (l.has_value()) {
        return *l == *r;
      } else {
        return l.error() == r.error();
      }
    } else {
      return false;
    }
  }
};
} // namespace cgui::bp

#endif

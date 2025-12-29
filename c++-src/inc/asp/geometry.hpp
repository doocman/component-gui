
#ifndef COMPONENT_GUI_ASP_GEOMETRY_HPP
#define COMPONENT_GUI_ASP_GEOMETRY_HPP

#include <cmath>
#include <concepts>
#include <ranges>
#include <type_traits>

#include <asp/import/mp-units.hpp>

#include <asp/call.hpp>
#include <asp/std-backport/concepts.hpp>
#include <asp/std-backport/limits.hpp>
#include <asp/std-backport/type_traits.hpp>
#include <asp/annotate.hpp>
#include <asp/warnings.hpp>
#include <asp/compat.hpp>

namespace asp {
ASP_EXPORT_BEGIN

inline constexpr struct point final
    : mp_units::named_unit<"point", mp_units::kind_of<mp_units::isq::length>> {
} point;
inline constexpr struct pixel final
    : mp_units::named_unit<"pixel", mp_units::kind_of<mp_units::isq::length>> {
} pixel;
inline constexpr auto pixel_width = mp_units::isq::width[pixel];
inline constexpr auto pixel_height = mp_units::isq::height[pixel];
inline constexpr auto point_width = mp_units::isq::width[point];
inline constexpr auto point_height = mp_units::isq::height[point];
inline constexpr auto point_per_pixel = point / pixel;
inline constexpr auto pixel_per_point = pixel / point;
inline constexpr struct frame final
    : mp_units::named_unit<"frame", mp_units::kind_of<mp_units::isq::time>> {
} frame;

template <mp_units::Reference auto R, typename Rep>
using from_zero_quantity_point_t =
    mp_units::quantity_point<R, mp_units::default_point_origin(R), Rep>;

template <typename T>
concept has_rep = requires() { typename std::remove_cvref_t<T>::rep; };
template <typename T>
using representation_of_t = typename std::remove_cvref_t<T>::rep;

template <typename T>
concept has_width_height = requires(T const &t) {
  { call::width(t) } -> bp::not_void;
  { call::height(t) } -> bp::not_void;
};
/// @brief Concept for bounding box types.
template <typename T>
concept bounding_box = has_width_height<T> && requires(T const &t) {
  { call::l_x(t) } -> bp::not_void;
  { call::t_y(t) } -> bp::not_void;
  { call::r_x(t) } -> bp::not_void;
  { call::b_y(t) } -> bp::not_void;
};
template <typename T>
concept two_dimensional_coordinate = requires(T const &t) {
  { call::get_x(t) } -> bp::not_void;
  { call::get_y(t) } -> bp::not_void;
};
template <typename T>
concept has_unit = requires() { std::remove_cvref_t<T>::unit; };
template <typename T, auto U>
concept has_unit_of = has_unit<T> && std::remove_cvref_t<T>::unit == U;
template <typename T>
concept is_any_quantity = mp_units::Quantity<std::remove_cvref_t<T>>;
template <typename T>
concept is_any_quantity_point = mp_units::QuantityPoint<std::remove_cvref_t<T>>;
template <typename Q, auto R>
concept is_quantity =
    is_any_quantity<Q> && mp_units::Reference<decltype(R)> &&
    mp_units::QuantityOf<std::remove_cvref_t<Q>, get_quantity_spec(R)> &&
    has_unit_of<Q, get_unit(R)>;
template <typename QP, auto R>
concept is_quantity_point =
    is_any_quantity_point<QP> && mp_units::Reference<decltype(R)> &&
    mp_units::QuantityPointOf<std::remove_cvref_t<QP>, get_quantity_spec(R)> &&
    has_unit_of<QP, get_unit(R)>;

template <typename T>
concept two_dimensional_point = two_dimensional_coordinate<T> && has_unit<T> &&
                                has_rep<T> && requires(T const &t) {
                                  { call::get_x(t) } -> is_any_quantity_point;
                                  { call::get_y(t) } -> is_any_quantity_point;
                                };
template <typename T>
concept two_dimensional_delta =
    two_dimensional_coordinate<T> && has_unit<T> && requires(T const &t) {
      { call::get_x(t) } -> is_any_quantity;
      { call::get_y(t) } -> is_any_quantity;
    };
template <typename T>
concept is_width_and_height = requires(T const &t) {
  { call::get_x(t) } -> is_any_quantity;
  { call::get_y(t) } -> is_any_quantity;
};
template <typename T, auto U>
concept is_width_and_height_with_unit =
    is_width_and_height<T> && requires(T const &t) {
      { call::get_x(t) } -> is_quantity<mp_units::isq::width[U]>;
      { call::get_y(t) } -> is_quantity<mp_units::isq::height[U]>;
    };

template <has_unit T>
inline constexpr auto unit_of_type = std::remove_cvref_t<T>::unit;

template <typename T>
using x_ref_of_t = call::call_result_cvref_t<call::get_x, T>;
template <typename T>
using y_ref_of_t = call::call_result_cvref_t<call::get_x, T>;
template <typename T>
using x_of_t = call::call_result_t<call::get_x, T>;
template <typename T>
using y_of_t = call::call_result_t<call::get_x, T>;

template <typename T, typename U>
concept same_unit_as =
    has_unit<T> && has_unit<U> && unit_of_type<T> == unit_of_type<U>;

template <typename T, auto R>
concept two_dimensional_delta_of =
    two_dimensional_delta<T> && unit_of_type<T> == R;

template <typename...> struct common_unit_base {};
template <has_unit T, same_unit_as<T>... Ts> struct common_unit_base<T, Ts...> {
  static constexpr mp_units::Unit auto unit = unit_of_type<T>;
};

template <typename... Ts> struct common_rep_base : common_unit_base<Ts...> {};
template <has_rep T, has_rep... Ts>
  requires(std::same_as<representation_of_t<T>, representation_of_t<Ts>> && ...)
struct common_rep_base<T, Ts...> : common_unit_base<T, Ts...> {
  using rep = representation_of_t<T>;
};

template <typename... Ts>
struct common_aliases_base : common_rep_base<Ts...> {};

template <is_any_quantity_point QP>
constexpr QP center_between(QP lhs, QP rhs) {
  if constexpr (std::floating_point<typename QP::rep>) {
    auto sum_quantity_from_zero = lhs.quantity_from(QP::point_origin) +
                                  rhs.quantity_from(QP::point_origin);
    return QP(sum_quantity_from_zero / 2, QP::point_origin);
  } else {
    static_assert(std::integral<typename QP::rep>);
    return lhs + (rhs - lhs) / 2;
  }
}
template <two_dimensional_coordinate C>
constexpr C center_between(C lhs, C rhs) {
  return C(center_between(call::get_x(lhs), call::get_x(rhs)),
           center_between(call::get_y(lhs), call::get_y(rhs)));
}

template <typename T>
concept has_reference_member = requires() { T::reference; };
template <typename T>
concept has_unit_reference =
    has_reference_member<T> && mp_units::Reference<decltype(T::reference)>;
struct no_unit_reference {
  friend constexpr bool
  operator==(const no_unit_reference &,
             const no_unit_reference &) noexcept = default;
};

template <typename T> constexpr auto get_unit_or_no_unit_reference() {
  if constexpr (has_unit<T>) {
    return T::unit;
  } else {
    return no_unit_reference{};
  }
}

/// @brief Basic structure for representing screen coordinates.
template <typename TX, typename TY>
struct xy_pair : common_aliases_base<TX, TY> {
  using x_t = TX;
  using y_t = TY;
  TX x{};
  TY y{};

  template <std::convertible_to<TX> X = TX, std::convertible_to<TY> Y = TY>
  constexpr xy_pair(X &&x_in, Y &&y_in)
      : x(std::forward<decltype(x_in)>(x_in)),
        y(std::forward<decltype(y_in)>(y_in)) {}
  constexpr xy_pair() = default;
  template <std::convertible_to<TX> XIn, std::convertible_to<TY> YIn>
    requires(!std::convertible_to<XIn, TY> && std::convertible_to<YIn, TX>)
  constexpr xy_pair(YIn &&y_in, XIn &&x_in)
      : x(std::forward<XIn>(x_in)), y(std::forward<YIn>(y_in)) {}
};

template <mp_units::Unit auto R, typename Rep>
using basic_coordinate =
    xy_pair<from_zero_quantity_point_t<mp_units::isq::width[R], Rep>,
            from_zero_quantity_point_t<mp_units::isq::height[R], Rep>>;

template <mp_units::Unit auto R, typename Rep>
using basic_coordinate_delta =
    xy_pair<mp_units::quantity<mp_units::isq::width[R], Rep>,
            mp_units::quantity<mp_units::isq::height[R], Rep>>;

template <mp_units::Unit auto R, typename Rep>
constexpr mp_units::quantity<mp_units::isq::width[R], Rep>
width(xy_pair<mp_units::quantity<mp_units::isq::width[R], Rep>,
              mp_units::quantity<mp_units::isq::height[R], Rep>>
          xy) {
  return xy.x;
};
template <mp_units::Unit auto R, typename Rep>
constexpr mp_units::quantity<mp_units::isq::height[R], Rep>
height(xy_pair<mp_units::quantity<mp_units::isq::width[R], Rep>,
               mp_units::quantity<mp_units::isq::height[R], Rep>>
           xy) {
  return xy.y;
};

template <typename X1, typename Y1, std::equality_comparable_with<X1> X2,
          std::equality_comparable_with<Y1> Y2>
constexpr bool operator==(xy_pair<X1, Y1> const &l, xy_pair<X2, Y2> const &r) {
  return (l.x == r.x) && (l.y == r.y);
}

template <typename... Ts>
concept all_multipliable = requires(Ts &&...ts) { (ts * ...); };
template <typename T>
concept squarable = all_multipliable<T, T>;
template <typename... Ts>
concept all_summable = requires(Ts &&...ts) { (ts + ...); };
template <typename Num, typename Den>
concept dividable_with = requires(Num n, Den d) { n / d; };

template <typename T>
concept is_geometric = bounding_box<T> || two_dimensional_coordinate<T>;

template <typename T, typename U>
concept same_geometry_as =
    is_geometric<T> && is_geometric<U> && bounding_box<T> == bounding_box<U> &&
    two_dimensional_coordinate<T> == two_dimensional_coordinate<U>;

template <typename T, typename U>
concept same_unit_geometry_as = same_geometry_as<T, U> && same_unit_as<T, U>;

template <typename... Ts>
  requires all_multipliable<Ts...>
using product_result_t = decltype((std::declval<Ts>() * ...));
template <typename T>
  requires all_multipliable<T, T>
using square_result_t = product_result_t<T, T>;
template <typename Num, typename Den>
  requires dividable_with<Num, Den>
using divide_result_t = decltype(std::declval<Num>() / std::declval<Den>());

template <typename... Ts>
concept all_sum_squarable =
    (squarable<Ts> && ...) && all_summable<square_result_t<Ts>...>;

template <typename... Ts>
  requires all_summable<Ts...>
using sum_result_t = decltype((std::declval<Ts>() + ...));
template <typename... Ts>
using sum_square_result_t = sum_result_t<square_result_t<Ts>...>;

template <typename X, typename Y>
  requires(all_sum_squarable<X, Y>)
constexpr sum_square_result_t<X, Y> length_square(xy_pair<X, Y> const &v) {
  return call::get_x(v) * call::get_x(v) + call::get_y(v) * call::get_y(v);
}
template <typename X, typename Y>
constexpr decltype(sqrt(std::declval<sum_square_result_t<X, Y>>()))
length(xy_pair<X, Y> const &v) {
  using std::sqrt;
  return sqrt(length_square(v));
}

template <typename X, typename Y, typename Den>
  requires(dividable_with<X, Den> && dividable_with<Y, Den>)
constexpr xy_pair<divide_result_t<X, Den>, divide_result_t<Y, Den>>
operator/(xy_pair<X, Y> const &xy, Den const &den) {
  return {xy.x / den, xy.y / den};
}

template <typename X1, typename Y1, std::totally_ordered_with<X1> X2,
          std::equality_comparable_with<Y1> Y2>
constexpr bool operator==(xy_pair<X1, Y1> const &l, xy_pair<X2, Y2> const &r) {
  auto xcmp = l.x <=> r.x;
  if (xcmp == 0) {
    return l.y <=> r.y;
  } else {
    return xcmp;
  }
}

template <typename X1, typename Y1, typename X2, typename Y2>
  requires(bp::subtractable_with<X1, X2> && bp::subtractable_with<Y1, Y2>)
constexpr xy_pair<bp::subtract_result_t<X1 const &, X2 const &>,
                  bp::subtract_result_t<Y1 const &, Y2 const &>>
operator-(xy_pair<X1, Y1> const &lhs, xy_pair<X2, Y2> const &rhs) {
  return {lhs.x - rhs.x, lhs.y - rhs.y};
}

template <typename TX, typename TY>
xy_pair(TX &&, TY &&)
    -> xy_pair<std::remove_cvref_t<TX>, std::remove_cvref_t<TY>>;

/// @brief Retrieves the x-coordinate from a basic pixel coordinate.
template <typename TX, typename TY>
constexpr TX const &get_x(xy_pair<TX, TY> const &c) {
  return c.x;
}

/// @brief Retrieves the y-coordinate from a basic pixel coordinate.
template <typename TX, typename TY>
constexpr TY const &get_y(xy_pair<TX, TY> const &c) {
  return c.y;
}

/// @brief Returns a reference to the x-coordinate of a basic pixel coordinate.
template <typename TX, typename TY> constexpr TX &get_x(xy_pair<TX, TY> &c) {
  return c.x;
}

/// @brief Returns a reference to the y-coordinate of a default pixel
/// coordinate.
template <typename TX, typename TY> constexpr TY &get_y(xy_pair<TX, TY> &c) {
  return c.y;
}

template <mp_units::Reference auto R, typename Rep> struct basic_rectangle {
  static constexpr auto reference = R;
  static constexpr auto unit = mp_units::get_unit(R);
  using rep = Rep;

  using x_t =
      mp_units::quantity_point<mp_units::isq::width[R],
                               default_point_origin(mp_units::isq::width[R]),
                               Rep>;
  using y_t =
      mp_units::quantity_point<mp_units::isq::height[R],
                               default_point_origin(mp_units::isq::height[R]),
                               Rep>;
  using width_t = mp_units::quantity<mp_units::isq::width[R], Rep>;
  using height_t = mp_units::quantity<mp_units::isq::height[R], Rep>;
  x_t left_x_{};
  x_t right_x_{};
  y_t top_y_{};
  y_t bottom_y_{};

  constexpr basic_rectangle() noexcept(
      std::is_nothrow_default_constructible_v<Rep>) = default;

  constexpr basic_rectangle(x_t left, y_t top, x_t right, y_t bottom)
      : left_x_(left), right_x_(right), top_y_(top), bottom_y_(bottom) {
    assert(left_x_ <= right_x_);
    assert(top_y_ <= bottom_y_);
  }
  template <std::convertible_to<x_t> LX = x_t,
            std::convertible_to<y_t> TY = y_t,
            std::convertible_to<width_t> W = width_t,
            std::convertible_to<height_t> H = height_t>
  constexpr basic_rectangle(LX lx, TY yt, W w, H h)
      : left_x_(std::forward<decltype(lx)>(lx)), right_x_(left_x_ + w),
        top_y_(std::forward<decltype(yt)>(yt)), bottom_y_(top_y_ + h) {
    assert(left_x_ <= right_x_);
    assert(top_y_ <= bottom_y_);
  }
  template <two_dimensional_coordinate Corner = xy_pair<x_t, y_t>>
  constexpr basic_rectangle(Corner top_left, Corner bottom_right)
    requires(requires() {
              { call::get_x(top_left) } -> std::convertible_to<x_t>;
              { call::get_y(top_left) } -> std::convertible_to<y_t>;
            })
      : left_x_(call::get_x(top_left)), right_x_(call::get_x(bottom_right)),
        top_y_(call::get_y(top_left)), bottom_y_(call::get_y(bottom_right)) {}

  static constexpr basic_rectangle from_xywh(x_t lx, y_t ty, width_t w,
                                             height_t h) {
    return {lx, ty, w, h};
  }

  constexpr auto &&l_x(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s).left_x_;
  }
  constexpr auto &&t_y(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s).top_y_;
  }
  constexpr auto &&r_x(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s).right_x_;
  }
  constexpr auto &&b_y(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s).bottom_y_;
  }
  constexpr bool operator==(basic_rectangle const &) const noexcept = default;
};

template <mp_units::QuantityPoint X, mp_units::QuantityPoint Y, mp_units::Quantity W, mp_units::Quantity H>
requires (
  bp::all_are_equal(unit_of_type<X>, unit_of_type<Y>, unit_of_type<W>, unit_of_type<H>)
  && bp::all_are_same_types<representation_of_t<X>, representation_of_t<Y>, representation_of_t<W>, representation_of_t<H>>)
basic_rectangle(X, Y, W, H) -> basic_rectangle<unit_of_type<X>, representation_of_t<X>>;

template <mp_units::QuantityPoint X, mp_units::QuantityPoint Y>
requires (
  bp::all_are_equal(unit_of_type<X>, unit_of_type<Y>)
  && bp::all_are_same_types<representation_of_t<X>, representation_of_t<Y>>)
basic_rectangle(X, Y, X, Y) -> basic_rectangle<unit_of_type<X>, representation_of_t<X>>;
template <two_dimensional_point P>
basic_rectangle(P, P) -> basic_rectangle<unit_of_type<P>, representation_of_t<P>>;

template <is_scalar T>
using point_rect = basic_rectangle<point, T>;
template <is_scalar T>
using pixel_rect = basic_rectangle<pixel, T>;

using default_point_rect = point_rect<float>;
using default_pixel_rect = pixel_rect<float>;

/// @brief Concept for types that represent a size with width and height.
template <typename T>
concept size_wh = requires(T const &t) {
  { call::width(t) } -> bp::not_void;
  { call::height(t) } -> bp::not_void;
};

/// @brief Basic structure for representing size with width and height.
template <typename W, typename H> struct basic_size_wh : common_aliases_base<W, H> {
  W width_;
  H height_;

  /// @brief Retrieves the width of the size. Best invoked with call::width()
  static constexpr auto &&width(bp::cvref_type<basic_size_wh> auto &&wh) {
    return std::forward<decltype(wh)>(wh).w;
  }
  /// @brief Retrieves the height of the size. Best invoked with call::height()
  static constexpr auto &&height(bp::cvref_type<basic_size_wh> auto &&wh) {
    return std::forward<decltype(wh)>(wh).h;
  }
};

template <mp_units::Unit auto u, typename Rep>
using basic_unit_size_wh = basic_size_wh<
  mp_units::quantity<mp_units::isq::width[u], Rep>,
  mp_units::quantity<mp_units::isq::height[u], Rep>
>;

template <typename Rep>
using point_size_wh = basic_unit_size_wh<point, Rep>;
template <typename Rep>
using pixel_size_wh = basic_unit_size_wh<pixel, Rep>;

/// Generates a lazy view of all (integer) pointer between left and right x of
/// b.
constexpr auto x_view(bounding_box auto &&b) {
  return std::views::iota(call::l_x(b), call::r_x(b));
}

/// Generates a lazy view of all (integer) pointer between top and bottom y of
/// b.
constexpr auto y_view(bounding_box auto &&b) {
  return std::views::iota(call::t_y(b), call::b_y(b));
}

ASP_EXPORT_END

namespace impl {
template <typename T, typename... Args>
concept has_from_xyxy = requires(bp::as_forward<Args>... vs) {
  { std::remove_cvref_t<T>::from_xyxy(*vs...) } -> bounding_box;
};
template <typename T, typename... Args>
concept has_from_xywh = requires(bp::as_forward<Args>... vs) {
  { std::remove_cvref_t<T>::from_xywh(*vs...) } -> bounding_box;
};
template <typename T, typename... Args>
concept has_bbox_init = has_from_xyxy<T, Args...> || has_from_xywh<T, Args...>;

struct do_from_xyxy {
  template <typename X, typename Y, has_bbox_init<X, Y, X, Y> T>
  constexpr bounding_box auto operator()(std::type_identity<T> const &, X xl,
                                         Y yt, X xr, Y yb) const {
    using raw_t = std::remove_cvref_t<T>;
    if constexpr (has_from_xyxy<T, X, Y, X, Y>) {
      return raw_t::from_xyxy(std::move(xl), std::move(yt), std::move(xr),
                              std::move(yb));
    } else if constexpr (has_from_xywh<T, X, Y, X, Y>) {
      auto w = xr - xl;
      auto h = yb - yt;
      return raw_t::from_xywh(std::move(xl), std::move(yt), w, h);
    }
  }
};
struct do_from_xywh {
  template <typename X, typename Y, typename W, typename H,
            has_bbox_init<X, Y, W, H> T>
  constexpr bounding_box auto operator()(std::type_identity<T> const &ti, X x,
                                         Y y, W w, H h) const {
    if constexpr (has_from_xywh<T, X, Y, W, H>) {
      return T::from_xywh(x, y, w, h);
    } else {
      return do_from_xyxy{}(ti, x, y, x + w, y + h);
    }
  }
};

template <typename TV1, typename TV2, typename /*mut_box_pair<TV1, TV2>*/ T,
          typename TTL, typename TBR>
constexpr void set_xx_or_yy(T b, TV1 tl, TV2 br, TTL getset1, TBR getset2) {
  if constexpr (is_placeholder_v<TV1>) {
    impl::set_xx_or_yy(b, tl(getset1, *b), br, getset1, getset2);
  } else if constexpr (is_placeholder_v<TV2>) {
    impl::set_xx_or_yy(b, tl, br(getset2, *b), getset1, getset2);
  } else {
    getset1(*b, tl);
    getset2(*b, br);
  }
}

}; // namespace impl
/// @endcond

ASP_EXPORT_BEGIN

/// Creates a box (presumably of type T) from two XY coordinates.
template <typename T, typename X, typename Y>
  requires(impl::has_bbox_init<T, X, Y, X, Y> ||
           impl::has_bbox_init<extend_api_t<T>, X, Y, X, Y>)
constexpr auto box_from_xyxy(X xl, Y yt, X xr, Y yb,
                             std::type_identity<T> = {}) {
  if constexpr (impl::has_bbox_init<T, X, Y, X, Y>) {
    return impl::do_from_xyxy{}(std::type_identity<T>{}, xl, yt, xr, yb);
  } else {
    return impl::do_from_xyxy{}(std::type_identity<extend_api_t<T>>{}, xl, yt,
                                 xr, yb);
  }
}

/// Creates a box (presumably of type T) from a top-left coordinate + width and
/// height.
template <typename T, typename X, typename Y, typename W, typename H>
  requires(impl::has_bbox_init<T, X, Y, W, H> ||
           impl::has_bbox_init<extend_api_t<T>, X, Y, W, H>)
constexpr auto box_from_xywh(X x, Y y, W w, H h, std::type_identity<T> = {}) {
  if constexpr (impl::has_bbox_init<T, X, Y, W, H>) {
    return impl::do_from_xywh{}(std::type_identity<T>{}, x, y, w, h);
  } else {
    return impl::do_from_xywh{}(std::type_identity<extend_api_t<T>>{}, x, y, w,
                                h);
  }
}

/// Copies a box of type T2 into a box of type T.
template <bounding_box T, bounding_box T2> constexpr T copy_box(T2 &&b) {
  if constexpr (bp::cvref_type<T2, T>) {
    return std::forward<T2>(b);
  } else if constexpr (std::constructible_from<T, T2 &&>) {
    return T(std::forward<T2>(b));
  } else if constexpr (impl::has_from_xywh<T, decltype(call::l_x(b)),
                                           decltype(call::t_y(b)),
                                           decltype(call::width(b)),
                                           decltype(call::height(b))>) {
    return box_from_xywh<T>(call::l_x(b), call::t_y(b), call::width(b),
                            call::height(b));
  } else {
    return box_from_xyxy<T>(call::l_x(b), call::t_y(b), call::r_x(b),
                            call::b_y(b));
  }
}

/// Range checker that models the open range min < c < max.
inline constexpr struct inside_open_range_t {
  ASP_STATIC_CALL constexpr bool operator()(auto &&c, auto &&min, auto &&max) ASP_STATIC_CALL_POST {
    return (min < c) && (c < max);
  }
} inside_open_range;
/// Range checker that models the closed range min <= c <= max.
inline constexpr struct inside_closed_range_t {
  ASP_STATIC_CALL constexpr bool operator()(auto &&c, auto &&min, auto &&max) ASP_STATIC_CALL_POST { return (min <= c) && (c <= max); }
} inside_closed_range;

/// Range checker that models the closed-open range min <= c < max.
inline constexpr struct inside_semiopen_range_t {
  ASP_STATIC_CALL constexpr bool operator()(auto &&c, auto &&min, auto &&max) ASP_STATIC_CALL_POST { return (min <= c) && (c < max); }
} inside_semiopen_range;

/// Returns true if width and height are non-negative.
constexpr bool valid_box(bounding_box auto const &b) {
  return (call::width(b) >= decltype(call::width(b)){}) &&
         (call::height(b) >= decltype(call::height(b)){});
}

/// @brief Concept to check if a type T meets the range condition for values of
/// type TX. The range_condition should from a test value and min/max values
/// determine if the test-value is inside the range of min max. Implementations
/// are e.g. open-range, closed-range and semi-open (open-closed).
template <typename T, typename TX>
concept range_condition = requires(T t, TX v) {
  { t(v, v, v) } -> std::convertible_to<bool>;
};

/// Check if coordinate c is inside box b, by the range checking policy
/// inside_range.
template <bounding_box TB, two_dimensional_coordinate TC,
          range_condition<decltype(call::get_x(std::declval<TC>()))> TRC =
              inside_semiopen_range_t>
  requires(same_unit_as<TB, TC>)
constexpr bool hit_box(TB const &b, TC const &c, TRC &&inside_range = {}) {
  ASP_ASSERT(valid_box(b));
  return inside_range(call::get_x(c), call::l_x(b), call::r_x(b)) &&
         inside_range(call::get_y(c), call::t_y(b), call::b_y(b));
}

template <two_dimensional_coordinate T1, same_unit_geometry_as<T1> T2>
constexpr T1 copy_coordinate(T2 &&p) {
  if constexpr (std::constructible_from<T1, T2>) {
    return T1(std::forward<T2>(p));
  } else {
    using out_x = call::call_result_t<call::get_x, T1>;
    using in_x = call::call_result_t<call::get_x, T2>;
    if constexpr (std::is_integral_v<out_x> && !std::is_integral_v<in_x>) {
      // We assume x and y are the same types for both T1 and T2.
      return T1(static_cast<out_x>(call::get_x(p)),
                static_cast<out_x>(call::get_y(p)));
    } else {
      return T1(call::get_x(p), call::get_y(p));
    }
  }
}

constexpr auto square_value(auto &&v) { return v * v; }

template <two_dimensional_coordinate T1, same_unit_geometry_as<T1> T2>
constexpr auto distance_squared(T1 const &p1, T2 const &p2) {
  return square_value(call::get_x(p1) - call::get_x(p2)) +
         square_value(call::get_y(p1) - call::get_y(p2));
}

/// Split the bounding box pointed to by T at x. b keeps the left part while the
/// returned area is the right part. Use "trim_from_*" to use width/height
/// arithmetics instead.
template <bounding_box B>
constexpr auto split_x(out<B&> b, call::call_result_t<call::get_left_x, B> x) {
  auto res = box_from_xyxy<B>(x, call::t_y(*b),call::r_x(*b), call::b_y(*b));
  call::set_right_x(*b, x);
  return res;
}

/// Split the bounding box pointed to by T at y. b keeps the upper part while
/// the returned area is the lower part. Use "trim_from_*" to use width/height
/// arithmetics instead.
template <bounding_box B>
constexpr auto split_y(out<B&> b, call::call_result_t<call::get_top_y, B> y) {
  ASP_ASSERT(b != nullptr);
  auto res = box_from_xyxy<B>(call::l_x(*b), y,
                                                  call::r_x(*b), call::b_y(*b));
  call::set_bottom_y(*b, y);
  return res;
}

ASP_EXPORT_END
#if 0

/// Set the pointer b to have the x-values lx-rx. Use "keep_current" to only
/// change a single value. May use placeholders.
template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T>
constexpr void set_xx(T b, TV1 lx, TV2 rx) {
  impl::set_xx_or_yy(b, lx, rx, call::l_x, call::r_x);
}

/// Set the pointer b to have the y-values ty-by. Use "keep_current" to only
/// change a single value. May use placeholders.
template <typename TV1, typename TV2, mut_box_pair<TV1, TV2> T>
constexpr void set_yy(T b, TV1 ty, TV2 by) {
  impl::set_xx_or_yy(b, ty, by, call::t_y, call::b_y);
}

/// Create a bounding box that has the same dimensions as b, but with its top
/// left corner at tl.
template <bounding_box TB, pixel_coord TC>
constexpr auto move_tl_to(TB b, TC tl) {
  auto w = call::width(b);
  auto h = call::height(b);
  call::l_x(b, call::get_x(tl));
  call::t_y(b, call::get_y(tl));
  call::width(b, w);
  call::height(b, h);
  return b;
}

/// Cut away a part v from the left side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_left(T bptr, TV v) {
  ASP_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  ASP_ASSERT(v <= call::width(b));
  auto org_lx = call::l_x(b);
  auto split_x = static_cast<decltype(org_lx)>(org_lx + v);
  set_xx(&b, split_x, keep_current);
  return box_from_xyxy<bp::dereferenced_t<T>>(org_lx, call::t_y(b), split_x,
                                              call::b_y(b));
}

/// Cut away a part v from the top side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_above(T bptr, TV v) {
  ASP_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  ASP_ASSERT(v <= call::height(b));
  auto org_y = call::t_y(b);
  auto split_y = static_cast<decltype(org_y)>(org_y + v);
  set_yy(&b, split_y, keep_current);
  return box_from_xyxy<bp::dereferenced_t<T>>(call::l_x(b), org_y, call::r_x(b),
                                              split_y);
}

/// Cut away a part v from the right side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_right(T bptr, TV v) {
  ASP_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  ASP_ASSERT(v <= call::width(b));
  call::width(b, call::width(b) - v);
  return box_from_xywh<bp::dereferenced_t<T>>(call::r_x(b), call::t_y(b), v,
                                              call::height(b));
}

/// Cut away a part v from the lower side of the box pointer at by bptr. Returns
/// the new part.
template <typename TV, mut_box_pointer<TV> T>
constexpr auto trim_from_below(T bptr, TV v) {
  ASP_ASSERT(bptr != nullptr);
  auto &b = *bptr;
  ASP_ASSERT(v <= call::height(b));
  call::height(b, call::height(b) - v);
  return box_from_xywh<bp::dereferenced_t<T>>(call::l_x(b), call::b_y(b),
                                              call::width(b), v);
}

/// Creates a larger box that includes the smaller boxes. Does not check for
/// empty boxes. Use box_add when the boxes may be empty or non-valid.
template <typename TRes = void, bounding_box T1, same_unit_geometry_as<T1> T2>
constexpr auto box_union(T1 const &b1, T2 const &b2) {
  ASP_ASSERT(valid_box(b1));
  ASP_ASSERT(valid_box(b2));
  using result_t =
      std::conditional_t<std::is_void_v<TRes>, std::common_type<T1, T2>,
                         std::type_identity<TRes>>::type;
  return box_from_xyxy<result_t>(std::min(call::l_x(b1), call::l_x(b2)),
                                 std::min(call::t_y(b1), call::t_y(b2)),
                                 std::max(call::r_x(b1), call::r_x(b2)),
                                 std::max(call::b_y(b1), call::b_y(b2)));
}

/// Creates a larger box that is the intersection of both b1 and b2.
template <typename TRes = void, bounding_box T1, bounding_box T2>
  requires(same_unit_as<T1, T2> &&
           (same_unit_as<TRes, T1> || std::is_void_v<TRes>))
constexpr auto box_intersection(T1 const &b1, T2 const &b2) {
  ASP_ASSERT(valid_box(b1));
  ASP_ASSERT(valid_box(b2));
  using result_t = typename std::conditional_t<std::is_void_v<TRes>,
                                               std::common_type<T1, T2>,
                                               std::type_identity<TRes>>::type;
  auto lx = std::max(call::l_x(b1), call::l_x(b2));
  auto ty = std::max(call::t_y(b1), call::t_y(b2));
  auto rx = std::min(call::r_x(b1), call::r_x(b2));
  auto by = std::min(call::b_y(b1), call::b_y(b2));
  return box_from_xyxy<result_t>(lx, ty, std::max(rx, lx), std::max(by, ty));
}

/// Creates a new pixel_coord that has moved left by val.
constexpr auto nudge_left(pixel_coord auto c,
                          same_unit_as<decltype(c)> auto &&val) {
  call::x_of(c, call::get_x(c) - val);
  return c;
}
/// Creates a new pixel_coord that has moved right by val.
constexpr auto nudge_right(pixel_coord auto c,
                           same_unit_as<decltype(c)> auto &&val) {
  return nudge_left(c, -val);
}
/// Creates a new pixel_coord that has moved up by val.
constexpr auto nudge_up(pixel_coord auto c,
                        same_unit_as<decltype(c)> auto &&val) {
  call::y_of(c, call::get_y(c) - val);
  return c;
}
/// Creates a new pixel_coord that has moved down by val.
constexpr auto nudge_down(pixel_coord auto c,
                          same_unit_as<decltype(c)> auto &&val) {
  return nudge_up(c, -val);
}

/// Creates a new box that has moved left by val.
constexpr auto nudge_left(bounding_box auto b, auto &&val) {
  set_xx(&b, call::l_x(b) - val, call::r_x(b) - val);
  return std::forward<decltype(b)>(b);
}
/// Creates a new box that has moved right by val.
constexpr auto nudge_right(bounding_box auto b, auto &&val) {
  return nudge_left(b, -val);
}
/// Creates a new box that has moved up by val.
constexpr auto nudge_up(bounding_box auto b, auto &&val) {
  set_yy(&b, call::t_y(b) - val, call::b_y(b) - val);
  return std::forward<decltype(b)>(b);
}
/// Creates a new box that has moved down by val.
constexpr auto nudge_down(bounding_box auto b, auto &&val) {
  return nudge_up(b, -val);
}

/// True if all corners of inner is inside the outer box.
template <bounding_box TB1, same_unit_geometry_as<TB1> TB2>
  requires(same_unit_as<TB1, TB2>)
constexpr bool box_includes_box(TB1 const &outer, TB2 const &inner) {
  if constexpr (size_tagged<TB1>) {
    return hit_box(outer, TB2::top_left(inner), inside_closed_range) &&
           hit_box(outer, TB2::bottom_right(inner), inside_closed_range);
  } else {
    return hit_box(outer, call::top_left(inner), inside_closed_range) &&
           hit_box(outer, call::bottom_right(inner), inside_closed_range);
  }
}

/// Returns true if the box is empty.
constexpr bool empty_box(bounding_box auto const &b) {
  return call::width(b) == bp::default_init_valued ||
         call::height(b) == bp::default_init_valued;
}

/// Calculates the length-squared of a coordinate vector
/// \tparam T Type of coordinate
/// \param p Coordinate vector
/// \return Scalar that is p^T p
template <pixel_coord T> constexpr auto length_sqr(T const &p) {
  if constexpr (size_tagged<T>) {
    // We currently don't support 'unit to the power of ...'.
    return length_sqr(p.value());
  } else {
    return square_value(call::get_x(p)) + square_value(call::get_y(p));
  }
}

/// Calculates the length-squared of a coordinate vector
/// \tparam T Type of coordinate
/// \param p Coordinate vector
/// \return Scalar that is p^T p
template <pixel_coord T> constexpr auto length(T const &p) {
  if constexpr (size_tagged<T>) {
    return T{std::sqrt(length_sqr(p.value()))};
  } else {
    return std::sqrt(length_sqr(p));
  }
}

/// Creates a box of type T by applying map_f on b.
/// \tparam T Return type
/// \tparam T2 Incoming box type
/// \param b box to morph
/// \param map_f function f(x)->x that maps all coordinates and sizes. The
/// map-function should be linear, as it does not know if it is called with the
/// top-left + bottom-right coordinates or with the top-left + width & height
/// coordinates.
/// \return new morphed box.
template <bounding_box T, bounding_box T2>
constexpr T map_box(T2 const &b, auto &&map_f) {
  if constexpr (impl::has_from_xywh<T, decltype(call::l_x(b))>) {
    return box_from_xywh<T>(map_f(call::l_x(b)), map_f(call::t_y(b)),
                            map_f(call::width(b)), map_f(call::height(b)));
  } else {
    return box_from_xyxy<T>(map_f(call::l_x(b)), map_f(call::t_y(b)),
                            map_f(call::r_x(b)), map_f(call::b_y(b)));
  }
}

/// Version of box_union that supports empty boxes.
template <typename TRes = void, typename TB1, typename TB2>
constexpr auto box_add(TB1 const &b1, TB2 const &b2)
    -> decltype(box_union<TRes>(b1, b2)) {
  using result_t = decltype(box_union<TRes>(b1, b2));
  if (empty_box(b1)) {
    return copy_box<result_t>(b2);
  }
  if (empty_box(b2)) {
    return copy_box<result_t>(b1);
  }
  return box_union<TRes>(b1, b2);
}

template <pixelpoint_tag SizeTag, typename T, typename Scale = double>
class autoconverting_pixelpoint_unit {
  Scale s_;
  T v_;

public:
  constexpr autoconverting_pixelpoint_unit(pixelpoint_unit<SizeTag, T> const &v,
                                           pixelpoint_scale auto &&s)
      : s_(s), v_(v.value()) {}
  template <typename T2, pixelpoint_scale S>
    requires(std::constructible_from<T, T2>)
  constexpr autoconverting_pixelpoint_unit(SizeTag, T2 const &v, S &&s)
      : s_(s), v_(v) {}

  constexpr Scale const &pixel_scale() const noexcept { return s_; }

  template <typename ST2 = SizeTag, typename T2 = T>
  constexpr pixelpoint_unit<ST2, T2> convert() const {
    if constexpr (std::is_same_v<SizeTag, ST2>) {
      return pixelpoint_unit<ST2, T2>(v_);
    } else {
      // return convert_to(pixelpoint_unit<SizeTag, T>(v_));
      return pixelpoint_unit<ST2, T2>(pixelpoint_unit<SizeTag, T>(v_), s_);
      // return pixelpoint_unit<ST2, T2>(v_, s_);
    }
  }

  template <typename ST2, typename T2>
  constexpr explicit(false) operator pixelpoint_unit<ST2, T2>() const {
    return convert<ST2, T2>();
  }
};

template <typename ST, typename T, pixelpoint_scale S>
autoconverting_pixelpoint_unit(pixelpoint_unit<ST, T> const &, S &&s)
    -> autoconverting_pixelpoint_unit<ST, T, std::remove_cvref_t<S>>;
template <pixelpoint_tag ST, typename T, pixelpoint_scale S>
autoconverting_pixelpoint_unit(ST, T &&, S &&)
    -> autoconverting_pixelpoint_unit<ST, std::remove_cvref_t<T>,
                                      std::remove_cvref_t<S>>;

template <pixelpoint_tag Tag, typename TagOrg, typename T, typename S>
constexpr pixelpoint_unit<Tag, T>
convert_to(autoconverting_pixelpoint_unit<TagOrg, T, S> const &pu) {
  return pu;
}
#endif
} // namespace asp

#endif

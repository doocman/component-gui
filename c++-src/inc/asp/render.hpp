
#ifndef ASPECT_GUI_ASP_RENDER_HPP
#define ASPECT_GUI_ASP_RENDER_HPP

#include <asp/assert.hpp>
#include <asp/call.hpp>
#include <asp/import/mp-units.hpp>
#include <asp/import/stl.hpp>
#include <asp/geometry.hpp>

namespace asp {
ASP_EXPORT_BEGIN
/// @brief Concept to check if a type T meets the range condition for values of
/// type TX. The range_condition should from a test value and min/max values
/// determine if the test-value is inside the range of min max. Implementations
/// are e.g. open-range, closed-range and semi-open (open-closed).
template <typename T, typename TX>
concept range_condition = requires(T t, TX v) {
  { t(v, v, v) } -> std::convertible_to<bool>;
};

using mp_units::isq::width;
using mp_units::isq::height;

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

ASP_EXPORT_END

#define ASP_NO_CONST(X)                                                        \
  decltype(X) {}

namespace call {
#if 0
namespace impl {
struct do_pixel_area {
  template <typename T>
    requires(has_pixel_area<T const &> ||
             (has_point_area<T const &> && has_pixel_scale<T const &>))
  constexpr /*pixel_rect*/ auto operator()(T const &t) const {
    if constexpr (has_pixel_area<T const &>) {
      return _do_pixel_area::call(t);
    } else {
      return convert_pixelpoint<pixel_size_tag>(_do_pixel_area::call(t),
                                                _do_pixel_scale(t));
    }
  }
};
struct do_point_area {
  template <typename T>
    requires(has_point_area<T const &> ||
             (has_pixel_area<T const &> && has_pixel_scale<T const &>))
  constexpr /*point_rect*/ auto operator()(T const &t) const {
    if constexpr (has_point_area<T const &>) {
      return _do_point_area::call(t);
    } else {
      return convert_pixelpoint<point_size_tag>(_do_pixel_area::call(t),
                                                _do_pixel_scale::call(t));
    }
  }
};
} // namespace impl
#endif
ASP_EXPORT_BEGIN
inline constexpr impl::_do_pixel_area pixel_area;
inline constexpr impl::_do_point_area point_area;
ASP_EXPORT_END
} // namespace call

ASP_EXPORT_BEGIN

template <typename T>
concept is_geometric = bounding_box<T> || two_dimensional_coordinate<T>;

template <typename T, typename U>
concept same_geometry_as =
    is_geometric<T> && is_geometric<U> && bounding_box<T> == bounding_box<U> &&
    two_dimensional_coordinate<T> == two_dimensional_coordinate<U>;

template <typename T, typename U>
concept same_unit_geometry_as = same_geometry_as<T, U> && same_unit_as<T, U>;

template <typename T>
concept colour = requires(T &&t) {
  call::red(t);
  call::blue(t);
  call::green(t);
  call::alpha(t);
};

template <typename T> struct basic_colour_t {
  T red, green, blue, alpha;

  constexpr bool operator==(basic_colour_t const &) const noexcept = default;
};
template <typename T> struct basic_rgb_t {
  T r, g, b;
  static constexpr auto &&red(auto &&c) {
    return std::forward<decltype(c)>(c).r;
  }
  static constexpr auto &&green(auto &&c) {
    return std::forward<decltype(c)>(c).r;
  }
  static constexpr auto &&blue(auto &&c) {
    return std::forward<decltype(c)>(c).r;
  }
  static constexpr T alpha(auto &&) { return std::numeric_limits<T>::max(); }
};

template <typename T, typename S>
  requires(requires(S &s, T const &t) { s << t; })
constexpr S &operator<<(S &stream, basic_colour_t<T> const &c) {
  std::format_to(std::ostreambuf_iterator<char>(stream), "{}", c);
  return stream;
}

using default_colour_t = basic_colour_t<std::uint_least8_t>;
using default_rgb_t = basic_rgb_t<std::uint_least8_t>;

template <typename T> constexpr T &red(basic_colour_t<T> &c) noexcept {
  return c.red;
}
template <typename T> constexpr T red(basic_colour_t<T> const &c) noexcept {
  return c.red;
}
template <typename T> constexpr T &blue(basic_colour_t<T> &c) noexcept {
  return c.blue;
}
template <typename T> constexpr T blue(basic_colour_t<T> const &c) noexcept {
  return c.blue;
}
template <typename T> constexpr T &green(basic_colour_t<T> &c) noexcept {
  return c.green;
}
template <typename T> constexpr T green(basic_colour_t<T> const &c) noexcept {
  return c.green;
}
template <typename T> constexpr T &alpha(basic_colour_t<T> &c) noexcept {
  return c.alpha;
}
template <typename T> constexpr T alpha(basic_colour_t<T> const &c) noexcept {
  return c.alpha;
}

template <mp_units::Reference auto R, typename Rep> struct basic_width_height {
  static constexpr auto reference = R;
  static constexpr auto unit = mp_units::get_unit(R);
  using rep = Rep;

  using width_t = mp_units::quantity<mp_units::isq::width[R], Rep>;
  using height_t = mp_units::quantity<mp_units::isq::height[R], Rep>;

  width_t _w_is_an_implementation_detail{};
  height_t _h_is_an_implementation_detail{};

  constexpr basic_width_height() noexcept = default;
  constexpr basic_width_height(width_t w, height_t h) noexcept
      : _w_is_an_implementation_detail(w), _h_is_an_implementation_detail(h) {}

  constexpr auto &&width(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s)._w_is_an_implementation_detail;
  }
  constexpr auto &&height(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s)._h_is_an_implementation_detail;
  }

  constexpr bool
  operator==(basic_width_height const &) const noexcept = default;
};

template <mp_units::Reference auto R, typename Rep> struct basic_rectangle {
  static constexpr auto reference = R;
  static constexpr auto unit = mp_units::get_unit(R);
  using rep = Rep;

  using left_x_t =
      mp_units::quantity_point<mp_units::isq::width[R],
                               default_point_origin(mp_units::isq::width[R]),
                               Rep>;
  using top_y_t =
      mp_units::quantity_point<mp_units::isq::height[R],
                               default_point_origin(mp_units::isq::height[R]),
                               Rep>;
  using width_t = mp_units::quantity<mp_units::isq::width[R], Rep>;
  using height_t = mp_units::quantity<mp_units::isq::height[R], Rep>;
  left_x_t left_x_{};
  top_y_t top_y_{};
  width_t width_{};
  height_t height_{};

  constexpr basic_rectangle() noexcept(
      std::is_nothrow_default_constructible_v<Rep>) = default;
  template <std::convertible_to<left_x_t> LX = left_x_t,
            std::convertible_to<top_y_t> TY = top_y_t,
            std::convertible_to<width_t> W = width_t,
            std::convertible_to<height_t> H = height_t>
  constexpr basic_rectangle(LX lx, TY yt, W w, H h)
      : left_x_(std::forward<decltype(lx)>(lx)),
        top_y_(std::forward<decltype(yt)>(yt)),
        width_(std::forward<decltype(w)>(w)),
        height_(std::forward<decltype(h)>(h)) {}
  static constexpr basic_rectangle from_xywh(left_x_t lx, top_y_t ty, width_t w,
                                             height_t h) {
    return {lx, ty, w, h};
  }

  constexpr auto &&l_x(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s).left_x_;
  }
  constexpr auto &&t_y(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s).top_y_;
  }
  constexpr auto &&width(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s).width_;
  }
  constexpr auto &&height(this auto &&s) noexcept {
    return std::forward<decltype(s)>(s).height_;
  }
  constexpr bool operator==(basic_rectangle const &) const noexcept = default;
  static_assert(requires() {
    mp_units::get_common_reference(left_x_t::reference, top_y_t::reference,
                                   width_t::reference, height_t::reference);
  });
  static_assert(left_x_t::unit == R);
};

template <typename T, typename... Ts>
  requires(std::equality_comparable_with<T, Ts> && ...)
constexpr bool equals_all_of(T const &t, Ts const &...ts) noexcept {
  return ((t == ts) && ...);
}

template <typename T, typename... Ts>
concept same_as_any_of = (std::same_as<T, Ts> || ...);

template <typename T>
concept is_integer =
    std::integral<T> &&
    !same_as_any_of<T, bool, char, char8_t, char16_t, char32_t, wchar_t>;

template <typename... Ts>
concept all_is_integers = (is_integer<Ts> && ...);

template <mp_units::QuantityPoint QX, mp_units::QuantityPoint QY,
          mp_units::Quantity W, mp_units::Quantity H>
  requires(equals_all_of(QX::unit, QY::unit, W::unit, H::unit) &&
           all_is_integers<typename QX::rep, typename QY::rep, typename W::rep,
                           typename H::rep>)
basic_rectangle(QX, QY, W, H)
    -> basic_rectangle<QX::unit,
                       std::common_type_t<typename QX::rep, typename QY::rep,
                                          typename W::rep, typename H::rep>>;

template <mp_units::Reference auto R, typename Rep, typename U>
constexpr auto operator*(basic_rectangle<R, Rep> const &rect, U const &rhs) {
  return basic_rectangle(
      mp_units::quantity_point(call::l_x(rect).quantity_from_zero() * rhs),
      mp_units::quantity_point(call::t_y(rect).quantity_from_zero() * rhs),
      call::width(rect) * rhs, call::height(rect) * rhs);
}

template <is_integer T> constexpr T lround(T v) { return v; }
template <std::floating_point T> constexpr is_integer auto lround(T v) {
  auto lround_res = std::lround(v);
  if constexpr (sizeof(T) <= 4) {
    return static_cast<std::int_least32_t>(v);
  } else {
    return lround_res;
  }
}
constexpr mp_units::Quantity auto lround(mp_units::Quantity auto q) {
  return lround(q.numerical_value_in(q.unit)) * q.reference;
}
constexpr mp_units::QuantityPoint auto lround(mp_units::QuantityPoint auto q) {
  return mp_units::quantity_point(lround(q.quantity_from_zero()));
}

template <typename T> using lround_t = decltype(lround(std::declval<T>()));

template <mp_units::Reference auto R, typename Rep>
constexpr basic_rectangle<R, lround_t<Rep>>
lround(basic_rectangle<R, Rep> const &rect) {
  return {lround(call::l_x(rect)), lround(call::t_y(rect)),
          lround(call::width(rect)), lround(call::height(rect))};
}

template <typename T, typename... Args>
concept direct_invocable = requires(T &&t, Args &&...args) {
  std::forward<T>(t)(std::forward<Args>(args)...);
};

template <typename T>
concept has_rep = requires() { typename std::remove_cvref_t<T>::rep; };
template <typename T>
using representation_of_t = typename std::remove_cvref_t<T>::rep;

template <typename T, auto R>
concept width_height_with_unit = has_width_height<T> && requires(T &&t) {
  { call::width(t) } -> is_quantity<ASP_NO_CONST(mp_units::isq::width[R])>;
  { call::height(t) } -> is_quantity<ASP_NO_CONST(mp_units::isq::height[R])>;
};

template <typename T, auto R>
concept rectangle_with_unit =
    bounding_box<std::remove_cvref_t<T>> && width_height_with_unit<T, R> &&
    requires(T &&t) {
      {
        call::l_x(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::width[R])>;
      {
        call::t_y(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::height[R])>;
      {
        call::r_x(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::width[R])>;
      {
        call::b_y(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::height[R])>;
    };
template <typename T, auto R>
concept two_dimensional_coordinate_with_unit =
    two_dimensional_coordinate<T> && requires(T &&t) {
      {
        call::x_of(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::width[R])>;
      {
        call::y_of(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::height[R])>;
    };
template <typename T>
concept point_coordinate = two_dimensional_coordinate_with_unit<T, point>;

template <typename T>
concept rep_is_integer =
    has_rep<T> && is_integer<typename std::remove_cvref_t<T>::rep>;

template <typename T>
concept is_rectangle_with_integer_rep = bounding_box<T> && requires(T &&t) {
  { call::l_x(t) } -> rep_is_integer;
  { call::t_y(t) } -> rep_is_integer;
  { call::width(t) } -> rep_is_integer;
  { call::height(t) } -> rep_is_integer;
};
template <typename T>
concept is_two_dimensional_coordinate_with_integer_rep =
    two_dimensional_coordinate<T> && requires(T &&t) {
      { call::x_of(t) } -> rep_is_integer;
      { call::y_of(t) } -> rep_is_integer;
    };
template <typename T>
concept is_int_pixel_rectangle =
    is_rectangle_with_integer_rep<T> && rectangle_with_unit<T, pixel>;
template <typename T>
concept is_int_pixel_coordinate =
    is_two_dimensional_coordinate_with_integer_rep<T> &&
    two_dimensional_coordinate_with_unit<T, pixel>;

template <typename TX, typename TY> class nudger {
  TX x_;
  TY y_;

public:
  constexpr nudger(TX x, TY y) : x_(x), y_(y) {}

  constexpr /*pixel_coord*/ auto operator()(auto &&in) const
    requires(requires() {
      nudge_down(in, y_);
      nudge_right(in, x_);
    })
  {
    return nudge_down(nudge_right(in, x_), y_);
  }
};

template <typename T> constexpr auto remove_unit_ref(T &&t) {
  // if constexpr (size_tagged<T>) {
  //   return t.remove_ref();
  // } else {
  return std::forward<T>(t);
  //}
}

template <typename T>
concept has_executing_renderer =
    requires(T &&t) { call::executing_renderer(std::forward<T>(t)); };
template <typename T>
using executing_renderer_t =
    decltype(call::executing_renderer(std::declval<T>()));

template <typename> struct render_command_traits {};
template <typename T>
  requires(requires() { typename T::associated_renderer; })
struct render_command_traits<T> {
  using associated_renderer = T::associated_renderer;
};
template <typename T>
using associated_renderer_t = render_command_traits<T>::associated_renderer;

template <typename T>
concept is_render_command =
    requires() { typename render_command_traits<T>::associated_renderer; } &&
    has_executing_renderer<
        typename render_command_traits<T>::associated_renderer> &&
    requires(T const &t, executing_renderer_t<associated_renderer_t<T>> &r) {
      call::execute(t, r);
    };

template <typename T>
concept fill_rect_command = is_render_command<T> && requires(T const &t) {
  { t.pixel_area() } -> rectangle_with_unit<pixel>;
  { call::colour(t) } -> colour;
};

template <typename T>
concept has_render_commands =
    requires(T &&t, basic_rectangle<pixel, std::int_least32_t> const &r,
             default_colour_t const &c,
             basic_rectangle<point, std::int_least32_t> const &point_box) {
      { call::fill(t, r, c) } -> fill_rect_command;
      { call::to_pixel(t, point_box) } -> is_int_pixel_rectangle;
    };

template <typename T>
concept is_renderer = has_render_commands<T> /*and executing_renderer*/;

template <typename T>
concept is_render_context = requires(T &&t, default_colour_t const &colour) {
  { t.widget_full_area() } -> rectangle_with_unit<point>;
  { t.widget_redraw_area() } -> rectangle_with_unit<point>;
  { call::fill(t, t.widget_redraw_area(), colour) } -> fill_rect_command;
  typename std::remove_cvref_t<T>::renderer;
} && is_renderer<typename std::remove_cvref_t<T>::renderer>;

template <typename T, typename... TVals>
concept has_native_fill =
    requires(bp::as_forward<T> t, bp::as_forward<TVals>... args) {
      call::fill(*t, *args...);
    };

constexpr fill_rect_command auto fill(is_render_context auto &&ctx,
                                      colour auto const &c) {
  return call::fill(ctx, ctx.widget_full_area(), c);
}

template <typename T>
using context_renderer_t = typename std::remove_cvref_t<T>::renderer;

template <typename T, typename TCoord,
          typename TColour>
concept single_pixel_draw = true; // pixel_coordinate<TCoord> && colour<TColour>
                                  // && std::invocable<T, TCoord, TColour>;
template <typename T, typename TCoord>
concept single_alpha_draw =
    true; // pixel_coordinate<TCoord> && std::invocable<T, TCoord,
          // std::uint_least8_t>;
struct dummy_pixel_drawer {
  constexpr void operator()(/*pixel_or_point_coordinate*/ auto &&,
                            colour auto &&) {}
};

template <colour TC> struct fill_on_draw_pixel {
  TC c;
  constexpr void operator()(bounding_box auto &&b,
                            /*single_pixel_draw*/ auto &&cb) const {
    for (auto y : y_view(b)) {
      for (auto x : x_view(b)) {
        cb(basic_coordinate(x, y), c);
      }
    }
  }
};

#if 0
constexpr auto fill = []<typename T, pixel_or_point_rect_basic TB, colour TC>(
                          T &&v, TB const &b, TC const &c)
  requires(has_native_fill<T, TB, TC> ||
           has_draw_pixels<T, TB, fill_on_draw_pixel<TC>>)
{
  auto vf = bp::as_forward<decltype(v)>(v);
  if constexpr (has_native_fill<T, TB, TC>) {
    return call::fill(*vf, b, c);
  } else {
    return call::draw_pixels(*vf, b, fill_on_draw_pixel<TC>{c});
  }
};
#endif

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
inline constexpr auto inside_open_range = [](auto &&c, auto &&min, auto &&max) {
  return (min < c) && (c < max);
};
/// Range checker that models the closed range min <= c <= max.
inline constexpr auto inside_closed_range =
    [](auto &&c, auto &&min, auto &&max) { return (min <= c) && (c <= max); };

/// Range checker that models the closed-open range min <= c < max.
inline constexpr auto inside_semiopen_range =
    [](auto &&c, auto &&min, auto &&max) { return (min <= c) && (c < max); };

using inside_open_range_t = decltype(inside_open_range);
using inside_closed_range_t = decltype(inside_closed_range);
using inside_semiopen_range_t = decltype(inside_semiopen_range);

/// Returns true if width and height are non-negative.
constexpr bool valid_box(bounding_box auto const &b) {
  return (call::width(b) >= decltype(call::width(b)){}) &&
         (call::height(b) >= decltype(call::height(b)){});
}

/// Check if coordinate c is inside box b, by the range checking policy
/// inside_range.
template <bounding_box TB, two_dimensional_coordinate TC,
          range_condition<decltype(call::x_of(std::declval<TC>()))> TRC =
              inside_semiopen_range_t>
  requires(same_unit_as<TB, TC>)
constexpr bool hit_box(TB const &b, TC const &c, TRC &&inside_range = {}) {
  ASP_ASSERT(valid_box(b));
  return inside_range(call::x_of(c), call::l_x(b), call::r_x(b)) &&
         inside_range(call::y_of(c), call::t_y(b), call::b_y(b));
}

template <two_dimensional_coordinate T1, same_unit_geometry_as<T1> T2>
constexpr T1 copy_coordinate(T2 &&p) {
  if constexpr (std::constructible_from<T1, T2>) {
    return T1(std::forward<T2>(p));
  } else {
    using out_x = call::call_result_t<call::x_of_t, T1>;
    using in_x = call::call_result_t<call::x_of_t, T2>;
    if constexpr (std::is_integral_v<out_x> && !std::is_integral_v<in_x>) {
      // We assume x and y are the same types for both T1 and T2.
      return T1(static_cast<out_x>(call::x_of(p)),
                static_cast<out_x>(call::y_of(p)));
    } else {
      return T1(call::x_of(p), call::y_of(p));
    }
  }
}

constexpr auto square_value(auto &&v) { return v * v; }

template <two_dimensional_coordinate T1, same_unit_geometry_as<T1> T2>
constexpr auto distance_squared(T1 const &p1, T2 const &p2) {
  return square_value(call::x_of(p1) - call::x_of(p2)) +
         square_value(call::y_of(p1) - call::y_of(p2));
}

template <bounding_box TB> class recursive_area_navigator {
  TB relative_area_;
  using x_t = decltype(remove_unit_ref(call::l_x(relative_area_)));
  using y_t = decltype(remove_unit_ref(call::t_y(relative_area_)));
  x_t offset_x_{};
  y_t offset_y_{};

  template <bounding_box A2> friend class recursive_area_navigator;

  constexpr recursive_area_navigator(TB const &b, x_t ox, y_t oy)
      : relative_area_(b), offset_x_(ox), offset_y_(oy) {}

public:
  constexpr explicit recursive_area_navigator(TB const &b)
      : relative_area_(b) {}
  template <typename /*same_unit_geometry_as<TB>*/ TB2 = TB>
  constexpr recursive_area_navigator sub(TB2 const &b) const {
    auto intersection = box_intersection<TB>(b, relative_area_);
    if (valid_box(intersection)) {
      return {nudge_up(nudge_left(intersection, call::l_x(b)), call::t_y(b)),
              offset_x_ + call::l_x(b), offset_y_ + call::t_y(b)};
    } else {
      auto x = call::l_x(relative_area_);
      auto y = call::t_y(relative_area_);
      return {box_from_xyxy<TB>(x, y, x, y), offset_x_, offset_y_};
    }
  }
  constexpr TB relative_area() const { return relative_area_; }
  template <typename /*same_unit_geometry_as<TB>*/ TB2 = TB,
            typename /*pixel_coord*/ C>
  // requires(same_unit_as<C, TB>)
  constexpr TB2 relative_area(TB2 b, C const &rel_point) const {
    return box_from_xywh<TB2>(call::l_x(b) + offset_x_ - call::x_of(rel_point),
                              call::t_y(b) + offset_y_ - call::y_of(rel_point),
                              call::width(b), call::height(b));
  }

  template <rectangle_with_unit<point> TB2 = TB>
  constexpr TB2 move_to_absolute(TB2 const &b) const {
    return box_from_xywh<TB2>(offset_x_ + call::l_x(b).quantity_from_zero(),
                              offset_y_ + call::t_y(b).quantity_from_zero(),
                              call::width(b), call::height(b));
  }
  constexpr TB absolute_area() const {
    return move_to_absolute(relative_area_);
  }

  constexpr auto offset() const {
    return basic_coordinate{offset_x_, offset_y_};
  }

  constexpr nudger<x_t, y_t> relative_to_absolute_nudger() const noexcept {
    return {offset_x_, offset_y_};
  }
  template <typename /*same_unit_geometry_as<TB>*/ A2>
  constexpr explicit operator recursive_area_navigator<A2>() const {
    return {copy_box<A2>(relative_area_), offset_x_, offset_y_};
  }

  template </*pixel_coord*/ typename V>
  // requires(same_unit_as<V, TB>)
  constexpr recursive_area_navigator translate(V const &v) const {
    return {
        nudge_down(nudge_right(relative_area_, call::x_of(v)), call::y_of(v)),
        offset_x_ - call::x_of(v), offset_y_ - call::y_of(v)};
  }
};

template <is_renderer T, rectangle_with_unit<point> TB>
class rendering_context {
public:
  using renderer = T;

private:
  T *c_;
  recursive_area_navigator<TB> area_;
  TB full_area_;
  default_colour_t set_colour_{};

  static constexpr TB bound_area(TB a) {
    if (!valid_box(a)) {
      call::height(a, 0);
      call::width(a, 0);
      assert(valid_box(a));
    }
    return a;
  }

  template <rectangle_with_unit<point> TB2>
  constexpr TB2 to_relative_dest(TB2 const &input_dest) const {
    return box_intersection<TB2>(input_dest, area_.relative_area());
  }
  template <typename TB2>
  constexpr TB2 to_absolute(TB2 const &relative_dest) const {
    return area_.move_to_absolute(relative_dest);
  }

public:
  constexpr rendering_context(T &c, recursive_area_navigator<TB> a,
                              TB const &full_area, default_colour_t sc)
      : c_(std::addressof(c)), area_(a), full_area_(full_area),
        set_colour_(sc) {
    // assert(valid_box(area_.relative_area()));
  }
  constexpr is_int_pixel_rectangle auto
  to_pixel(rectangle_with_unit<point> auto const &b) const {
    return call::to_pixel(*c_, b);
  }

  constexpr rendering_context(T &c, TB a)
      : rendering_context(c, recursive_area_navigator<TB>(a), a, {}) {}
  constexpr explicit rendering_context(T &c)
      : rendering_context(c, call::pixel_area(c)) {}
  template <rectangle_with_unit<point> TB2>
  constexpr rendering_context(T &c, TB2 const &a)
      : rendering_context(c, call::to_pixel(c, a)) {}

  template <typename /*pixel_or_point_rect_basic*/ TB2,
            typename /*pixel_draw_callback*/ TCB>
  constexpr auto draw_pixels(TB2 const &dest, TCB &&cb) const {
    ASP_ASSERT(valid_box(dest.value()));
    auto relative_dest = to_relative_dest(to_pixels(dest));
    ASP_ASSERT(valid_box(relative_dest.value()));
    constexpr auto get_autoconv_dest = [](auto &&dest, auto &&px_scaler) {
      return autoconverting_pixelpoint_unit(dest, call::pixel_scale(px_scaler));
    };
    if (empty_box(relative_dest)) {
      using return_type = decltype((*c_).draw_pixels(
          get_autoconv_dest(relative_dest, *c_), [](auto &&...) {}));
      if constexpr (std::is_void_v<return_type>) {
        return;
      } else {
        return return_type{};
      }
    }
    auto absolute_dest = to_absolute(relative_dest);
    ASP_ASSERT(valid_box(absolute_dest));
    return call::draw_pixels(
        *c_, get_autoconv_dest(absolute_dest, *c_),
        [cb = bp::as_forward(std::forward<decltype(cb)>(cb)), relative_dest,
         nudge = area_.relative_to_absolute_nudger()](auto &&drawer) {
          std::invoke(
              *cb, relative_dest,
              [d = bp::as_forward(std::forward<decltype(drawer)>(drawer)),
               &nudge](/*pixel_coordinate*/ auto &&px, colour auto &&col) {
                auto absolute_pos = nudge(px);
                std::invoke(*d, absolute_pos, col);
              });
        });
  }

  template <typename /*pixel_or_point_rect_basic*/ B, typename F>
  void draw_alpha(B const &b, F &&cb) {
    /*if (empty_box(b)) {
      return;
    }
    using bpix = convert_pixelpoint_t<pixel_size_tag, B>;
    draw_pixels(std::forward<decltype(b)>(b), [this, &cb](bpix const &bbox,
                                                          auto &&drawer) {
      ASP_ASSERT(valid_box(bbox));
      cb(bbox, [this, &drawer](pixel_coordinate auto &&point, auto &&alpha) {
        drawer(point, multiply_alpha(set_colour_, alpha));
      });
    });
     */
  }

  constexpr auto fill(rectangle_with_unit<point> auto const &dest,
                      colour auto const &c) {
    if constexpr (has_native_fill<decltype(*c_), decltype(dest), decltype(c)>) {
      auto absolute_dest = to_absolute(dest);
      return call::fill(*c_, absolute_dest, c);
    } else {
      draw_pixels(dest,
                  fill_on_draw_pixel<std::remove_cvref_t<decltype(c)>>{c});
    }
  }

  constexpr rendering_context sub(rectangle_with_unit<point> auto const &b,
                                  default_colour_t col) const {
    return {*c_, area_.sub(b), col};
  }

  constexpr rendering_context
  sub(rectangle_with_unit<point> auto const &b) const {
    return sub(b, set_colour_);
  }

  /*constexpr rendering_context
  translate(pixel_coordinate auto const &p) const {
    return {*c_, area_.translate(p), set_colour_};
  }
   */
  constexpr rendering_context
  translate(/*point_coordinate*/ auto const &p) const {
    return translate(p);
  }

  constexpr rendering_context with(default_colour_t c) {
    auto res = *this;
    res.set_colour_ = c;
    return res;
  }

  // constexpr TB area() const { return area_.relative_area(); }
  constexpr TB const &widget_full_area() const { return full_area_; }
  constexpr TB widget_redraw_area() const { return area_.relative_area(); }
  constexpr T const &underlying_renderer() const { return *c_; }
};

template <typename T, is_int_pixel_rectangle TB>
rendering_context(T &, TB) -> rendering_context<T, TB>;
template <typename T>
rendering_context(T &t)
    -> rendering_context<T, std::remove_cvref_t<decltype(call::pixel_area(t))>>;
template <typename T, rectangle_with_unit<point> TB>
rendering_context(T &, TB const &)
    -> rendering_context<T,
                         decltype(call::to_pixel(std::declval<T &>(), TB{}))>;

template <is_render_context T>
constexpr is_int_pixel_rectangle auto full_pixel_area(T const &t) {
  return call::to_pixel(t, t.widget_full_area());
}

constexpr is_render_command auto
fill(auto &&r, rectangle_with_unit<point> auto const &area,
     colour auto const &c)
  requires(requires() {
    call::fill(std::forward<decltype(r)>(r), call::to_pixel(r, area), c);
  })
{
  return call::fill(std::forward<decltype(r)>(r), call::to_pixel(r, area), c);
}
ASP_EXPORT_END
} // namespace asp

namespace std {
ASP_EXPORT_BEGIN
template <typename T> struct formatter<asp::basic_colour_t<T>, char> {

  template <class ParseContext>
  constexpr ParseContext::iterator parse(ParseContext &ctx) {
    return ctx.begin();
  }

  template <class FmtContext>
  FmtContext::iterator format(asp::basic_colour_t<T> const &c,
                              FmtContext &ctx) const {
    return format_to(ctx.out(), "[R: {}, G: {}, B: {}, A: {}]", c.red, c.green,
                     c.blue, c.alpha);
  }
};
ASP_EXPORT_END
} // namespace std

#endif

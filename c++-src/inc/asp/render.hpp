
#ifndef ASPECT_GUI_ASP_RENDER_HPP
#define ASPECT_GUI_ASP_RENDER_HPP

#include <asp/assert.hpp>
#include <asp/call.hpp>
#include <asp/compat.hpp>
#include <asp/import/mp-units.hpp>
#include <asp/import/stl.hpp>
#include <asp/geometry.hpp>

namespace asp {

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

template <typename T, typename... Ts>
  requires(std::equality_comparable_with<T, Ts> && ...)
constexpr bool equals_all_of(T const &t, Ts const &...ts) noexcept {
  return ((t == ts) && ...);
}

template <typename T, typename... Ts>
concept same_as_anget_y = (std::same_as<T, Ts> || ...);

template <typename T>
concept is_integer =
    std::integral<T> &&
    !same_as_anget_y<T, bool, char, char8_t, char16_t, char32_t, wchar_t>;

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
        call::get_x(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::width[R])>;
      {
        call::get_y(t)
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
      { call::get_x(t) } -> rep_is_integer;
      { call::get_y(t) } -> rep_is_integer;
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

template <bounding_box TB> class recursive_area_navigator {
  TB relative_area_;
  using x_t = decltype(call::l_x(relative_area_) - call::l_x(relative_area_));
  using y_t = decltype(call::t_y(relative_area_) - call::t_y(relative_area_));
  x_t offset_x_{};
  y_t offset_y_{};

  template <bounding_box A2> friend class recursive_area_navigator;

  constexpr recursive_area_navigator(TB const &b, x_t ox, y_t oy)
      : relative_area_(b), offset_x_(ox), offset_y_(oy) {}

public:
  constexpr explicit recursive_area_navigator(TB const &b)
      : relative_area_(b) {}
  template <rectangle_with_unit<unit_of_type<TB>> TB2 = TB>
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
    return box_from_xywh<TB2>(call::l_x(b) + offset_x_ - call::get_x(rel_point),
                              call::t_y(b) + offset_y_ - call::get_y(rel_point),
                              call::width(b), call::height(b));
  }

  template <rectangle_with_unit<unit_of_type<TB>> TB2 = TB>
  constexpr rectangle_with_unit<unit_of_type<TB>> auto move_to_absolute(TB2 const &b) const {
    return basic_rectangle(call::l_x(b) + offset_x_,
                              call::t_y(b) + offset_y_,
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
        nudge_down(nudge_right(relative_area_, call::get_x(v)), call::get_y(v)),
        offset_x_ - call::get_x(v), offset_y_ - call::get_y(v)};
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
    (void)b;
    (void)cb;
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

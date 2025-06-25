
#include <concepts>
#include <print>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>

#include <gmock/gmock.h>

#include <mp-units/concepts.h>
#include <mp-units/framework.h>
#include <mp-units/math.h>
#include <mp-units/systems/isq.h>

#include <asp/call.hpp>
#include <asp/types.hpp>

// import mp_units;

namespace asp::tests {

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

template <mp_units::Reference auto R, typename Rep> struct basic_rectangle {
  static constexpr auto reference = R;
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
template <typename... Ts>
concept all_is_floats = (std::floating_point<Ts> && ...);

template <typename... Ts>
concept all_is_either_integers_or_floats =
    all_is_integers<Ts...> || all_is_floats<Ts...>;

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

template <typename T>
concept has_unit = requires() { std::remove_cvref_t<T>::unit; };
template <typename T, auto U>
concept has_unit_of = has_unit<T> && std::remove_cvref_t<T>::unit == U;
template <typename Q, auto R>
concept is_quantity =
    mp_units::Reference<decltype(R)> &&
    mp_units::QuantityOf<std::remove_cvref_t<Q>, get_quantity_spec(R)> &&
    has_unit_of<Q, get_unit(R)>;
template <typename QP, auto R>
concept is_quantity_point =
    mp_units::Reference<decltype(R)> &&
    mp_units::QuantityPointOf<std::remove_cvref_t<QP>, get_quantity_spec(R)> &&
    has_unit_of<QP, get_unit(R)>;

#define ASP_NO_CONST(X)                                                        \
  decltype(X) {}

template <typename T, typename... Args>
concept direct_invocable = requires(T &&t, Args &&...args) {
  std::forward<T>(t)(std::forward<Args>(args)...);
};

template <typename T, auto... Constraints>
concept satisfy_all =
#if 1
    (requires(T &&t) { decltype(Constraints){}(std::forward<T>(t)); } && ...);
#else
    (direct_invocable<decltype(Constraints), T> && ...);
#endif

template <typename T, auto... Constraints>
concept is_quantity_point_satisfying =
    mp_units::QuantityPoint<std::remove_cvref_t<T>> &&
    satisfy_all<T, Constraints...>;

template <typename T, auto... Constraints>
concept is_quantity_satisfying = mp_units::Quantity<std::remove_cvref_t<T>> &&
                                 satisfy_all<T, Constraints...>;
template <typename T>
concept has_rep = requires() { typename std::remove_cvref_t<T>::rep; };
template <typename T, auto... Constraints>
concept rep_satisfy =
    has_rep<T> && (direct_invocable<decltype(Constraints),
                                    typename std::remove_cvref_t<T>::rep> &&
                   ...);

template <typename T, auto... Constraints>
concept is_rectangle_satisfying =
    bounding_box<std::remove_cvref_t<T>> && requires(T &&t) {
      { call::l_x(t) } -> is_quantity_point_satisfying<Constraints...>;
      { call::t_y(t) } -> is_quantity_point_satisfying<Constraints...>;
      { call::width(t) } -> is_quantity_satisfying<Constraints...>;
      { call::height(t) } -> is_quantity_satisfying<Constraints...>;
    };

template <typename T, auto R>
concept rectangle_with_unit =
    bounding_box<std::remove_cvref_t<T>> && requires(T &&t) {
      {
        call::l_x(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::width[R])>;
      {
        call::t_y(t)
      } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::height[R])>;
      { call::width(t) } -> is_quantity<ASP_NO_CONST(mp_units::isq::width[R])>;
      {
        call::height(t)
      } -> is_quantity<ASP_NO_CONST(mp_units::isq::height[R])>;
    };

template <typename T>
concept rep_is_integer =
    has_rep<T> && is_integer<typename std::remove_cvref_t<T>::rep>;

template <typename T>
concept is_rectangle_with_integer_rep =
    bounding_box<std::remove_cvref_t<T>> && requires(T &&t) {
      { call::l_x(t) } -> rep_is_integer;
      { call::t_y(t) } -> rep_is_integer;
      { call::width(t) } -> rep_is_integer;
      { call::height(t) } -> rep_is_integer;
    };
template <typename T>
concept is_int_pixel_rectangle =
    is_rectangle_with_integer_rep<T> && rectangle_with_unit<T, pixel>;

static_assert(is_quantity_point<
              mp_units::quantity_point<decltype(mp_units::isq::width[pixel]){}>,
              decltype(mp_units::isq::width[pixel]){}>);
static_assert(rectangle_with_unit<basic_rectangle<pixel, int>, pixel>);
// static_assert(has_unit_of<basic_rectangle<point, float>, point>);
static_assert(
    has_unit_of<mp_units::quantity_point<mp_units::isq::width[pixel]>, pixel>);
static_assert(rectangle_with_unit<basic_rectangle<point, float>, point>);

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

constexpr fill_rect_command auto fill(is_render_context auto &&ctx,
                                      colour auto const &c) {
  return call::fill(ctx, ctx.widget_full_area(), c);
}

template <typename T>
using context_renderer_t = typename std::remove_cvref_t<T>::renderer;

template <bounding_box TB = default_rect> class recursive_area_navigator {
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
  template <same_unit_geometry_as<TB> TB2 = TB>
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
  template <same_unit_geometry_as<TB> TB2 = TB, pixel_coord C>
    requires(same_unit_as<C, TB>)
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

  constexpr default_coordinate offset() const
    requires(!size_tagged<TB>)
  {
    return {offset_x_, offset_y_};
  }
  template <typename TB2 = TB, typename SizeTag = tag_t_of<TB2>,
            typename ResultT = pixelpoint_unit<SizeTag, default_coordinate>>
    requires(size_tagged<TB>)
  constexpr ResultT offset() const {
    return ResultT(offset_x_, offset_y_);
  }

  constexpr nudger<x_t, y_t> relative_to_absolute_nudger() const noexcept {
    return {offset_x_, offset_y_};
  }
  template <same_unit_geometry_as<TB> A2>
  constexpr explicit operator recursive_area_navigator<A2>() const {
    return {copy_box<A2>(relative_area_), offset_x_, offset_y_};
  }

  template <pixel_coord V>
    requires(same_unit_as<V, TB>)
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
    assert(valid_box(area_.relative_area()));
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

  template <pixel_or_point_rect_basic TB2, pixel_draw_callback TCB>
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
               &nudge](pixel_coordinate auto &&px, colour auto &&col) {
                auto absolute_pos = nudge(px);
                std::invoke(*d, absolute_pos, col);
              });
        });
  }

  template <pixel_or_point_rect_basic B, typename F>
  void draw_alpha(B const &b, F &&cb) {
    if (empty_box(b)) {
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

  constexpr rendering_context
  partial_redraw(rectangle_with_unit<point> auto const &area) {
    return {*c_, area_, area, set_colour_};
  }

  constexpr rendering_context sub(pixel_rect auto &&b,
                                      default_colour_t col) const {
    return {*c_, area_.sub(b), col};
  }

  constexpr rendering_context sub(pixel_rect auto &&b) const {
    return sub(b, set_colour_);
  }

  constexpr rendering_context sub(point_rect auto const &b,
                                      auto &&...args) const {
    return sub(to_pixels(b), std::forward<decltype(args)>(args)...);
  }

  constexpr rendering_context
  translate(pixel_coordinate auto const &p) const {
    return {*c_, area_.translate(p), set_colour_};
  }
  constexpr rendering_context
  translate(point_coordinate auto const &p) const {
    return translate(to_pixels(p));
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
rendering_context(T &t) -> rendering_context<
    T, std::remove_cvref_t<decltype(call::pixel_area(t))>>;
template <typename T, rectangle_with_unit<point> TB>
rendering_context(T &, TB const &)
    -> rendering_context<T, decltype(call::to_pixel(std::declval<T &>(),
                                                        TB{}))>;

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

constexpr default_colour_t straight_alpha_blend(default_colour_t const &fg,
                                                default_colour_t const &bg) {
  auto norm_bg_alpha = (call::alpha(bg) * (255 - call::alpha(fg))) / 255;
  assert(norm_bg_alpha < 256);
  assert(norm_bg_alpha >= 0);
  auto new_alpha =
      static_cast<std::uint_least8_t>(call::alpha(fg) + norm_bg_alpha);
  auto calculate_colour = [&fg, &bg, new_alpha,
                           norm_bg_alpha](auto colour_get) {
    auto fgc = colour_get(fg);
    auto bgc = colour_get(bg);
    return new_alpha == 0 ? std::uint_least8_t{}
                          : static_cast<std::uint_least8_t>(
                                (fgc * call::alpha(fg) + bgc * norm_bg_alpha +
                                 new_alpha / 2) /
                                (new_alpha));
  };
  return {calculate_colour(call::red), calculate_colour(call::green),
          calculate_colour(call::blue), new_alpha};
}

struct stub_renderer {
  mp_units::quantity<pixel_per_point, float> px_p_pt = 1.f * pixel_per_point;
  class executor {
    std::make_signed_t<std::size_t> columns_{1};
    std::make_signed_t<std::size_t> rows_{1};
    std::vector<default_colour_t> raw_results_ = {{}};

    constexpr std::make_signed_t<std::size_t> calc_size() const noexcept {
      return columns_ * rows_;
    }

  public:
    constexpr executor(mp_units::quantity<pixel_width, int> width,
                       mp_units::quantity<pixel_height, int> height)
        : columns_(width.numerical_value_in(pixel)),
          rows_(height.numerical_value_in(pixel)), raw_results_(calc_size()) {}

    constexpr default_colour_t const &
    operator[](is_quantity<pixel> auto const &x,
               is_quantity<pixel> auto const &y) const & noexcept {
      auto i =
          columns_ * y.numerical_value_in(pixel) + x.numerical_value_in(pixel);
      return raw_results_.at(i);
    }
    constexpr default_colour_t &
    operator[](is_quantity<pixel> auto const &x,
               is_quantity<pixel> auto const &y) & noexcept {
      auto i =
          columns_ * y.numerical_value_in(pixel) + x.numerical_value_in(pixel);
      return raw_results_.at(i);
    }
    constexpr std::size_t size() const noexcept { return raw_results_.size(); }
    constexpr std::size_t extend(std::size_t e) const noexcept {
      switch (e) {
      case 0:
        return columns_;
      case 1:
        return rows_;
      default:
        return 0u;
      }
    }
    constexpr std::span<default_colour_t const> pixel_span() const noexcept {
      return raw_results_;
    }
    constexpr default_colour_t const &front() {
      assert(!empty(raw_results_));
      return raw_results_.front();
    }
  };
  struct cached_fill_rect {
    using associated_renderer = stub_renderer;
    basic_rectangle<pixel, std::int_least32_t> area;
    default_colour_t c;

    constexpr basic_rectangle<pixel, std::int_least32_t>
    pixel_area() const noexcept {
      return area;
    }
    constexpr default_colour_t colour() const noexcept { return c; }
  };

  static constexpr cached_fill_rect
  fill(basic_rectangle<pixel, std::int_least32_t> const &area,
       default_colour_t colour) {
    return {area, colour};
  }
  constexpr mp_units::Quantity auto pixel_to_point_ratio() const {
    return px_p_pt;
  }
  constexpr executor
  executing_renderer(basic_rectangle<pixel, int> const &r) const {
    return {call::width(r), call::height(r)};
  }
  constexpr executor executing_renderer() const {
    return executing_renderer({{}, {}, 1 * pixel_width, 1 * pixel_height});
  }
  constexpr basic_rectangle<pixel, int>
  to_pixel(rectangle_with_unit<point> auto const &rect) const {
    auto to_pixel_impl = [this]<typename S, typename Pnt>(this S &&self,
                                                          Pnt const &point) {
      if constexpr (mp_units::Quantity<Pnt>) {
        auto pix_val = point * pixel_to_point_ratio();
        return static_cast<int>(
                   std::lround(pix_val.numerical_value_in(pixel))) *
               pixel;
      } else {
        auto pix_val = self(point.quantity_from_zero());
        return mp_units::quantity_point(pix_val, point.point_origin);
      }
    };
    return {to_pixel_impl(call::l_x(rect)), to_pixel_impl(call::t_y(rect)),
            to_pixel_impl(call::width(rect)),
            to_pixel_impl(call::height(rect))};
  }
};

constexpr void execute(stub_renderer::cached_fill_rect const &cmd,
                       stub_renderer::executor &r) {
  auto a = cmd.pixel_area();

  for (auto y : std::views::iota(
           call::t_y(a).quantity_from_zero().numerical_value_in(pixel),
           call::b_y(a).quantity_from_zero().numerical_value_in(pixel))) {
    for (auto x : std::views::iota(
             call::l_x(a).quantity_from_zero().numerical_value_in(pixel),
             call::r_x(a).quantity_from_zero().numerical_value_in(pixel))) {
      auto &cur = r[x * pixel_width, y * pixel_height];
      cur = straight_alpha_blend(cmd.colour(), cur);
    }
  }
}

static_assert(requires(stub_renderer const &sr,
                       basic_rectangle<point, float> const &rect) {
  sr.to_pixel(rect);
});
static_assert(is_renderer<stub_renderer>);

template <colour C> class fill_rectangle {
  C c_;

public:
  constexpr explicit fill_rectangle(C colour) : c_(colour) {}
  friend constexpr fill_rect_command auto
  initial_render_cache(fill_rectangle const &fr, is_render_context auto &&ctx) {
    return call::fill(ctx, fr.c_);
  }
  template <fill_rect_command CMD>
  constexpr std::remove_cvref_t<CMD> render(is_render_context auto &&ctx,
                                            CMD &&c) {
    if (c.pixel_area() != full_pixel_area(ctx)) {
      return initial_render_cache(*this, ctx);
    }
    return std::forward<CMD>(c);
  }
};

template <typename T>
concept is_widget = true;

template <is_render_command FGC, is_render_command BGC>
class background_render_cache_t {
  FGC foreground_;
  BGC background_;

public:
  using associated_renderer = associated_renderer_t<FGC>;
  constexpr background_render_cache_t(std::convertible_to<FGC> auto &&fg,
                                      std::convertible_to<BGC> auto &&bg)
      : foreground_(std::forward<decltype(fg)>(fg)),
        background_(std::forward<decltype(bg)>(bg)) {}
  constexpr auto execute(auto &&exe_renderer) const {
    call::execute(background_, exe_renderer);
    call::execute(foreground_, exe_renderer);
  }
};
template <is_render_command F, is_render_command B>
background_render_cache_t(F &&, B &&)
    -> background_render_cache_t<std::remove_cvref_t<F>,
                                 std::remove_cvref_t<B>>;

template <is_widget FG, is_widget BG> class background_t {
  FG fg_;
  BG bg_;

public:
  constexpr background_t(std::convertible_to<FG> auto &&fg,
                         std::convertible_to<BG> auto &&bg)
      : fg_(std::forward<decltype(fg)>(fg)),
        bg_(std::forward<decltype(bg)>(bg)) {}

  friend constexpr is_render_command auto
  initial_render_cache(background_t const &self, is_render_context auto &&ctx) {
    return background_render_cache_t{call::initial_render_cache(self.fg_, ctx),
                                     call::initial_render_cache(self.bg_, ctx)};
  }
};

template <is_widget F, is_widget B>
background_t(F &&, B &&)
    -> background_t<std::remove_cvref_t<F>, std::remove_cvref_t<B>>;

template <auto call, typename... Ts> class pipe_to_invoke {
  std::tuple<Ts...> args_;

public:
  constexpr explicit(sizeof...(Ts) == 1)
      pipe_to_invoke(std::convertible_to<Ts> auto &&...args)
      : args_(std::forward<decltype(args)>(args)...) {}
  template <typename T, auto c, typename... Us>
    requires(std::invocable<decltype(c), T, Us...>)
  friend constexpr std::invoke_result_t<decltype(c), T, Us...>
  operator|(T &&, pipe_to_invoke<c, Us...> &&);
  template <typename T, auto c, typename... Us>
    requires(std::invocable<decltype(c), T, Us...>)
  friend constexpr std::invoke_result_t<decltype(c), T, Us...>
  operator|(T &&, pipe_to_invoke<c, Us...> const &);
};

template <typename T, auto c, typename... Us>
  requires(std::invocable<decltype(c), T, Us...>)
constexpr std::invoke_result_t<decltype(c), T, Us...>
operator|(T &&t, pipe_to_invoke<c, Us...> &&v) {
#if __cpp_structured_bindings >= 202411L
  auto &&[args...] = std::move(v).args_;
  return c(std::forward<T>(t), std::forward<decltype(args)>(args)...);
#else
  return std::apply(
      [&t](Us &&...vs) {
        return c(std::forward<T>(t), std::forward<Us>(vs)...);
      },
      std::move(v).args_);
#endif
}
template <typename T, auto c, typename... Us>
  requires(std::invocable<decltype(c), T, Us...>)
constexpr std::invoke_result_t<decltype(c), T, Us...>
operator|(T &&t, pipe_to_invoke<c, Us...> const &v) {
#if __cpp_structured_bindings >= 202411L
  auto &&[args...] = v.args_;
  return c(std::forward<T>(t), std::forward<decltype(args)>(args)...);
#else
  return std::apply(
      [&t]<typename... Ts>(Ts &&...vs) {
        return c(std::forward<T>(t), std::forward<Ts>(vs)...);
      },
      v.args_);
#endif
}

inline constexpr auto background(is_widget auto &&bg) {
  using bg_t = decltype(bg);
  return pipe_to_invoke<[]<is_widget FG, is_widget BG>(FG &&fg, BG &&bg) {
    return background_t(std::forward<FG>(fg), std::forward<BG>(bg));
  },
                        std::remove_cvref_t<bg_t>>(std::forward<bg_t>(bg));
}

using namespace ::testing;

TEST(StraightAlphaBlendUInt8,
     BackgroundStaysWhenForegroundIsInvisible) // NOLINT
{
  auto res = straight_alpha_blend(default_colour_t{1, 2, 3, 0},
                                  default_colour_t{4, 5, 6, 255});
  EXPECT_THAT(res, Eq(default_colour_t{4, 5, 6, 255}));
}
TEST(StraightAlphaBlendUInt8, ForegroundTakesOverWhenFullyOpaque) // NOLINT
{
  auto res = straight_alpha_blend(default_colour_t{1, 2, 3, 255},
                                  default_colour_t{4, 5, 6, 255});
  EXPECT_THAT(res, Eq(default_colour_t{1, 2, 3, 255}));
}
TEST(StraightAlphaBlendUInt8, Foreground25PercentVisible) // NOLINT
{
  auto res = straight_alpha_blend(default_colour_t{64, 128, 0, 64},
                                  default_colour_t{128, 0, 64, 255});
  EXPECT_THAT(res, Eq(default_colour_t{96 + 16, 32, 48, 255}));
}
TEST(StraightAlphaBlendUInt8, Foreground75PercentVisible) // NOLINT
{
  auto res = straight_alpha_blend(default_colour_t{64, 128, 0, 192},
                                  default_colour_t{128, 0, 64, 255});
  EXPECT_THAT(res, Eq(default_colour_t{32 + 48, 96, 16, 255}));
}

TEST(StubRenderer, CreateRendererWithSize2x1) // NOLINT
{
  auto r = stub_renderer{};
  auto exe = r.executing_renderer(
      basic_rectangle<pixel, int>({}, {}, 2 * pixel_width, 1 * pixel_height));
  EXPECT_THAT(exe.extend(0), Eq(2));
}

TEST(StubRenderer, FillRectApplyToSinglePixel) // NOLINT
{
  auto r = stub_renderer{};
  auto rect =
      basic_rectangle<point, int>({}, {}, 1 * point_width, 1 * point_height);
  auto fr = call::fill(r, rect, default_colour_t{1, 2, 3, 255});
  auto exe = r.executing_renderer(
      basic_rectangle<pixel, int>({}, {}, 1 * pixel_width, 1 * pixel_height));
  call::execute(fr, exe);
  EXPECT_THAT(exe.size(), Eq(1));
  EXPECT_THAT(
      (exe[0 * mp_units::isq::width[pixel], 0 * mp_units::isq::height[pixel]]),
      Eq(default_colour_t{1, 2, 3, 255}));
}
TEST(StubRenderer, TransparentOverOpaqueBlend) // NOLINT
{
  auto r = stub_renderer{};
  auto rect =
      basic_rectangle<point, int>({}, {}, 1 * point_width, 1 * point_height);
  auto background = call::fill(r, rect, default_colour_t{2, 0, 4, 255});
  auto foreground = call::fill(r, rect, default_colour_t{0, 2, 4, 128});
  auto exe = r.executing_renderer(lround(rect * 1 * pixel_per_point));
  call::execute(background, exe);
  call::execute(foreground, exe);
  EXPECT_THAT(exe.front(), Eq(default_colour_t{1, 1, 4, 255}));
}

TEST(FillRect, InitialRenderCacheFillsWithCorrectColour) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(255, 0, 0, 127));
  auto renderer = stub_renderer{};
  auto rect =
      basic_rectangle<point, float>({}, {}, 1 * point_width, 1 * point_height);
  auto cache = initial_render_cache(fr, rendering_context(renderer, rect));
  EXPECT_THAT(cache.colour().red, Eq(255));
  EXPECT_THAT(cache.colour().green, Eq(0));
  EXPECT_THAT(cache.colour().blue, Eq(0));
  EXPECT_THAT(cache.colour().alpha, Eq(127));
}

TEST(FillRect, InitialRenderCacheFillsWithCorrectArea) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(255, 0, 0, 127));
  auto renderer = stub_renderer{};
  auto rect = basic_rectangle<point, int>(mp_units::absolute<point_width>(1),
                                          mp_units::absolute<point_height>(5),
                                          2 * point_width, 3 * point_height);
  auto cache = initial_render_cache(fr, rendering_context(renderer, rect));
  EXPECT_THAT(cache.area.l_x(), Eq(mp_units::absolute<pixel_width>(1)));
  EXPECT_THAT(cache.area.t_y(), Eq(mp_units::absolute<pixel_height>(5)));
  EXPECT_THAT(cache.area.width(), Eq(2 * pixel_width));
  EXPECT_THAT(cache.area.height(), Eq(3 * pixel_height));
}

TEST(FillRect, NewRenderCacheIsUpdatedWhenSizeChanged) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(0, 255, 0, 127));
  auto renderer = stub_renderer{};
  auto init_rect = basic_rectangle<point, int>(
      mp_units::absolute<point_width>(1), mp_units::absolute<point_height>(5),
      2 * point_width, 3 * point_height);
  auto cache =
      initial_render_cache(fr, rendering_context(renderer, init_rect));
  auto new_rect = basic_rectangle<point, int>(
      mp_units::absolute<point_width>(3), mp_units::absolute<point_height>(2),
      3 * point_width, 7 * point_height);
  cache = call::render(fr, rendering_context(renderer, new_rect),
                       std::move(cache));
  EXPECT_THAT(cache.area.l_x(), Eq(mp_units::absolute<pixel_width>(3)));
  EXPECT_THAT(cache.area.t_y(), Eq(mp_units::absolute<pixel_height>(2)));
  EXPECT_THAT(cache.area.width(), Eq(3 * pixel_width));
  EXPECT_THAT(cache.area.height(), Eq(7 * pixel_height));
}
TEST(FillRect, NewRenderCacheIsUpdatedWhenColourChanged) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(0, 255, 0, 127));
  auto renderer = stub_renderer{};
  auto rect = basic_rectangle<point, int>(mp_units::absolute<point_width>(1),
                                          mp_units::absolute<point_height>(5),
                                          2 * point_width, 3 * point_height);
  auto cache = initial_render_cache(fr, rendering_context(renderer, rect));
  fr = fill_rectangle(default_colour_t(0, 0, 255, 127));
  cache =
      call::render(fr, rendering_context(renderer, rect), std::move(cache));
  EXPECT_THAT(cache.colour().red, Eq(0));
  EXPECT_THAT(cache.colour().blue, Eq(0));
  EXPECT_THAT(cache.colour().green, Eq(255));
  EXPECT_THAT(cache.colour().alpha, Eq(127));
}
template <colour T> constexpr bool colour_equal(T const &l, T const &r) {
  auto field_equal = [&l, &r](auto field_get) {
    return field_get(l) == field_get(r);
  };
  using field_t = std::remove_cvref_t<decltype(call::alpha(l))>;
  return field_equal(call::alpha) &&
         (call::alpha(l) == field_t{} ||
          (field_equal(call::red) && field_equal(call::green) &&
           field_equal(call::blue)));
}
MATCHER_P(ColourEq, exp, "") {
  if (result_listener != nullptr && result_listener->stream() != nullptr) {
    std::print(*result_listener->stream(),
               "Expected colour to be {}, actual was {}", exp, arg);
  }
  return colour_equal(arg, exp);
}
TEST(FillRectBackground, FillRectAndFillRect) // NOLINT
{
  auto c = fill_rectangle(default_colour_t(255, 0, 0, 255)) |
           background(fill_rectangle(default_colour_t{0, 255, 0, 255}));
  auto renderer = stub_renderer{};
  auto rect = basic_rectangle<point, int>(mp_units::absolute<point_width>(0),
                                          mp_units::absolute<point_height>(0),
                                          2 * point_width, 3 * point_height);
  auto cache = initial_render_cache(c, rendering_context(renderer, rect));
  auto exe_r = renderer.executing_renderer(
      basic_rectangle<pixel, int>({}, {}, 2 * pixel_width, 3 * pixel_height));
  call::execute(cache, exe_r);
  EXPECT_THAT(exe_r.pixel_span(),
              Each(ColourEq(default_colour_t{255, 0, 0, 255})));
}

struct no_op_widget {
  template <typename R> struct render_command_t {
    using associated_renderer = R;
    constexpr void execute(auto &&...) const {}
  };
  constexpr auto initial_render_cache(is_render_context auto &&ctx) const
      -> render_command_t<context_renderer_t<decltype(ctx)>> {
    return {};
  }
};

TEST(FillRectBackground, DummyForegroundWillDisplayBackground) {
  auto c = no_op_widget() |
           background(fill_rectangle(default_colour_t{4, 7, 15, 255}));
  auto renderer = stub_renderer{};
  auto rect = basic_rectangle<point, int>(mp_units::absolute<point_width>(0),
                                          mp_units::absolute<point_height>(0),
                                          1 * point_width, 1 * point_height);
  (void)no_op_widget().initial_render_cache(
      rendering_context(renderer, rect));
  auto cache = initial_render_cache(c, rendering_context(renderer, rect));
  auto exe_r = renderer.executing_renderer(
      basic_rectangle<pixel, int>({}, {}, 1 * pixel_width, 1 * pixel_height));
  call::execute(cache, exe_r);
  EXPECT_THAT(exe_r.pixel_span(),
              Each(ColourEq(default_colour_t{4, 7, 15, 255})));
}
} // namespace asp::tests

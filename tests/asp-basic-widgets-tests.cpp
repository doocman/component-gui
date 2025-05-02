
#include <concepts>
#include <type_traits>
#include <utility>

#include <gmock/gmock.h>

#include <asp/call.hpp>
#include <asp/types.hpp>

import mp_units;

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

template <mp_units::Reference auto R, typename Rep = float>
struct basic_rectangle {
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

template <mp_units::QuantityPoint QX, mp_units::QuantityPoint QY,
          mp_units::Quantity W, mp_units::Quantity H>
  requires(equals_all_of(QX::unit, QY::unit, W::unit, H::unit))
basic_rectangle(QX, QY, W, H) -> basic_rectangle<QX::unit>;

template <mp_units::Reference auto R, typename Rep, typename... Ts>
  requires(requires(Ts &&...args) {
    basic_rectangle(std::forward<Ts>(args)...);
  })
constexpr bounding_box auto deduce_alternative(basic_rectangle<R, Rep> const &,
                                               Ts &&...args) {
  return basic_rectangle(std::forward<Ts>(args)...);
}

// TODO: Make this generic, only the 'deduce_alternative'-function must exist
// explicitly for each type-instance.
template <mp_units::Reference auto R, typename Rep, mp_units::Quantity Q>
constexpr auto linear_map(basic_rectangle<R, Rep> const &v,
                          Q const &mapping_factor) {
  return deduce_alternative(
      v,
      mp_units::quantity_point(v.l_x().quantity_from_zero() * mapping_factor,
                               v.l_x().point_origin),
      mp_units::quantity_point(v.t_y().quantity_from_zero() * mapping_factor,
                               v.t_y().point_origin),
      v.width() * mapping_factor, v.height() * mapping_factor);
}

template <typename Q, auto R>
concept is_quantity =
    mp_units::Reference<decltype(R)> &&
    mp_units::QuantityOf<std::remove_cvref_t<Q>, get_quantity_spec(R)> &&
    (std::remove_cvref_t<Q>::unit == get_unit(R));
template <typename QP, auto R>
concept is_quantity_point =
    mp_units::Reference<decltype(R)> &&
    mp_units::QuantityPointOf<std::remove_cvref_t<QP>, get_quantity_spec(R)> &&
    (std::remove_cvref_t<QP>::unit == get_unit(R));

template <typename T, auto R>
concept rectangle_with_unit = bounding_box<T> && requires(T &&t) {
  { call::l_x(t) } -> is_quantity_point<mp_units::isq::width[R]>;
  { call::t_y(t) } -> is_quantity_point<mp_units::isq::height[R]>;
  { call::width(t) } -> is_quantity<mp_units::isq::width[R]>;
  { call::height(t) } -> is_quantity<mp_units::isq::height[R]>;
};

struct stub_renderer {
  struct cached_fill_rect {
    basic_rectangle<pixel> area;
    default_colour_t colour;
  };

  static constexpr cached_fill_rect fill(basic_rectangle<pixel> const &area,
                                         default_colour_t colour) {
    return {area, colour};
  }
};

constexpr rectangle_with_unit<pixel> auto
to_pixel_rectangle(stub_renderer const &,
                   rectangle_with_unit<point> auto const &pnt_rect) {
  return linear_map(pnt_rect, 1 * pixel_per_point);
}

template <typename Renderer, typename Area> class simple_display_context {
  Renderer r_;
  Area a_;

public:
  constexpr simple_display_context(auto &&r, auto &&a)
      : r_(std::forward<decltype(r)>(r)), a_(a) {}
  constexpr Renderer &renderer() { return r_; }

  rectangle_with_unit<pixel> auto pixel_area() const noexcept {
    if constexpr (rectangle_with_unit<Area, pixel>) {
      return a_;
    } else {
      return to_pixel_rectangle(r_, a_);
    }
  }
};
template <typename R, typename A>
simple_display_context(R &&, A &&)
    -> simple_display_context<std::unwrap_ref_decay_t<R>,
                              std::unwrap_ref_decay_t<A>>;

template <colour C> class fill_rectangle {
  C c_;

public:
  template <typename CMD> struct render_cache_t {
    CMD command;
  };
  constexpr explicit fill_rectangle(C colour) : c_(colour) {}
  friend constexpr auto initial_render_cache(fill_rectangle const &fr,
                                             auto &&ctx) {
    return render_cache_t{ctx.renderer().fill(ctx.pixel_area(), fr.c_)};
  }
};

using namespace ::testing;

static_assert(rectangle_with_unit<basic_rectangle<pixel>, pixel>);

TEST(FillRect, InitialRenderCacheFillsWithCorrectColour) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(255, 0, 0, 127));
  auto renderer = stub_renderer{};
  auto rect = basic_rectangle<point>({}, {}, 1 * point_width, 1 * point_height);
  auto cache = initial_render_cache(fr, simple_display_context(renderer, rect));
  EXPECT_THAT(cache.command.colour.red, Eq(255));
  EXPECT_THAT(cache.command.colour.green, Eq(0));
  EXPECT_THAT(cache.command.colour.blue, Eq(0));
  EXPECT_THAT(cache.command.colour.alpha, Eq(127));
}

TEST(FillRect, InitialRenderCacheFillsWithCorrectArea) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(255, 0, 0, 127));
  auto renderer = stub_renderer{};
  auto rect = basic_rectangle<point>(mp_units::absolute<point_width>(1),
                                     mp_units::absolute<point_height>(5),
                                     2 * point_width, 3 * point_height);
  auto cache = initial_render_cache(fr, simple_display_context(renderer, rect));
  EXPECT_THAT(cache.command.area.l_x(), Eq(mp_units::absolute<pixel_width>(1)));
  EXPECT_THAT(cache.command.area.t_y(),
              Eq(mp_units::absolute<pixel_height>(5)));
  EXPECT_THAT(cache.command.area.width(), Eq(2 * pixel_width));
  EXPECT_THAT(cache.command.area.height(), Eq(3 * pixel_height));
}
} // namespace asp::tests

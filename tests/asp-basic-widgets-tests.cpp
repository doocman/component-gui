
#include <utility>

#include <asp/types.hpp>
#include <gmock/gmock.h>

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
  mp_units::quantity_point<mp_units::isq::width[R],
                           default_point_origin(mp_units::isq::width[R]), Rep>
      left_x;
  mp_units::quantity_point<mp_units::isq::height[R],
                           default_point_origin(mp_units::isq::height[R]), Rep>
      top_y;
  mp_units::quantity<mp_units::isq::width[R], Rep> width;
  mp_units::quantity<mp_units::isq::height[R], Rep> height;
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
template <typename Renderer> class simple_display_context {
  Renderer r_;

public:
  constexpr simple_display_context(auto &&r, auto &&)
      : r_(std::forward<decltype(r)>(r)) {}
  constexpr Renderer &renderer() { return r_; }
};
template <typename R, typename A>
simple_display_context(R &&, A &&)
    -> simple_display_context<std::unwrap_ref_decay_t<R>>;

template <colour C> class fill_rectangle {
  C c_;

public:
  template <typename CMD> struct render_cache_t {
    CMD command;
  };
  constexpr explicit fill_rectangle(C colour) : c_(colour) {}
  friend constexpr auto initial_render_cache(fill_rectangle const &fr,
                                             auto &&ctx) {
    return render_cache_t{ctx.renderer().fill(
        basic_rectangle<pixel>({}, {}, 1 * pixel, 1 * pixel), fr.c_)};
  }
};

using namespace ::testing;

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
  auto rect = basic_rectangle<point>(mp_units::absolute<point_width>(1), mp_units::absolute<point_height>(5), 2 * point_width, 3 * point_height);
  auto cache = initial_render_cache(fr, simple_display_context(renderer, rect));
  EXPECT_THAT(cache.command.area.left_x, Eq(mp_units::absolute<pixel_width>(1)));
  EXPECT_THAT(cache.command.area.top_y, Eq(mp_units::absolute<pixel_height>(5)));
  EXPECT_THAT(cache.command.area.width, Eq(2 * pixel_width));
  EXPECT_THAT(cache.command.area.height, Eq(3 * pixel_height));
}
} // namespace asp::tests

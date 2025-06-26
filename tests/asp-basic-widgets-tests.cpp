
#include <concepts>
#include <print>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>

#include <gmock/gmock.h>

//#include <asp/import/mp-units.hpp>

import aspect_gui;

//#include <asp/call.hpp>
//#include <asp/types.hpp>

// import mp_units;

namespace asp::tests {

static_assert(is_quantity_point<
              mp_units::quantity_point<decltype(mp_units::isq::width[pixel]){}>,
              decltype(mp_units::isq::width[pixel]){}>);
static_assert(rectangle_with_unit<basic_rectangle<pixel, int>, pixel>);
// static_assert(has_unit_of<basic_rectangle<point, float>, point>);
static_assert(
    has_unit_of<mp_units::quantity_point<mp_units::isq::width[pixel]>, pixel>);
static_assert(rectangle_with_unit<basic_rectangle<point, float>, point>);

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


#include <concepts>
#include <tuple>
#include <type_traits>
#include <utility>
#include <ranges>

#include <gmock/gmock.h>

#include <mp-units/concepts.h>
#include <mp-units/framework.h>
#include <mp-units/math.h>
#include <mp-units/systems/isq.h>

#include <asp/call.hpp>
#include <asp/types.hpp>

//import mp_units;

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

template <mp_units::Reference auto R, typename Rep>
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
concept is_integer = std::integral<T> && !same_as_any_of<T, bool, char, char8_t, char16_t, char32_t, wchar_t>;

template <typename... Ts>
concept all_is_integers = (is_integer<Ts> && ...);
template <typename... Ts>
concept all_is_floats = (std::floating_point<Ts> && ...);

template <typename... Ts>
concept all_is_either_integers_or_flaots = all_is_integers<Ts...> || all_is_floats<Ts...>;

template <mp_units::QuantityPoint QX, mp_units::QuantityPoint QY,
          mp_units::Quantity W, mp_units::Quantity H>
  requires(equals_all_of(QX::unit, QY::unit, W::unit, H::unit) && all_is_integers<typename QX::rep,typename QY::rep,typename W::rep,typename H::rep>)
basic_rectangle(QX, QY, W, H) -> basic_rectangle<QX::unit, std::common_type_t<typename QX::rep,typename  QY::rep,typename  W::rep, typename H::rep>>;

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

template <typename T>
concept has_unit = requires() { std::remove_cvref_t<T>::unit; };
template <typename T, auto U>
concept has_unit_of = has_unit<T> && std::remove_cvref_t<T>::unit == U;
template <typename Q, auto R>
concept is_quantity =
    mp_units::Reference<decltype(R)> &&
    mp_units::QuantityOf<std::remove_cvref_t<Q>, get_quantity_spec(R)> &&
        has_unit_of<Q, get_unit(R)>;;
template <typename QP, auto R>
concept is_quantity_point =
    mp_units::Reference<decltype(R)> &&
    mp_units::QuantityPointOf<std::remove_cvref_t<QP>, get_quantity_spec(R)> &&
    has_unit_of<QP, get_unit(R)>;

#define ASP_NO_CONST(X) decltype(X){}

template <typename T, auto... Constraints>
concept satisfy_all = (std::invocable<decltype(Constraints), T> && ...);

template <typename T, auto... Constraints>
concept is_quantity_point_satisfying = mp_units::QuantityPoint<std::remove_cvref_t<T>> && satisfy_all<T, Constraints...>;

template <typename T, auto... Constraints>
concept is_quantity_satisfying = mp_units::Quantity<std::remove_cvref_t<T>> && satisfy_all<T, Constraints...>;

template <typename T, auto... Constraints>
concept is_rectangle_satisfying = bounding_box<std::remove_cvref_t<T>> && requires(T&& t) {
  { call::l_x(t) } -> is_quantity_point_satisfying<Constraints...>;
  { call::t_y(t) } -> is_quantity_point_satisfying<Constraints...>;
  { call::width(t) } -> is_quantity_satisfying<Constraints...>;
  { call::height(t) } -> is_quantity_satisfying<Constraints...>;
};

template <typename T, auto R>
concept rectangle_with_unit = bounding_box<std::remove_cvref_t<T>>
                              && is_rectangle_satisfying<T, [] (has_unit_of<get_unit(pixel)> auto) {}>
                              && requires(T &&t) {
  { call::l_x(t) } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::width[R])>;
  { call::t_y(t) } -> is_quantity_point<ASP_NO_CONST(mp_units::isq::height[R])>;
  { call::width(t) } -> is_quantity<ASP_NO_CONST(mp_units::isq::width[R])>;
  { call::height(t) } -> is_quantity<ASP_NO_CONST(mp_units::isq::height[R])>;
};

static_assert(is_quantity_point<mp_units::quantity_point<decltype(mp_units::isq::width[pixel]){}>
    , decltype(mp_units::isq::width[pixel]){}>);
static_assert(rectangle_with_unit<basic_rectangle<pixel, int>, pixel>);

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
concept is_renderer = requires(T &&t, basic_rectangle<pixel, std::int_least32_t> const &r,
                               default_colour_t const &c, basic_rectangle<point, std::int_least32_t> const& point_box) {
  { call::fill(t, r, c) } -> fill_rect_command;
  { call::to_pixel(t, point_box) } -> is_box_satisfying<[] (is_integer auto) {}, [] (is_unit<pixel> auto) {}>;
};

template <typename T>
concept is_render_context = requires(T &&t) {
  { t.renderer() } -> is_renderer;
  { t.pixel_area() } -> rectangle_with_unit<pixel>;
};

constexpr is_render_command auto
fill(auto &&r, rectangle_with_unit<point> auto const &area,
     colour auto const &c) requires(requires() { call::fill(std::forward<decltype(r)>(r),
      call::to_pixel(r, area), c); }) {
  return call::fill(std::forward<decltype(r)>(r),
                    linear_map(area, r.pixel_to_point_ratio()), c);
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
    constexpr executor(mp_units::quantity<pixel_width, int> width, mp_units::quantity<pixel_height, int> height)
     : columns_(width.numerical_value_in(pixel)), rows_(height.numerical_value_in(pixel)), raw_results_(calc_size()) {}

    
    constexpr default_colour_t const& operator[](is_quantity<pixel> auto const& x, is_quantity<pixel> auto const& y) const& noexcept {
      auto i = columns_ * y.numerical_value_in(pixel) + x.numerical_value_in(pixel);
      return raw_results_.at(i);
    }
    constexpr default_colour_t& operator[](is_quantity<pixel> auto const& x, is_quantity<pixel> auto const& y) &noexcept {
      auto i = columns_ * y.numerical_value_in(pixel) + x.numerical_value_in(pixel);
      return raw_results_.at(i);
    }
    constexpr std::size_t size() const noexcept { return raw_results_.size(); }
    constexpr std::size_t extend(std::size_t e) const noexcept {
      switch(e) {
        case 0 : return columns_;
        case 1 : return rows_;
        default: return 0u;
      }
    }
  };
  struct cached_fill_rect {
    using associated_renderer = stub_renderer;
    basic_rectangle<pixel, std::int_least32_t> area;
    default_colour_t c;

    constexpr basic_rectangle<pixel, std::int_least32_t> pixel_area() const noexcept {
      return area;
    }
    constexpr default_colour_t colour() const noexcept { return c; }
  };

  static constexpr cached_fill_rect fill(basic_rectangle<pixel, std::int_least32_t> const &area,
                                         default_colour_t colour) {
    return {area, colour};
  }
  constexpr mp_units::Quantity auto pixel_to_point_ratio() const {
    return px_p_pt;
  }
  constexpr executor executing_renderer(basic_rectangle<pixel, int> const& r) const {
    return {call::width(r), call::height(r)};
  }
  constexpr executor executing_renderer() const {
    return executing_renderer({{}, {}, 1 * pixel_width, 1 * pixel_height});
  }
};

constexpr void execute(stub_renderer::cached_fill_rect const &cmd,
                               stub_renderer::executor &r) {
                                auto a = cmd.pixel_area();
  
  for(auto y : std::views::iota(call::t_y(a).quantity_from_zero().numerical_value_in(pixel), call::b_y(a).quantity_from_zero().numerical_value_in(pixel))) {
    for(auto x : std::views::iota(call::l_x(a).quantity_from_zero().numerical_value_in(pixel), call::r_x(a).quantity_from_zero().numerical_value_in(pixel))) {
      r[x * pixel_width, y * pixel_height] = cmd.colour();
    }
  }
}

static_assert(is_renderer<stub_renderer>);

constexpr rectangle_with_unit<pixel> auto
to_pixel_rectangle(stub_renderer const &,
                   rectangle_with_unit<point> auto const &pnt_rect) {
  return linear_map(pnt_rect, 1 * pixel_per_point);
}

template <is_renderer Renderer, typename Area> class simple_display_context {
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
  constexpr explicit fill_rectangle(C colour) : c_(colour) {}
  friend constexpr fill_rect_command auto
  initial_render_cache(fill_rectangle const &fr, is_render_context auto &&ctx) {
    return ctx.renderer().fill(ctx.pixel_area(), fr.c_);
  }
  template <fill_rect_command CMD>
  constexpr std::remove_cvref_t<CMD> render(is_render_context auto &&ctx,
                                            CMD &&c) {
    if (c.pixel_area() != ctx.pixel_area()) {
      return initial_render_cache(*this, ctx);
    }
    return std::forward<CMD>(c);
  }
};

template <typename T>
concept is_widget = true;

template <is_widget, is_widget> class background_t {
public:
  struct render_cache_t {
    constexpr auto execute(auto &&) {}
  };
  constexpr background_t(auto &&, auto &&) {}

  friend constexpr render_cache_t
  initial_render_cache(background_t const &, is_render_context auto &&) {
    return {};
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

TEST(StubRenderer, CreateRendererWithSize2x1) // NOLINT
{
  auto r = stub_renderer{};
  auto exe = r.executing_renderer(basic_rectangle<pixel, int>({}, {}, 2 * pixel_width, 1 * pixel_height));
  EXPECT_THAT(exe.extend(0), Eq(2));
}

TEST(StubRenderer, FillRectApplyToSinglePixel) // NOLINT
{
  auto r = stub_renderer{};
  //r.set_area({{}, {}, 1 * pixel_width, 1 * pixel_height});
  auto rect = basic_rectangle<point, int>({}, {}, 1 * point_width, 1 * point_height);
  auto fr = call::fill(r, rect, default_colour_t{1, 2, 3, 255});
  // NOTE: THIS EXECUTE MUST BE ALTERED, THE RENDERER MAY NEED TO DO STUFF
  // BEFORE AND AFTER THE ACTUAL RENDERING.
  auto exe = r.executing_renderer(basic_rectangle<pixel, int>({}, {}, 1 * pixel_width, 1 * pixel_height));
  auto pixels = call::execute(fr, exe);
  EXPECT_THAT(exe.size(), Eq(1));
  EXPECT_THAT((exe[0, 0]), Eq(default_colour_t{1, 2, 3, 255}));
}

TEST(FillRect, InitialRenderCacheFillsWithCorrectColour) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(255, 0, 0, 127));
  auto renderer = stub_renderer{};
  auto rect = basic_rectangle<point, float>({}, {}, 1 * point_width, 1 * point_height);
  auto cache = initial_render_cache(fr, simple_display_context(renderer, rect));
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
  auto cache = initial_render_cache(fr, simple_display_context(renderer, rect));
  EXPECT_THAT(cache.area.l_x(), Eq(mp_units::absolute<pixel_width>(1)));
  EXPECT_THAT(cache.area.t_y(), Eq(mp_units::absolute<pixel_height>(5)));
  EXPECT_THAT(cache.area.width(), Eq(2 * pixel_width));
  EXPECT_THAT(cache.area.height(), Eq(3 * pixel_height));
}

TEST(FillRect, NewRenderCacheIsUpdatedWhenSizeChanged) // NOLINT
{
  auto fr = fill_rectangle(default_colour_t(0, 255, 0, 127));
  auto renderer = stub_renderer{};
  auto init_rect = basic_rectangle<point, int>(mp_units::absolute<point_width>(1),
                                          mp_units::absolute<point_height>(5),
                                          2 * point_width, 3 * point_height);
  auto cache =
      initial_render_cache(fr, simple_display_context(renderer, init_rect));
  auto new_rect = basic_rectangle<point, int>(mp_units::absolute<point_width>(3),
                                         mp_units::absolute<point_height>(2),
                                         3 * point_width, 7 * point_height);
  cache = call::render(fr, simple_display_context(renderer, new_rect),
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
  auto cache = initial_render_cache(fr, simple_display_context(renderer, rect));
  fr = fill_rectangle(default_colour_t(0, 0, 255, 127));
  cache = call::render(fr, simple_display_context(renderer, rect),
                       std::move(cache));
  EXPECT_THAT(cache.colour().red, Eq(0));
  EXPECT_THAT(cache.colour().blue, Eq(0));
  EXPECT_THAT(cache.colour().green, Eq(255));
  EXPECT_THAT(cache.colour().alpha, Eq(127));
}
TEST(FillRectBackground, DISABED_FillRectAndFillRect) // NOLINT
{
  auto c = fill_rectangle(default_colour_t(255, 0, 0, 255)) |
           background(fill_rectangle(default_colour_t{0, 255, 0, 255}));
  auto renderer = stub_renderer{};
  auto rect = basic_rectangle<point, int>(mp_units::absolute<point_width>(1),
                                     mp_units::absolute<point_height>(5),
                                     2 * point_width, 3 * point_height);
  auto cache = initial_render_cache(c, simple_display_context(renderer, rect));
  call::execute(cache, renderer);
  FAIL()
      << "Not yet implemented. Must test that the foreground takes 'priority'";
}
} // namespace asp::tests
